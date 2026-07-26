#!/usr/bin/env python3
"""Build minimal Type0 (CID) test PDFs with real embedded subset fonts.
Produces controlled oracles for folio M3."""
import sys, io, struct
from fontTools.ttLib import TTFont
from fontTools.subset import Subsetter, Options

def subset_font(path, text, cff=False):
    opts = Options()
    opts.set(notdef_outline=True, recalc_bounds=True, glyph_names=True)
    opts.name_IDs = ['*']; opts.name_legacy = True; opts.name_languages = ['*']
    f = TTFont(path)
    ss = Subsetter(options=opts)
    ss.populate(unicodes=[ord(c) for c in set(text)])
    ss.subset(f)
    buf = io.BytesIO(); f.save(buf); f2 = TTFont(io.BytesIO(buf.getvalue()))
    return f2, buf.getvalue()

def obj(n, body): return (n, body)

class PDF:
    def __init__(self): self.objs=[]; self.n=0
    def add(self, body):
        self.n+=1; self.objs.append(body); return self.n
    def build(self, root):
        out=b"%PDF-1.7\n%\xe2\xe3\xcf\xd3\n"
        offs=[0]*(len(self.objs)+1)
        for i,body in enumerate(self.objs,1):
            offs[i]=len(out)
            if isinstance(body,str): body=body.encode('latin-1')
            out+=b"%d 0 obj\n"%i + body + b"\nendobj\n"
        xref=len(out)
        out+=b"xref\n0 %d\n"%(self.n+1)
        out+=b"0000000000 65535 f \n"
        for i in range(1,self.n+1):
            out+=b"%010d 00000 n \n"%offs[i]
        out+=b"trailer\n<< /Size %d /Root %d 0 R >>\nstartxref\n%d\n%%%%EOF\n"%(self.n+1,root,xref)
        return out

def stream_obj(dict_str, data):
    return dict_str.encode('latin-1')+b" stream\n"+data+b"\nendstream"

def build_cid(path_out, fontpath, text, cff=False, cidtype0=False):
    f2, fbytes = subset_font(fontpath, text)
    cmap = f2.getBestCmap()
    glyphorder = f2.getGlyphOrder()
    gid_of = {g:i for i,g in enumerate(glyphorder)}
    hmtx = f2['hmtx']
    upem = f2['head'].unitsPerEm
    # char -> gid
    def cgid(ch):
        gn = cmap.get(ord(ch))
        return gid_of.get(gn,0) if gn else 0
    codes=[]
    for ch in text:
        codes.append(cgid(ch))
    # content: encode each code as 2-byte gid (Identity-H, CIDToGIDMap Identity => GID=CID=code)
    hexstr=''.join('%04X'%c for c in codes)
    # widths in 1000/em
    scale=1000.0/upem
    W=[]
    seen=set()
    for c in codes:
        if c in seen: continue
        seen.add(c)
        gn=glyphorder[c]
        adv=hmtx[gn][0]
        W.append((c, round(adv*scale)))
    W.sort()
    warr='['+' '.join('%d[%d]'%(c,w) for c,w in W)+']'
    content=("BT /F1 24 Tf 72 700 Td <%s> Tj ET\n"%hexstr).encode('latin-1')

    p=PDF()
    # font file
    if cff:
        # extract bare CFF table
        raw=f2['CFF '].compile(f2)
        ffkey="FontFile3"; ffsub="/Subtype /%s"%("CIDFontType0C")
        ff=p.add(stream_obj("<< /Length %d %s >>"%(len(raw),ffsub), raw))
        cidsub="CIDFontType0"
    else:
        raw=fbytes
        ff=p.add(stream_obj("<< /Length %d /Length1 %d >>"%(len(raw),len(raw)), raw))
        cidsub="CIDFontType2"
    fd=p.add("<< /Type /FontDescriptor /FontName /SUBSET+Test /Flags 4 "
             "/FontBBox [0 -200 1000 900] /ItalicAngle 0 /Ascent 800 /Descent -200 "
             "/CapHeight 700 /StemV 80 /%s %d 0 R >>"%(ffkey if cff else "FontFile2", ff))
    ci = "<< /Type /Font /Subtype /%s /BaseFont /SUBSET+Test "\
         "/CIDSystemInfo << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> "\
         "/FontDescriptor %d 0 R /CIDToGIDMap /Identity /DW 500 /W %s >>"%(cidsub,fd,warr)
    cid=p.add(ci)
    t0=p.add("<< /Type /Font /Subtype /Type0 /BaseFont /SUBSET+Test "
             "/Encoding /Identity-H /DescendantFonts [%d 0 R] >>"%cid)
    cs=p.add(stream_obj("<< /Length %d >>"%len(content), content))
    pg=p.add("<< /Type /Page /Parent 0 0 R /MediaBox [0 0 612 792] "
             "/Resources << /Font << /F1 %d 0 R >> >> /Contents %d 0 R >>"%(t0,cs))
    pages=p.add("<< /Type /Pages /Kids [%d 0 R] /Count 1 >>"%pg)
    # fix parent ref
    p.objs[pg-1]=p.objs[pg-1].replace("/Parent 0 0 R","/Parent %d 0 R"%pages)
    root=p.add("<< /Type /Catalog /Pages %d 0 R >>"%pages)
    data=p.build(root)
    open(path_out,'wb').write(data)
    print("wrote",path_out,"codes=",codes,"text=",repr(text))

def build_simple_cff(path_out, fontpath, text):
    f2, fbytes = subset_font(fontpath, text)
    cmap=f2.getBestCmap(); go=f2.getGlyphOrder(); gid_of={g:i for i,g in enumerate(go)}
    hmtx=f2['hmtx']; upem=f2['head'].unitsPerEm; scale=1000.0/upem
    raw=f2['CFF '].compile(f2)
    # simple font: single-byte codes = ascii; WinAnsi encoding
    codes=[ord(c) for c in text]
    first=min(codes); last=max(codes)
    widths=[]
    for c in range(first,last+1):
        ch=chr(c); gn=cmap.get(c)
        adv=hmtx[gn][0] if gn else 0
        widths.append(round(adv*scale))
    warr='['+' '.join(str(w) for w in widths)+']'
    content=("BT /F1 24 Tf 72 700 Td (%s) Tj ET\n"%text).encode('latin-1')
    p=PDF()
    ff=p.add(stream_obj("<< /Length %d /Subtype /Type1C >>"%len(raw), raw))
    fd=p.add("<< /Type /FontDescriptor /FontName /SUBSET+Test /Flags 32 "
             "/FontBBox [0 -200 1000 900] /ItalicAngle 0 /Ascent 800 /Descent -200 "
             "/CapHeight 700 /StemV 80 /FontFile3 %d 0 R >>"%ff)
    fnt=p.add("<< /Type /Font /Subtype /Type1 /BaseFont /SUBSET+Test "
              "/FirstChar %d /LastChar %d /Widths %s /Encoding /WinAnsiEncoding "
              "/FontDescriptor %d 0 R >>"%(first,last,warr,fd))
    cs=p.add(stream_obj("<< /Length %d >>"%len(content), content))
    pg=p.add("<< /Type /Page /Parent 0 0 R /MediaBox [0 0 612 792] "
             "/Resources << /Font << /F1 %d 0 R >> >> /Contents %d 0 R >>"%(fnt,cs))
    pages=p.add("<< /Type /Pages /Kids [%d 0 R] /Count 1 >>"%pg)
    p.objs[pg-1]=p.objs[pg-1].replace("/Parent 0 0 R","/Parent %d 0 R"%pages)
    root=p.add("<< /Type /Catalog /Pages %d 0 R >>"%pages)
    open(path_out,'wb').write(p.build(root))
    print("wrote",path_out,"simple-cff text=",repr(text))

if __name__=="__main__":
    DV="/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
    NR="/usr/share/fonts/opentype/urw-base35/NimbusRoman-Regular.otf"
    txt="Hello CID World 123"
    build_cid("/home/claude/folio/test/real-cid-tt.pdf", DV, txt, cff=False)
    build_cid("/home/claude/folio/test/real-cid-cff.pdf", NR, "Nimbus CFF CID 42", cff=True)
    build_simple_cff("/home/claude/folio/test/real-simple-cff.pdf", NR, "Simple CFF abc")
