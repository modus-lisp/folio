import io, os
from fontTools.ttLib import TTFont
from fontTools.subset import Subsetter, Options
HERE = os.path.dirname(os.path.abspath(__file__))
# A CID-keyed CFF font (Source Han Serif or similar); override via $SOURCEHAN.
SRC = os.environ.get("SOURCEHAN", "SourceHanSerif.otf")
text="Aα漢字BC"   # latin + greek + kanji
opts=Options(); opts.set(notdef_outline=True, glyph_names=True)
f=TTFont(SRC)
ss=Subsetter(options=opts); ss.populate(unicodes=[ord(c) for c in set(text)]); ss.subset(f)
buf=io.BytesIO(); f.save(buf); f2=TTFont(io.BytesIO(buf.getvalue()))
cff=f2['CFF '].cff; td=cff[cff.fontNames[0]]
print("subset CID-keyed:", hasattr(td,'ROS'), "nglyphs:", len(f2.getGlyphOrder()))
cmap=f2.getBestCmap(); go=f2.getGlyphOrder(); gid_of={g:i for i,g in enumerate(go)}
hmtx=f2['hmtx']; upem=f2['head'].unitsPerEm; scale=1000.0/upem
# glyph name -> CID: for CID fonts fonttools names glyphs 'cidXXXXX'
def name_cid(gn):
    if gn.startswith('cid'): return int(gn[3:])
    if gn=='.notdef': return 0
    return None
codes=[]; W={}
for ch in text:
    gn=cmap.get(ord(ch))
    if not gn: codes.append(0); continue
    cid=name_cid(gn)
    codes.append(cid if cid is not None else gid_of[gn])
    adv=hmtx[gn][0]; W[codes[-1]]=round(adv*scale)
raw=f2['CFF '].compile(f2)
hexstr=''.join('%04X'%c for c in codes)
warr='['+' '.join('%d[%d]'%(c,w) for c,w in sorted(W.items()))+']'
content=("BT /F1 24 Tf 72 700 Td <%s> Tj ET\n"%hexstr).encode()
# build PDF
objs=[]
def add(b): objs.append(b); return len(objs)
def strm(d,data): return d.encode('latin-1')+b" stream\n"+data+b"\nendstream"
ff=add(strm("<< /Length %d /Subtype /CIDFontType0C >>"%len(raw), raw))
fd=add("<< /Type /FontDescriptor /FontName /SUBSET+Han /Flags 4 /FontBBox [0 -200 1000 900] "
       "/ItalicAngle 0 /Ascent 880 /Descent -120 /CapHeight 700 /StemV 80 /FontFile3 %d 0 R >>"%ff)
ci=add("<< /Type /Font /Subtype /CIDFontType0 /BaseFont /SUBSET+Han "
       "/CIDSystemInfo << /Registry (Adobe) /Ordering (Japan1) /Supplement 6 >> "
       "/FontDescriptor %d 0 R /DW 1000 /W %s >>"%(fd,warr))
t0=add("<< /Type /Font /Subtype /Type0 /BaseFont /SUBSET+Han /Encoding /Identity-H /DescendantFonts [%d 0 R] >>"%ci)
cs=add(strm("<< /Length %d >>"%len(content), content))
pg=add("<< /Type /Page /Parent 99 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 %d 0 R >> >> /Contents %d 0 R >>"%(t0,cs))
pages=add("<< /Type /Pages /Kids [%d 0 R] /Count 1 >>"%pg)
objs[pg-1]=objs[pg-1].replace("/Parent 99 0 R","/Parent %d 0 R"%pages)
root=add("<< /Type /Catalog /Pages %d 0 R >>"%pages)
out=b"%PDF-1.7\n%\xe2\xe3\xcf\xd3\n"; offs=[0]*(len(objs)+1)
for i,b in enumerate(objs,1):
    offs[i]=len(out); 
    if isinstance(b,str): b=b.encode('latin-1')
    out+=b"%d 0 obj\n"%i+b+b"\nendobj\n"
x=len(out); out+=b"xref\n0 %d\n0000000000 65535 f \n"%(len(objs)+1)
for i in range(1,len(objs)+1): out+=b"%010d 00000 n \n"%offs[i]
out+=b"trailer\n<< /Size %d /Root %d 0 R >>\nstartxref\n%d\n%%%%EOF\n"%(len(objs)+1,root,x)
open(os.path.join(HERE, "real-cid-keyed.pdf"),"wb").write(out)
print("wrote real-cid-keyed.pdf codes=",codes)
