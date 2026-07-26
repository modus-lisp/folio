#!/usr/bin/env python3
"""Author embedded-CJK test PDFs that exercise a NAMED predefined CMap encoding.

Shape (the common distributed-CJK-doc form): a Type0 font whose /Encoding is a
named Adobe CMap (not Identity-H), with an embedded CIDFontType2 (TrueType) whose
/CIDToGIDMap translates the CMap's registry CIDs to the font's own GIDs.  folio
must: segment the text bytes via the CMap codespace, map each code -> CID via the
named CMap, then CID -> GID via /CIDToGIDMap, then draw the TrueType glyph.

Two fixtures:
  cjk-cmap-gb.pdf    Chinese, /Encoding /UniGB-UCS2-H  (Adobe-GB1, 2-byte UCS2)
  cjk-cmap-rksj.pdf  Japanese, /Encoding /90ms-RKSJ-H  (Adobe-Japan1, mixed 1/2-byte Shift-JIS)
"""
import io, os, re, zlib, struct
from fontTools.ttLib import TTFont
from fontTools.subset import Subsetter, Options

FONT = "/usr/share/fonts/truetype/droid/DroidSansFallbackFull.ttf"
CMAPDIR = "/usr/share/poppler/cMap"
OUT = os.path.dirname(__file__)

hexre = re.compile(r"<([0-9A-Fa-f]+)>")

def load_cmap(regpath):
    """Parse an Adobe CMap text file into a code->CID lookup dict (cidrange+cidchar)."""
    ranges = []; chars = {}
    lines = open(regpath, encoding="latin-1").read().split("\n")
    i = 0; n = len(lines)
    while i < n:
        s = lines[i]
        if "begincidrange" in s:
            i += 1
            while i < n and "endcidrange" not in lines[i]:
                hs = hexre.findall(lines[i]); parts = lines[i].split()
                if len(hs) >= 2 and parts:
                    ranges.append((int(hs[0],16), int(hs[1],16), int(parts[-1])))
                i += 1
        elif "begincidchar" in s:
            i += 1
            while i < n and "endcidchar" not in lines[i]:
                hs = hexre.findall(lines[i]); parts = lines[i].split()
                if len(hs) >= 1 and parts:
                    chars[int(hs[0],16)] = int(parts[-1])
                i += 1
        i += 1
    def lookup(code):
        if code in chars: return chars[code]
        for lo,hi,cid in ranges:
            if lo <= code <= hi: return cid + (code - lo)
        return 0
    return lookup

def build(out_name, text, enc_name, cmap_file, registry, ordering, supp, codes_bytes):
    """codes_bytes(ch) -> the byte string this char occupies under the CMap encoding."""
    cidlookup = load_cmap(os.path.join(CMAPDIR, cmap_file))
    # subset the font to our chars; CIDToGIDMap is built afterwards from the
    # subset font's own cmap, so the compacted GIDs are what we reference.
    opts = Options(); opts.set(notdef_outline=True)
    f = TTFont(FONT)
    ss = Subsetter(options=opts)
    ss.populate(unicodes=[ord(c) for c in set(text)])
    ss.subset(f)
    buf = io.BytesIO(); f.save(buf); sub = buf.getvalue()
    f2 = TTFont(io.BytesIO(sub))
    cmap = f2.getBestCmap(); go = f2.getGlyphOrder()
    gid_of = {g:i for i,g in enumerate(go)}
    upem = f2['head'].unitsPerEm; hmtx = f2['hmtx']; scale = 1000.0/upem
    # per-char: code -> CID (named CMap), CID -> GID (font)
    content = bytearray(); c2g = {}; W = {}
    for ch in text:
        cb = codes_bytes(ch)
        code = int.from_bytes(cb, 'big')
        cid = cidlookup(code)
        gn = cmap.get(ord(ch)); gid = gid_of.get(gn, 0)
        c2g[cid] = gid
        W[cid] = round(hmtx[gn][0]*scale) if gn else 1000
        content += cb
    maxcid = max(c2g) if c2g else 0
    c2g_arr = bytearray((maxcid+1)*2)
    for cid,gid in c2g.items():
        struct.pack_into('>H', c2g_arr, cid*2, gid)
    c2g_z = zlib.compress(bytes(c2g_arr), 9)
    warr = '['+' '.join('%d[%d]'%(c,w) for c,w in sorted(W.items()))+']'
    text_show = ("BT /F1 28 Tf 60 720 Td <%s> Tj ET\n" %
                 content.hex().upper()).encode()

    objs = []
    def add(b): objs.append(b); return len(objs)
    def strm(d, data): return d.encode('latin-1')+b" stream\n"+data+b"\nendstream"
    ff = add(strm("<< /Length %d >>" % len(sub), sub))
    fd = add("<< /Type /FontDescriptor /FontName /SUBSET+Droid /Flags 4 "
             "/FontBBox [-100 -300 1100 900] /ItalicAngle 0 /Ascent 880 /Descent -120 "
             "/CapHeight 700 /StemV 80 /FontFile2 %d 0 R >>" % ff)
    c2gobj = add(strm("<< /Length %d /Filter /FlateDecode >>" % len(c2g_z), c2g_z))
    ci = add("<< /Type /Font /Subtype /CIDFontType2 /BaseFont /SUBSET+Droid "
             "/CIDSystemInfo << /Registry (%s) /Ordering (%s) /Supplement %d >> "
             "/FontDescriptor %d 0 R /CIDToGIDMap %d 0 R /DW 1000 /W %s >>"
             % (registry, ordering, supp, fd, c2gobj, warr))
    t0 = add("<< /Type /Font /Subtype /Type0 /BaseFont /SUBSET+Droid /Encoding /%s "
             "/DescendantFonts [%d 0 R] >>" % (enc_name, ci))
    cs = add(strm("<< /Length %d >>" % len(text_show), text_show))
    pg = add("<< /Type /Page /Parent 99 0 R /MediaBox [0 0 612 792] "
             "/Resources << /Font << /F1 %d 0 R >> >> /Contents %d 0 R >>" % (t0, cs))
    pages = add("<< /Type /Pages /Kids [%d 0 R] /Count 1 >>" % pg)
    objs[pg-1] = objs[pg-1].replace("/Parent 99 0 R", "/Parent %d 0 R" % pages)
    root = add("<< /Type /Catalog /Pages %d 0 R >>" % pages)
    out = b"%PDF-1.7\n%\xe2\xe3\xcf\xd3\n"; offs=[0]*(len(objs)+1)
    for i,b in enumerate(objs,1):
        offs[i]=len(out)
        if isinstance(b,str): b=b.encode('latin-1')
        out += b"%d 0 obj\n"%i + b + b"\nendobj\n"
    x=len(out); out += b"xref\n0 %d\n0000000000 65535 f \n"%(len(objs)+1)
    for i in range(1,len(objs)+1): out += b"%010d 00000 n \n"%offs[i]
    out += b"trailer\n<< /Size %d /Root %d 0 R >>\nstartxref\n%d\n%%%%EOF\n"%(len(objs)+1,root,x)
    path = os.path.join(OUT, out_name)
    open(path,"wb").write(out)
    print("wrote %s  chars=%r  enc=%s  size=%d  maxcid=%d"
          % (out_name, text, enc_name, len(out), maxcid))

if __name__ == "__main__":
    # Chinese via UniGB-UCS2-H (2-byte UCS2)
    build("cjk-cmap-gb.pdf", "汉字测试",  # 汉字测试
          "UniGB-UCS2-H", "Adobe-GB1/UniGB-UCS2-H", "Adobe", "GB1", 5,
          lambda ch: ch.encode("utf-16-be"))
    # Japanese via 90ms-RKSJ-H (mixed 1/2-byte Shift-JIS)
    build("cjk-cmap-rksj.pdf", "日本語",       # 日本語
          "90ms-RKSJ-H", "Adobe-Japan1/90ms-RKSJ-H", "Adobe", "Japan1", 6,
          lambda ch: ch.encode("shift_jis"))
