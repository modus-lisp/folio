#!/usr/bin/env python3
"""Generate type1-embed.pdf: a minimal PDF embedding an Adobe Type 1 (FontFile,
eexec) font program and showing a known string (incl. an accented char that
exercises seac and braces).  Both folio and pdfium render its OWN outlines, so
exact-Type1 support lets folio pixel-match the oracle."""
import struct, zlib, os
from fontTools import t1Lib
from fontTools.afmLib import AFM

PFB = "/usr/share/fonts/X11/Type1/NimbusRoman-Regular.pfb"

def read_pfb_segments(path):
    data = open(path,"rb").read()
    segs=[]; p=0
    while p < len(data) and data[p]==0x80:
        t=data[p+1]
        if t==3: break
        ln=struct.unpack("<I",data[p+2:p+6])[0]
        p+=6
        segs.append((t,data[p:p+ln])); p+=ln
    return segs

segs=read_pfb_segments(PFB)
ascii_parts=b"".join(s[1] for s in segs if s[0]==1)
# split ascii into header (before trailer of zeros) — the trailer is the LAST ascii seg
# reconstruct: clear = first ascii seg, binary = binary seg(s), trailer = last ascii seg
clear=segs[0][1]
binary=b"".join(s[1] for s in segs if s[0]==2)
trailer=b"".join(s[1] for i,s in enumerate(segs) if s[0]==1 and i>0)
fontfile = clear + binary + trailer
L1,L2,L3=len(clear),len(binary),len(trailer)

# widths: use the AFM if present else a flat 500
widths={}
try:
    afmp=PFB.replace(".pfb",".afm")
    if os.path.exists(afmp):
        afm=AFM(afmp)
except Exception:
    afm=None

# encoding: WinAnsi with a couple of Differences; string uses é(233) { } and digits
text = "Type1 outlines: {a,b,c}@x.org é"  # eacute via combining? no—use code
# We'll show ASCII plus eacute at code 233 (WinAnsi).
show = b"Type 1 exact: {jacob,ming}@ex.org  caf\xe9 123"

objs=[]
def obj(s):
    objs.append(s); return len(objs)

# We build the PDF with a fixed object layout.
pdf=b"%PDF-1.4\n"
offsets=[0]*10
def emit(n, body):
    global pdf
    offsets[n]=len(pdf)
    pdf += (str(n).encode()+b" 0 obj\n"+body+b"\nendobj\n")

# 1 catalog 2 pages 3 page 4 content 5 font 6 descriptor 7 fontfile
emit(1,b"<< /Type /Catalog /Pages 2 0 R >>")
emit(2,b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
emit(3,b"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 400 120] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>")
content=b"BT /F1 22 Tf 20 60 Td (" + show.replace(b"(",b"\\(").replace(b")",b"\\)") + b") Tj ET"
emit(4,b"<< /Length "+str(len(content)).encode()+b" >>\nstream\n"+content+b"\nendstream")
emit(5,b"<< /Type /Font /Subtype /Type1 /BaseFont /NimbusRoman-Regular /FirstChar 32 /LastChar 255 /Widths ["+b" ".join(b"500" for _ in range(224))+b"] /FontDescriptor 6 0 R /Encoding /WinAnsiEncoding >>")
emit(6,b"<< /Type /FontDescriptor /FontName /NimbusRoman-Regular /Flags 34 /FontBBox [-168 -281 1031 924] /ItalicAngle 0 /Ascent 683 /Descent -217 /CapHeight 662 /StemV 84 /FontFile 7 0 R >>")
emit(7,b"<< /Length "+str(len(fontfile)).encode()+b" /Length1 "+str(L1).encode()+b" /Length2 "+str(L2).encode()+b" /Length3 "+str(L3).encode()+b" >>\nstream\n"+fontfile+b"\nendstream")

xref_off=len(pdf)
pdf+=b"xref\n0 8\n0000000000 65535 f \n"
for n in range(1,8):
    pdf+=(("%010d 00000 n \n"%offsets[n]).encode())
pdf+=b"trailer\n<< /Size 8 /Root 1 0 R >>\nstartxref\n"+str(xref_off).encode()+b"\n%%EOF"
open("type1-embed.pdf","wb").write(pdf)
print("wrote type1-embed.pdf  L1=%d L2=%d L3=%d total=%d"%(L1,L2,L3,len(fontfile)))
