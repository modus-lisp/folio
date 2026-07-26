#!/usr/bin/env python3
"""Hand-author Type3 font test PDFs (ISO 32000-1 §9.6.5).

A Type3 font's glyphs are CharProc content streams painted in glyph space and
transformed by /FontMatrix.  We build a font whose glyphs are simple VECTOR
shapes drawn with d1 (uncoloured — painted in the text fill colour) plus a d0
(coloured — draws its own colours) glyph and a d1 BITMAP glyph (an image mask,
the classic dvips bitmap-font shape).  The page shows the d1 glyphs in TWO fill
colours to prove d1 recolouring, then the d0 glyph, then the bitmap glyph.
"""
import struct, zlib, sys

def obj(n, body):
    return (n, body)

# --- CharProc streams (glyph space, FontMatrix 0.001 -> 1000 units/em) --------
# 'A' (65): d1 filled triangle, advance 750
procA = b"""750 0 0 0 700 700 d1
0 0 m 700 0 l 350 700 l f
"""
# 'B' (66): d1 two vertical bars
procB = b"""750 0 0 0 700 700 d1
0 0 150 700 re 400 0 150 700 re f
"""
# 'C' (67): d0 COLOURED glyph — draws its own red + blue bars (ignores text colour)
procC = b"""750 0 d0
1 0 0 rg 0 0 300 700 re f
0 0 1 rg 400 0 300 700 re f
"""
# 'D' (68): d1 BITMAP glyph — an 8x8 image mask scaled to 700x700 (dvips style),
# painted in the current fill colour.  /D [1 0] so sample bit 1 paints the shape.
# 8x8 "X" pattern, one byte per row.
xbits = bytes([0x81,0x42,0x24,0x18,0x18,0x24,0x42,0x81])
hexbits = xbits.hex().upper().encode()
procD = (b"750 0 0 0 700 700 d1\n"
         b"q 700 0 0 700 0 0 cm\n"
         b"BI /W 8 /H 8 /IM true /D [1 0] /F /AHx ID\n"
         + hexbits + b">\nEI\nQ\n")

# --- page content: d1 in red then blue (recolour proof), d0, bitmap -----------
content = b"""BT
/T3 40 Tf
1 0 0 rg
72 340 Td
(AB) Tj
0 0 1 rg
0 -60 Td
(AB) Tj
0 0 0 rg
0 -60 Td
(C) Tj
0 -60 Td
(D) Tj
ET
"""

def build():
    objs = {}
    # 1 catalog, 2 pages, 3 page, 4 content, 5 font, 6 charprocs dict,
    # 7-10 procA..procD, 11 encoding
    objs[1] = b"<< /Type /Catalog /Pages 2 0 R >>"
    objs[2] = b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
    objs[3] = (b"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 300 400] "
               b"/Resources << /Font << /T3 5 0 R >> >> /Contents 4 0 R >>")
    objs[4] = b"<< /Length %d >>\nstream\n%s\nendstream" % (len(content), content)
    objs[5] = (b"<< /Type /Font /Subtype /Type3 "
               b"/FontBBox [0 0 750 700] "
               b"/FontMatrix [0.001 0 0 0.001 0 0] "
               b"/CharProcs 6 0 R "
               b"/Encoding 11 0 R "
               b"/FirstChar 65 /LastChar 68 "
               b"/Widths [750 750 750 750] "
               b"/Resources << >> >>")
    objs[6] = b"<< /A 7 0 R /B 8 0 R /C 9 0 R /D 10 0 R >>"
    for n, proc in ((7, procA), (8, procB), (9, procC), (10, procD)):
        objs[n] = b"<< /Length %d >>\nstream\n%s\nendstream" % (len(proc), proc)
    objs[11] = (b"<< /Type /Encoding /Differences "
                b"[65 /A /B /C /D] >>")

    # serialize with xref
    out = b"%PDF-1.4\n%\xe2\xe3\xcf\xd3\n"
    offsets = {}
    for n in sorted(objs):
        offsets[n] = len(out)
        out += b"%d 0 obj\n" % n + objs[n] + b"\nendobj\n"
    xref_pos = len(out)
    nobj = max(objs) + 1
    out += b"xref\n0 %d\n" % nobj
    out += b"0000000000 65535 f \n"
    for n in range(1, nobj):
        out += b"%010d 00000 n \n" % offsets[n]
    out += (b"trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n"
            % (nobj, xref_pos))
    return out

if __name__ == "__main__":
    data = build()
    with open("type3-vector.pdf", "wb") as f:
        f.write(data)
    print("wrote type3-vector.pdf (%d bytes)" % len(data))
