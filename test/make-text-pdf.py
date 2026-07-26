#!/usr/bin/env python3
"""Hand-author a minimal PDF (standard Helvetica, no embedded program) exercising
the text machinery: a word at a known position, a TJ line with kerning, and an
INVISIBLE (Tr 3) string that must NOT appear."""
import struct, zlib, sys, os

def obj(n, body):
    return ("%d 0 obj\n" % n).encode() + body + b"\nendobj\n"

# content stream: Helvetica 24 at (72, 700); a TJ kerned line at (72, 650);
# an invisible OCR-style string at (72, 600) that must render nothing.
content = b"""BT
/F1 24 Tf
1 0 0 1 72 700 Tm
(Hello World) Tj
ET
BT
/F1 24 Tf
1 0 0 1 72 650 Tm
[(V) -160 (A) -80 (W) 200 (ait)] TJ
ET
BT
/F1 24 Tf
3 Tr
1 0 0 1 72 600 Tm
(INVISIBLE-SECRET) Tj
0 Tr
ET
"""

objs = []
objs.append(obj(1, b"<< /Type /Catalog /Pages 2 0 R >>"))
objs.append(obj(2, b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>"))
objs.append(obj(3, b"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] "
                   b"/Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
objs.append(obj(4, b"<< /Length %d >>\nstream\n" % len(content) + content + b"endstream"))
objs.append(obj(5, b"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica "
                   b"/Encoding /WinAnsiEncoding >>"))

out = b"%PDF-1.4\n"
offsets = []
for o in objs:
    offsets.append(len(out))
    out += o
xref_pos = len(out)
out += b"xref\n0 %d\n" % (len(objs) + 1)
out += b"0000000000 65535 f \n"
for off in offsets:
    out += b"%010d 00000 n \n" % off
out += b"trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n" % (len(objs)+1, xref_pos)

open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "text-test.pdf"), "wb").write(out)
print("wrote text-test.pdf (%d bytes)" % len(out))
