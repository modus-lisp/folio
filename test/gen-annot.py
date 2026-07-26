#!/usr/bin/env python3
"""A PDF whose visible ink lives in an ANNOTATION appearance stream (/AP /N),
not the page /Contents — exercises folio's annotation rendering.  The page
content draws only a thin border; a Square annotation draws a red-filled box
with an X.  Before the fix folio showed only the border; pdfium shows both."""
import io

objs = []
def obj(n, body): objs.append((n, body))

obj(1, b"<< /Type /Catalog /Pages 2 0 R >>")
obj(2, b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
obj(3, (b"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 400 400] "
        b"/Contents 4 0 R /Annots [5 0 R] >>"))
# page content: a thin border rectangle only
content = b"q 2 w 20 20 360 360 re S Q"
obj(4, (b"<< /Length %d >>\nstream\n" % len(content)) + content + b"\nendstream")
# a Square annotation whose /AP /N form draws the real ink
obj(5, (b"<< /Type /Annot /Subtype /Square /Rect [100 150 300 300] "
        b"/AP << /N 6 0 R >> >>"))
ap = b"q 1 0 0 rg 0 0 200 150 re f 0 0 1 RG 8 w 0 0 m 200 150 l 0 150 m 200 0 l S Q"
obj(6, (b"<< /Type /XObject /Subtype /Form /BBox [0 0 200 150] "
        b"/Length %d >>\nstream\n" % len(ap)) + ap + b"\nendstream")

out = io.BytesIO()
out.write(b"%PDF-1.5\n%\xe2\xe3\xcf\xd3\n")
offsets = {}
for n, body in objs:
    offsets[n] = out.tell()
    out.write(("%d 0 obj\n" % n).encode()); out.write(body); out.write(b"\nendobj\n")
xref = out.tell()
maxn = max(n for n, _ in objs)
out.write(("xref\n0 %d\n" % (maxn + 1)).encode())
out.write(b"0000000000 65535 f \n")
for n in range(1, maxn + 1):
    out.write(("%010d 00000 n \n" % offsets[n]).encode())
out.write(("trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n"
           % (maxn + 1, xref)).encode())
open("annot-stamp.pdf", "wb").write(out.getvalue())
print("wrote annot-stamp.pdf", len(out.getvalue()), "bytes")
