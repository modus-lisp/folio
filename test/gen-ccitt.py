#!/usr/bin/env python3
"""Small known CCITT Group-4 (ITU-T T.6) bilevel image in a PDF — a micro-test
for folio's fax decoder (pixel-match vs pdfium).

A compact 96x72 bitmap with a distinctive pattern (border, diagonal, a filled
box, a checker) is encoded as a single-strip G4 stream and embedded as an image
XObject.  Drawn naturally (black on white); libtiff codes raw bit 0 as the fax
white run, so /BlackIs1 true restores black-on-white polarity (matches pdfium)."""
import io
from PIL import Image, ImageDraw

W, H = 96, 72
img = Image.new("1", (W, H), 1)          # white
d = ImageDraw.Draw(img)
d.rectangle([2, 2, W - 3, H - 3], outline=0)          # frame
for i in range(min(W, H)):                            # diagonal (vertical modes)
    d.point((i, i), fill=0)
d.rectangle([60, 10, 85, 30], fill=0)                 # solid box (long runs)
for y in range(40, 65, 4):                            # dashes (horizontal modes)
    for x in range(10, 50, 4):
        d.point((x, y), fill=0)

buf = io.BytesIO()
img.save(buf, format="TIFF", compression="group4", strip_size=1 << 30)  # single strip
raw = buf.getvalue()
tif = Image.open(io.BytesIO(raw)); tif.load()
o, c = tif.tag_v2[273], tif.tag_v2[279]
g4 = b"".join(raw[a:a + n] for a, n in zip(o, c))
assert len(o) == 1, "expected a single G4 strip"

objs = []
def obj(n, body): objs.append((n, body))
obj(1, b"<< /Type /Catalog /Pages 2 0 R >>")
obj(2, b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
obj(3, ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 %d %d] "
        "/Resources << /XObject << /Im0 5 0 R >> >> /Contents 4 0 R >>"
        % (W, H)).encode())
content = ("q %d 0 0 %d 0 0 cm /Im0 Do Q" % (W, H)).encode()
obj(4, ("<< /Length %d >>" % len(content)).encode() + b"\nstream\n" + content + b"\nendstream")
img_dict = ("<< /Type /XObject /Subtype /Image /Width %d /Height %d "
            "/BitsPerComponent 1 /ColorSpace /DeviceGray "
            "/Filter /CCITTFaxDecode /DecodeParms << /K -1 /Columns %d /Rows %d "
            "/BlackIs1 true >> /Length %d >>" % (W, H, W, H, len(g4)))
obj(5, img_dict.encode() + b"\nstream\n" + g4 + b"\nendstream")

out = io.BytesIO()
out.write(b"%PDF-1.5\n%\xe2\xe3\xcf\xd3\n")
offsets = {}
for n, body in objs:
    offsets[n] = out.tell()
    out.write(("%d 0 obj\n" % n).encode()); out.write(body); out.write(b"\nendobj\n")
xref_pos = out.tell()
maxn = max(n for n, _ in objs)
out.write(("xref\n0 %d\n" % (maxn + 1)).encode())
out.write(b"0000000000 65535 f \n")
for n in range(1, maxn + 1):
    out.write(("%010d 00000 n \n" % offsets[n]).encode())
out.write(("trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n"
           % (maxn + 1, xref_pos)).encode())
open("ccitt-micro.pdf", "wb").write(out.getvalue())
print("wrote ccitt-micro.pdf", len(out.getvalue()), "bytes; G4", len(g4))
