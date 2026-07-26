#!/usr/bin/env python3
"""Generate a scanned-style PDF whose page is a bilevel CCITT Group-4 image —
exercises folio's CCITTFaxDecode (ITU-T T.6) path.

The page image is drawn naturally (black marks on a white background).  libtiff's
CCITT encoder treats raw bit 0 as the fax "white" run, so the natural "1"-mode
image (white background = 1) codes the background as fax-black; declaring
/BlackIs1 true in the PDF flips the sample polarity back so the page renders as
black marks on white (matching pdfium)."""
import struct, zlib
from PIL import Image, ImageDraw, ImageFont
import io

W, H = 1000, 1400
img = Image.new("1", (W, H), 1)  # white
d = ImageDraw.Draw(img)
# fake scanned text: horizontal rules + blocks of "text" lines
for i, y in enumerate(range(120, 1300, 46)):
    ln = 900 if i % 5 else 500
    for x in range(80, 80 + ln, 8):
        d.rectangle([x, y, x + 5, y + 14], fill=0)
d.rectangle([80, 60, 920, 66], fill=0)  # title rule

# encode as CCITT Group 4 via a TIFF then pull the strip.  Force a SINGLE strip:
# libtiff codes each strip independently (own EOFB), so a multi-strip image would
# concatenate several G4 sub-streams and a decoder would stop at the first EOFB.
buf = io.BytesIO()
img.save(buf, format="TIFF", compression="group4", strip_size=1 << 30)
buf.seek(0)
tif = Image.open(buf)
# read TIFF strips manually
raw = buf.getvalue()
# Simpler: use libtiff via PIL to get the G4 stream by re-reading tags
import PIL.TiffImagePlugin as T
tif = Image.open(io.BytesIO(raw))
tif.load()
strip_offsets = tif.tag_v2[273]
strip_counts = tif.tag_v2[279]
g4 = b"".join(raw[o:o + c] for o, c in zip(strip_offsets, strip_counts))

# Build a minimal one-page PDF with a CCITTFaxDecode image XObject.
objs = []
def obj(n, body): objs.append((n, body))

obj(1, b"<< /Type /Catalog /Pages 2 0 R >>")
obj(2, b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
obj(3, ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 856] "
        "/Resources << /XObject << /Im0 5 0 R >> >> /Contents 4 0 R >>").encode())
content = b"q 612 0 0 856 0 0 cm /Im0 Do Q"
obj(4, ("<< /Length %d >>" % len(content)).encode() + b"\nstream\n" + content + b"\nendstream")
img_dict = ("<< /Type /XObject /Subtype /Image /Width %d /Height %d "
            "/BitsPerComponent 1 /ColorSpace /DeviceGray /ImageMask false "
            "/Filter /CCITTFaxDecode /DecodeParms << /K -1 /Columns %d /Rows %d "
            "/BlackIs1 true >> /Length %d >>" % (W, H, W, H, len(g4)))
obj(5, img_dict.encode() + b"\nstream\n" + g4 + b"\nendstream")

out = io.BytesIO()
out.write(b"%PDF-1.5\n%\xe2\xe3\xcf\xd3\n")
offsets = {}
for n, body in objs:
    offsets[n] = out.tell()
    out.write(("%d 0 obj\n" % n).encode())
    out.write(body)
    out.write(b"\nendobj\n")
xref_pos = out.tell()
maxn = max(n for n, _ in objs)
out.write(("xref\n0 %d\n" % (maxn + 1)).encode())
out.write(b"0000000000 65535 f \n")
for n in range(1, maxn + 1):
    out.write(("%010d 00000 n \n" % offsets[n]).encode())
out.write(("trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n"
           % (maxn + 1, xref_pos)).encode())

open("scan-ccitt.pdf", "wb").write(out.getvalue())
print("wrote scan-ccitt.pdf", len(out.getvalue()), "bytes; G4 strip", len(g4))
