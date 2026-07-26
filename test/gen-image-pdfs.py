#!/usr/bin/env python3
"""Hand-author minimal image-test PDFs (no reportlab): a DCTDecode JPEG, a Flate
raw-RGB image, an ImageMask painted in a colour, an image + semi-transparent
SMask over a coloured background, and an Indexed-colour image.  Each is one page,
200x200 pt, with the image placed via `cm` scaling the unit square."""
import zlib, io, struct
from PIL import Image, ImageDraw

class PDF:
    def __init__(self):
        self.objs = {}     # num -> bytes
        self.n = 0
    def add(self, body):
        self.n += 1
        self.objs[self.n] = body
        return self.n
    def write(self, path, root):
        out = io.BytesIO()
        out.write(b"%PDF-1.7\n%\xe2\xe3\xcf\xd3\n")
        offsets = {}
        for num in sorted(self.objs):
            offsets[num] = out.tell()
            out.write(("%d 0 obj\n" % num).encode())
            out.write(self.objs[num])
            out.write(b"\nendobj\n")
        xref_off = out.tell()
        out.write(("xref\n0 %d\n" % (self.n + 1)).encode())
        out.write(b"0000000000 65535 f \n")
        for num in range(1, self.n + 1):
            out.write(("%010d 00000 n \n" % offsets[num]).encode())
        out.write(("trailer\n<< /Size %d /Root %d 0 R >>\nstartxref\n%d\n%%%%EOF\n"
                   % (self.n + 1, root, xref_off)).encode())
        open(path, "wb").write(out.getvalue())
        print("wrote", path, len(out.getvalue()), "bytes")

def page_with_image(img_obj_body, content_stream, extra_res=b""):
    """Return a PDF with one page whose XObject /Im0 is IMG_OBJ_BODY (a full
    stream object body) and whose content is CONTENT_STREAM (bytes)."""
    pdf = PDF()
    img_num = pdf.add(img_obj_body)
    cs = zlib.compress(content_stream)
    content_num = pdf.add(b"<< /Length %d /Filter /FlateDecode >>\nstream\n" % len(cs)
                          + cs + b"\nendstream")
    res = (b"<< /XObject << /Im0 %d 0 R >> %s >>" % (img_num, extra_res))
    page_num = pdf.add(b"")   # placeholder to reserve number order
    pages_num = pdf.add(b"")
    cat_num = pdf.add(b"")
    pdf.objs[page_num] = (b"<< /Type /Page /Parent %d 0 R /MediaBox [0 0 200 200] "
                          b"/Resources %s /Contents %d 0 R >>"
                          % (pages_num, res, content_num))
    pdf.objs[pages_num] = (b"<< /Type /Pages /Kids [%d 0 R] /Count 1 >>" % page_num)
    pdf.objs[cat_num] = (b"<< /Type /Catalog /Pages %d 0 R >>" % pages_num)
    return pdf, cat_num

# ---- (a) DCTDecode JPEG ----------------------------------------------------
im = Image.new("RGB", (64, 64))
d = ImageDraw.Draw(im)
d.rectangle([0, 0, 31, 31], fill=(220, 40, 40))
d.rectangle([32, 0, 63, 31], fill=(40, 200, 40))
d.rectangle([0, 32, 31, 63], fill=(40, 40, 220))
d.rectangle([32, 32, 63, 63], fill=(230, 210, 30))
buf = io.BytesIO(); im.save(buf, "JPEG", quality=90); jpg = buf.getvalue()
body = (b"<< /Type /XObject /Subtype /Image /Width 64 /Height 64 "
        b"/ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /DCTDecode /Length %d >>\n"
        b"stream\n" % len(jpg)) + jpg + b"\nendstream"
# content: fill grey background, then draw the image scaled to 150x150 at (25,25)
content = (b"0.85 0.85 0.85 rg 0 0 200 200 re f\n"
           b"q 150 0 0 150 25 25 cm /Im0 Do Q\n")
pdf, root = page_with_image(body, content)
pdf.write("test/img-jpeg.pdf", root)

# ---- (b) Flate raw RGB -----------------------------------------------------
im2 = Image.new("RGB", (4, 4))
im2.putpixel((0, 0), (255, 0, 0)); im2.putpixel((3, 3), (0, 0, 255))
for y in range(4):
    for x in range(4):
        im2.putpixel((x, y), (x * 80, y * 80, 128))
raw = im2.tobytes()  # RGB row-major, top row first
flate = zlib.compress(raw)
body = (b"<< /Type /XObject /Subtype /Image /Width 4 /Height 4 "
        b"/ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /FlateDecode /Length %d >>\n"
        b"stream\n" % len(flate)) + flate + b"\nendstream"
content = (b"1 1 1 rg 0 0 200 200 re f\n"
           b"q 160 0 0 160 20 20 cm /Im0 Do Q\n")
pdf, root = page_with_image(body, content)
pdf.write("test/img-flate.pdf", root)

# ---- (c) ImageMask ---------------------------------------------------------
# 8x8 1-bit stencil: an X shape.  sample 0 = paint (default Decode).
mask_rows = []
for y in range(8):
    bits = 0
    for x in range(8):
        on = (x == y) or (x == 7 - y)          # X pattern
        # paint where sample==0, so set bit to 0 where we want ink
        bit = 0 if on else 1
        bits = (bits << 1) | bit
    mask_rows.append(bits)
maskdata = bytes(mask_rows)
body = (b"<< /Type /XObject /Subtype /Image /Width 8 /Height 8 "
        b"/ImageMask true /BitsPerComponent 1 /Length %d >>\n"
        b"stream\n" % len(maskdata)) + maskdata + b"\nendstream"
content = (b"1 1 1 rg 0 0 200 200 re f\n"
           b"0.9 0.1 0.1 rg\n"                  # fill colour the mask paints in
           b"q 160 0 0 160 20 20 cm /Im0 Do Q\n")
pdf, root = page_with_image(body, content)
pdf.write("test/img-mask.pdf", root)

# ---- (d) image + semi-transparent SMask over a coloured background ---------
base = Image.new("RGB", (32, 32), (255, 120, 0))    # solid orange
rawb = base.tobytes(); fb = zlib.compress(rawb)
# SMask: horizontal alpha ramp 0..255 (left transparent -> right opaque)
sm = Image.new("L", (32, 32))
for y in range(32):
    for x in range(32):
        sm.putpixel((x, y), int(x / 31 * 255))
raws = sm.tobytes(); fs = zlib.compress(raws)
pdf = PDF()
smask_num = pdf.add((b"<< /Type /XObject /Subtype /Image /Width 32 /Height 32 "
                     b"/ColorSpace /DeviceGray /BitsPerComponent 8 /Filter /FlateDecode "
                     b"/Length %d >>\nstream\n" % len(fs)) + fs + b"\nendstream")
img_num = pdf.add((b"<< /Type /XObject /Subtype /Image /Width 32 /Height 32 "
                   b"/ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /FlateDecode "
                   b"/SMask %d 0 R /Length %d >>\nstream\n" % (smask_num, len(fb)))
                  + fb + b"\nendstream")
content = (b"0 0 0.6 rg 0 0 200 200 re f\n"        # dark-blue background
           b"q 160 0 0 160 20 20 cm /Im0 Do Q\n")
cs = zlib.compress(content)
content_num = pdf.add(b"<< /Length %d /Filter /FlateDecode >>\nstream\n" % len(cs)
                      + cs + b"\nendstream")
page_num = pdf.add(b""); pages_num = pdf.add(b""); cat_num = pdf.add(b"")
pdf.objs[page_num] = (b"<< /Type /Page /Parent %d 0 R /MediaBox [0 0 200 200] "
                      b"/Resources << /XObject << /Im0 %d 0 R >> >> /Contents %d 0 R >>"
                      % (pages_num, img_num, content_num))
pdf.objs[pages_num] = b"<< /Type /Pages /Kids [%d 0 R] /Count 1 >>" % page_num
pdf.objs[cat_num] = b"<< /Type /Catalog /Pages %d 0 R >>" % pages_num
pdf.write("test/img-smask.pdf", cat_num)

# ---- (e) Indexed colour image ----------------------------------------------
# 4-colour palette, 8x8 image, BPC 8, indices choose palette entries.
palette = bytes([255, 0, 0,  0, 255, 0,  0, 0, 255,  255, 255, 0])  # R G B Y
idx = bytearray()
for y in range(8):
    for x in range(8):
        idx.append(((x // 2) + (y // 2)) % 4)
lut_hex = palette.hex().upper()
body = (b"<< /Type /XObject /Subtype /Image /Width 8 /Height 8 "
        b"/ColorSpace [ /Indexed /DeviceRGB 3 < %s > ] /BitsPerComponent 8 /Length %d >>\n"
        b"stream\n" % (lut_hex.encode(), len(idx))) + bytes(idx) + b"\nendstream"
content = (b"1 1 1 rg 0 0 200 200 re f\n"
           b"q 160 0 0 160 20 20 cm /Im0 Do Q\n")
pdf, root = page_with_image(body, content)
pdf.write("test/img-indexed.pdf", root)

# ---- (f) inline image (BI/ID/EI), DeviceRGB 2x2 raw ------------------------
inline = bytes([255,0,0, 0,255,0, 0,0,255, 255,255,0])   # 2x2 RGB, top-left red
pdf = PDF()
content = (b"1 1 1 rg 0 0 200 200 re f\n"
           b"q 160 0 0 160 20 20 cm\n"
           b"BI /W 2 /H 2 /CS /RGB /BPC 8 ID " + inline + b" EI Q\n")
cs = zlib.compress(content)
content_num = pdf.add(b"<< /Length %d /Filter /FlateDecode >>\nstream\n" % len(cs)
                      + cs + b"\nendstream")
page_num = pdf.add(b""); pages_num = pdf.add(b""); cat_num = pdf.add(b"")
pdf.objs[page_num] = (b"<< /Type /Page /Parent %d 0 R /MediaBox [0 0 200 200] "
                      b"/Resources << >> /Contents %d 0 R >>" % (pages_num, content_num))
pdf.objs[pages_num] = b"<< /Type /Pages /Kids [%d 0 R] /Count 1 >>" % page_num
pdf.objs[cat_num] = b"<< /Type /Catalog /Pages %d 0 R >>" % pages_num
pdf.write("test/img-inline.pdf", cat_num)

print("all image fixtures generated")
