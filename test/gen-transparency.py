#!/usr/bin/env python3
"""Hand-author transparency test PDFs (no reportlab): ExtGState blend modes (/BM)
and soft masks (/SMask luminosity groups).  Each is one page; compared against
pdfium in verify-transparency.py.

  blend-modes.pdf : a row of cells, each two overlapping rects composited with a
                    different /BM (Normal/Multiply/Screen/Darken/Lighten/
                    HardLight/Difference/Exclusion).  Tests B(Cb,Cs) at full alpha.
  smask-fade.pdf  : an orange rect under an /SMask /Luminosity group that paints a
                    stepped white->black gray ramp, so the rect fades L->R.
  smask-image.pdf : a DeviceRGB image under an /SMask /Luminosity group of nested
                    gray rects (a boxy vignette): bright centre, dim edges.
"""
import zlib, io
from PIL import Image

class PDF:
    def __init__(self): self.objs = {}; self.n = 0
    def add(self, body):
        self.n += 1; self.objs[self.n] = body; return self.n
    def write(self, path, root):
        out = io.BytesIO()
        out.write(b"%PDF-1.7\n%\xe2\xe3\xcf\xd3\n")
        offs = {}
        for num in sorted(self.objs):
            offs[num] = out.tell()
            out.write(("%d 0 obj\n" % num).encode()); out.write(self.objs[num])
            out.write(b"\nendobj\n")
        xo = out.tell()
        out.write(("xref\n0 %d\n" % (self.n + 1)).encode())
        out.write(b"0000000000 65535 f \n")
        for num in range(1, self.n + 1):
            out.write(("%010d 00000 n \n" % offs[num]).encode())
        out.write(("trailer\n<< /Size %d /Root %d 0 R >>\nstartxref\n%d\n%%%%EOF\n"
                   % (self.n + 1, root, xo)).encode())
        open(path, "wb").write(out.getvalue())
        print("wrote", path, len(out.getvalue()), "bytes")

def flate_stream(body_dict_prefix, raw):
    z = zlib.compress(raw)
    return body_dict_prefix + (b" /Filter /FlateDecode /Length %d >>\nstream\n" % len(z)) + z + b"\nendstream"

def one_page(pdf, content, resources, W, H, root_after=True):
    cs = zlib.compress(content)
    cnum = pdf.add(b"<< /Length %d /Filter /FlateDecode >>\nstream\n" % len(cs) + cs + b"\nendstream")
    page = pdf.add(b""); pages = pdf.add(b""); cat = pdf.add(b"")
    pdf.objs[page] = (b"<< /Type /Page /Parent %d 0 R /MediaBox [0 0 %d %d] "
                      b"/Resources %s /Contents %d 0 R >>" % (pages, W, H, resources, cnum))
    pdf.objs[pages] = b"<< /Type /Pages /Kids [%d 0 R] /Count 1 >>" % page
    pdf.objs[cat] = b"<< /Type /Catalog /Pages %d 0 R >>" % pages
    return cat

# ---------------------------------------------------------------------------
# (1) blend modes
# ---------------------------------------------------------------------------
MODES = ["Normal","Multiply","Screen","Darken","Lighten","HardLight","Difference","Exclusion"]
CELL = 60; PAD = 8; W = CELL*len(MODES); H = 90
pdf = PDF()
# ExtGState objects, one per mode
gs_refs = []
for m in MODES:
    gs_refs.append(pdf.add(b"<< /Type /ExtGState /BM /%s >>" % m.encode()))
gs_res = b"".join(b"/GS%d %d 0 R " % (i, r) for i, r in enumerate(gs_refs))
# base colour (warm), overlay colour (cool)
CB = (0.85, 0.55, 0.20)   # backdrop rect
CS = (0.25, 0.45, 0.85)   # source rect (blended)
buf = [b"1 1 1 rg 0 0 %d %d re f\n" % (W, H)]
for i, m in enumerate(MODES):
    x = i*CELL
    # backdrop rect (opaque, Normal)
    buf.append(b"/GS0 gs %.3f %.3f %.3f rg %d %d 44 55 re f\n" % (CB[0],CB[1],CB[2], x+PAD, 20))
    # source rect with this blend mode, overlapping lower-left, extending onto white
    buf.append(b"/GS%d gs %.3f %.3f %.3f rg %d %d 44 40 re f\n" % (i, CS[0],CS[1],CS[2], x+PAD-4, 8))
buf.append(b"/GS0 gs\n")   # reset
root = one_page(pdf, b"".join(buf), b"<< /ExtGState << %s >> >>" % gs_res, W, H)
pdf.write("test/blend-modes.pdf", root)

# ---------------------------------------------------------------------------
# (2) soft mask luminosity fade
# ---------------------------------------------------------------------------
W = 200; H = 120
pdf = PDF()
# mask group form XObject: a stepped white->black ramp (N vertical strips)
N = 20
strip = []
for i in range(N):
    g = 1.0 - i/(N-1)
    x0 = i*W/N
    strip.append(b"%.3f g %.2f 0 %.2f %d re f\n" % (g, x0, W/N+1, H))
group_content = b"".join(strip)
gnum = pdf.add(flate_stream(
    b"<< /Type /XObject /Subtype /Form /FormType 1 /BBox [0 0 %d %d] "
    b"/Group << /S /Transparency /CS /DeviceGray >>" % (W, H), group_content))
smask = pdf.add(b"<< /Type /Mask /S /Luminosity /G %d 0 R >>" % gnum)
gs = pdf.add(b"<< /Type /ExtGState /SMask %d 0 R >>" % smask)
content = (b"0.90 0.90 0.90 rg 0 0 %d %d re f\n" % (W, H) +   # light-grey bg
           b"q /GS1 gs 0.95 0.45 0.10 rg 10 10 %d %d re f Q\n" % (W-20, H-20))
root = one_page(pdf, content, b"<< /ExtGState << /GS1 %d 0 R >> >>" % gs, W, H)
pdf.write("test/smask-fade.pdf", root)

# ---------------------------------------------------------------------------
# (3) soft mask over an image (boxy vignette)
# ---------------------------------------------------------------------------
W = 200; H = 200
pdf = PDF()
# a colourful DeviceRGB image (diagonal gradient)
iw = ih = 48
im = Image.new("RGB", (iw, ih))
for y in range(ih):
    for x in range(iw):
        im.putpixel((x, y), (int(255*x/(iw-1)), int(255*y/(ih-1)), 160))
raw = im.tobytes()
imgnum = pdf.add(flate_stream(
    b"<< /Type /XObject /Subtype /Image /Width %d /Height %d "
    b"/ColorSpace /DeviceRGB /BitsPerComponent 8" % (iw, ih), raw))
# mask group: nested gray rects — dim edge, bright centre
gc = (b"0.15 g 0 0 %d %d re f\n" % (W, H) +
      b"0.55 g 40 40 %d %d re f\n" % (W-80, H-80) +
      b"1.0 g 75 75 %d %d re f\n" % (W-150, H-150))
gnum = pdf.add(flate_stream(
    b"<< /Type /XObject /Subtype /Form /FormType 1 /BBox [0 0 %d %d] "
    b"/Group << /S /Transparency /CS /DeviceGray >>" % (W, H), gc))
smask = pdf.add(b"<< /Type /Mask /S /Luminosity /G %d 0 R >>" % gnum)
gs = pdf.add(b"<< /Type /ExtGState /SMask %d 0 R >>" % smask)
content = (b"0 0 0.35 rg 0 0 %d %d re f\n" % (W, H) +               # dark-blue bg
           b"q /GS1 gs 180 0 0 180 10 10 cm /Im0 Do Q\n")
res = b"<< /ExtGState << /GS1 %d 0 R >> /XObject << /Im0 %d 0 R >> >>" % (gs, imgnum)
root = one_page(pdf, content, res, W, H)
pdf.write("test/smask-image.pdf", root)

print("transparency fixtures generated")
