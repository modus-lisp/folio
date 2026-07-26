#!/usr/bin/env python3
"""Hand-author shading / gradient test PDFs (ISO 32000-1 §8.7.4.5, §8.7.3.3), no
reportlab.  Each is one page, compared against pdfium in conformance.py.

  shade-axial.pdf    : `sh` axial (type 2) red->blue, exponential fn, Extend.
  shade-radial.pdf   : `sh` radial (type 3) concentric white->blue, Extend.
  shade-stitch.pdf   : `sh` axial with a type-3 stitching fn (red->green->blue).
  shade-pattern.pdf  : a circle filled with a shading PATTERN (PatternType 2, radial).
  shade-sampled.pdf  : `sh` axial with a type-0 sampled fn (red/yellow/green/blue).
  shade-ps.pdf       : `sh` axial with a type-4 PostScript-calculator fn.
"""
import zlib, io

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

def one_page(pdf, content, resources, W, H):
    cs = zlib.compress(content)
    cnum = pdf.add(b"<< /Length %d /Filter /FlateDecode >>\nstream\n" % len(cs) + cs + b"\nendstream")
    page = pdf.add(b""); pages = pdf.add(b""); cat = pdf.add(b"")
    pdf.objs[page] = (b"<< /Type /Page /Parent %d 0 R /MediaBox [0 0 %d %d] "
                      b"/Resources %s /Contents %d 0 R >>" % (pages, W, H, resources, cnum))
    pdf.objs[pages] = b"<< /Type /Pages /Kids [%d 0 R] /Count 1 >>" % page
    pdf.objs[cat] = b"<< /Type /Catalog /Pages %d 0 R >>" % pages
    return cat

def circle_path(cx, cy, r):
    k = 0.5522847498 * r
    return (b"%.2f %.2f m " % (cx + r, cy) +
            b"%.2f %.2f %.2f %.2f %.2f %.2f c " % (cx+r, cy+k, cx+k, cy+r, cx, cy+r) +
            b"%.2f %.2f %.2f %.2f %.2f %.2f c " % (cx-k, cy+r, cx-r, cy+k, cx-r, cy) +
            b"%.2f %.2f %.2f %.2f %.2f %.2f c " % (cx-r, cy-k, cx-k, cy-r, cx, cy-r) +
            b"%.2f %.2f %.2f %.2f %.2f %.2f c h" % (cx+k, cy-r, cx+r, cy-k, cx+r, cy))

W, H = 240, 180

# ---------------------------------------------------------------------------
# (a) axial, type-2 exponential fn, Extend
# ---------------------------------------------------------------------------
pdf = PDF()
fn = b"<< /FunctionType 2 /Domain [0 1] /C0 [1 0 0] /C1 [0 0 1] /N 1 >>"
sh = (b"<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [40 0 200 0] "
      b"/Function %s /Extend [true true] >>" % fn)
shnum = pdf.add(sh)
content = (b"1 1 1 rg 0 0 %d %d re f\n" % (W, H) +
           b"q 20 20 %d %d re W n /Sh0 sh Q\n" % (W-40, H-40))
res = b"<< /Shading << /Sh0 %d 0 R >> >>" % shnum
pdf.write("test/shade-axial.pdf", one_page(pdf, content, res, W, H))

# ---------------------------------------------------------------------------
# (b) radial, type-3, concentric white centre -> blue edge, Extend
# ---------------------------------------------------------------------------
pdf = PDF()
fn = b"<< /FunctionType 2 /Domain [0 1] /C0 [1 1 1] /C1 [0.1 0.2 0.8] /N 1 >>"
sh = (b"<< /ShadingType 3 /ColorSpace /DeviceRGB /Coords [120 90 0 120 90 90] "
      b"/Function %s /Extend [true true] >>" % fn)
shnum = pdf.add(sh)
content = (b"0.15 0.15 0.15 rg 0 0 %d %d re f\n" % (W, H) +
           b"q 0 0 %d %d re W n /Sh0 sh Q\n" % (W, H))
res = b"<< /Shading << /Sh0 %d 0 R >> >>" % shnum
pdf.write("test/shade-radial.pdf", one_page(pdf, content, res, W, H))

# ---------------------------------------------------------------------------
# (c) axial with a type-3 STITCHING fn: red -> green -> blue
# ---------------------------------------------------------------------------
pdf = PDF()
f0 = b"<< /FunctionType 2 /Domain [0 1] /C0 [1 0 0] /C1 [0 1 0] /N 1 >>"
f1 = b"<< /FunctionType 2 /Domain [0 1] /C0 [0 1 0] /C1 [0 0 1] /N 1 >>"
stitch = (b"<< /FunctionType 3 /Domain [0 1] /Functions [%s %s] "
          b"/Bounds [0.5] /Encode [0 1 0 1] >>" % (f0, f1))
sh = (b"<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [20 0 220 0] "
      b"/Function %s /Extend [true true] >>" % stitch)
shnum = pdf.add(sh)
content = (b"1 1 1 rg 0 0 %d %d re f\n" % (W, H) +
           b"q 20 20 %d %d re W n /Sh0 sh Q\n" % (W-40, H-40))
res = b"<< /Shading << /Sh0 %d 0 R >> >>" % shnum
pdf.write("test/shade-stitch.pdf", one_page(pdf, content, res, W, H))

# ---------------------------------------------------------------------------
# (d) shading PATTERN (PatternType 2, radial) filling a circle path
# ---------------------------------------------------------------------------
pdf = PDF()
fn = b"<< /FunctionType 2 /Domain [0 1] /C0 [1 0.9 0.4] /C1 [0.6 0.1 0.0] /N 1 >>"
sh = (b"<< /ShadingType 3 /ColorSpace /DeviceRGB /Coords [100 110 5 120 90 80] "
      b"/Function %s /Extend [true true] >>" % fn)
shnum = pdf.add(sh)
pat = b"<< /Type /Pattern /PatternType 2 /Shading %d 0 R /Matrix [1 0 0 1 0 0] >>" % shnum
patnum = pdf.add(pat)
content = (b"0.95 0.95 0.95 rg 0 0 %d %d re f\n" % (W, H) +
           b"/Pattern cs /P0 scn\n" +
           circle_path(120, 90, 70) + b" f\n")
res = b"<< /Pattern << /P0 %d 0 R >> >>" % patnum
pdf.write("test/shade-pattern.pdf", one_page(pdf, content, res, W, H))

# ---------------------------------------------------------------------------
# (e) axial with a type-0 SAMPLED fn (4 stops: red, yellow, green, blue)
# ---------------------------------------------------------------------------
pdf = PDF()
samples = bytes([255,0,0, 255,255,0, 0,255,0, 0,0,255])   # 4 samples x 3 comps, 8bpc
fn = (b"<< /FunctionType 0 /Domain [0 1] /Range [0 1 0 1 0 1] /Size [4] "
      b"/BitsPerSample 8 /Length %d >>\nstream\n" % len(samples)) + samples + b"\nendstream"
fnum = pdf.add(fn)
sh = (b"<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [20 0 220 0] "
      b"/Function %d 0 R /Extend [true true] >>" % fnum)
shnum = pdf.add(sh)
content = (b"1 1 1 rg 0 0 %d %d re f\n" % (W, H) +
           b"q 20 20 %d %d re W n /Sh0 sh Q\n" % (W-40, H-40))
res = b"<< /Shading << /Sh0 %d 0 R >> >>" % shnum
pdf.write("test/shade-sampled.pdf", one_page(pdf, content, res, W, H))

# ---------------------------------------------------------------------------
# (f) axial with a type-4 POSTSCRIPT-calculator fn: R=t, G=1-t, B=0.5
# ---------------------------------------------------------------------------
pdf = PDF()
prog = b"{ dup 1 exch sub 0.5 }"
fn = (b"<< /FunctionType 4 /Domain [0 1] /Range [0 1 0 1 0 1] /Length %d >>\nstream\n"
      % len(prog)) + prog + b"\nendstream"
fnum = pdf.add(fn)
sh = (b"<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [20 0 220 0] "
      b"/Function %d 0 R /Extend [true true] >>" % fnum)
shnum = pdf.add(sh)
content = (b"1 1 1 rg 0 0 %d %d re f\n" % (W, H) +
           b"q 20 20 %d %d re W n /Sh0 sh Q\n" % (W-40, H-40))
res = b"<< /Shading << /Sh0 %d 0 R >> >>" % shnum
pdf.write("test/shade-ps.pdf", one_page(pdf, content, res, W, H))

# ---------------------------------------------------------------------------
# (g) a radial gradient painted UNDER an /SMask luminosity group (pairs the
#     shading path with the M10 transparency path: the gradient should fade out
#     toward the page edges where the mask is dark).
# ---------------------------------------------------------------------------
def flate_stream(prefix, raw):
    z = zlib.compress(raw)
    return prefix + (b" /Filter /FlateDecode /Length %d >>\nstream\n" % len(z)) + z + b"\nendstream"

pdf = PDF()
# a full-frame radial gradient shading
fn = b"<< /FunctionType 2 /Domain [0 1] /C0 [1 0.4 0.0] /C1 [0.0 0.2 0.7] /N 1 >>"
sh = (b"<< /ShadingType 3 /ColorSpace /DeviceRGB /Coords [120 90 0 120 90 130] "
      b"/Function %s /Extend [true true] >>" % fn)
shnum = pdf.add(sh)
# soft-mask group: bright centre disc -> dark edges (a vignette in DeviceGray)
gc = (b"0.0 g 0 0 %d %d re f\n" % (W, H) +
      circle_path(120, 90, 95) + b" 0.6 g f\n" +
      circle_path(120, 90, 55) + b" 1.0 g f\n")
gnum = pdf.add(flate_stream(
    b"<< /Type /XObject /Subtype /Form /FormType 1 /BBox [0 0 %d %d] "
    b"/Group << /S /Transparency /CS /DeviceGray >>" % (W, H), gc))
smask = pdf.add(b"<< /Type /Mask /S /Luminosity /G %d 0 R >>" % gnum)
gs = pdf.add(b"<< /Type /ExtGState /SMask %d 0 R >>" % smask)
content = (b"1 1 1 rg 0 0 %d %d re f\n" % (W, H) +
           b"q /GS1 gs 0 0 %d %d re W n /Sh0 sh Q\n" % (W, H))
res = b"<< /ExtGState << /GS1 %d 0 R >> /Shading << /Sh0 %d 0 R >> >>" % (gs, shnum)
pdf.write("test/shade-smask.pdf", one_page(pdf, content, res, W, H))

print("shading fixtures generated")
