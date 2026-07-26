#!/usr/bin/env python3
"""Hand-author shade-stencil.pdf: an ImageMask stencil painted with a shading
PATTERN fill (§8.7.3.3 + §8.9.6.2) — the stencil's coverage shows the per-pixel
gradient, not a flat colour.  This is the isolated form of the sm-3 corpus case
(a halftone ImageMask over a rainbow axial gradient).  Compared to pdfium in
conformance.py.
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

W, H = 200, 200
pdf = PDF()

# --- a 16x16 ImageMask stencil: a diagonal-stripe pattern (bit 0 = painted) ---
sw, sh = 16, 16
rowbytes = (sw + 7) // 8
data = bytearray()
for y in range(sh):
    row = bytearray(rowbytes)
    for x in range(sw):
        # paint (bit 0) where (x+y) even -> checker/stripe coverage
        painted = ((x + y) & 1) == 0
        if not painted:
            row[x // 8] |= (0x80 >> (x % 8))  # bit 1 = NOT painted (default Decode)
    data += row
mask = pdf.add(b"<< /Type /XObject /Subtype /Image /Width %d /Height %d "
               b"/ImageMask true /BitsPerComponent 1 /Length %d >>\nstream\n"
               % (sw, sh, len(data)) + bytes(data) + b"\nendstream")

# --- an axial shading: red -> green -> blue across the page ---
fn = pdf.add(b"<< /FunctionType 3 /Domain [0 1] /Bounds [0.5] /Encode [0 1 0 1] "
             b"/Functions [ "
             b"<< /FunctionType 2 /Domain [0 1] /C0 [1 0 0] /C1 [0 1 0] /N 1 >> "
             b"<< /FunctionType 2 /Domain [0 1] /C0 [0 1 0] /C1 [0 0 1] /N 1 >> ] >>")
shading = (b"<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [0 0 %d %d] "
           b"/Function %d 0 R /Extend [true true] >>" % (W, H, fn))
pattern = pdf.add(b"<< /Type /Pattern /PatternType 2 /Shading %s /Matrix [1 0 0 1 0 0] >>"
                  % shading)

resources = (b"<< /Pattern << /P0 %d 0 R >> /ColorSpace << /CSP [/Pattern] >> "
             b"/XObject << /Im0 %d 0 R >> >>" % (pattern, mask))

# fill = shading pattern P0; then draw the stencil scaled to fill the page.
content = (b"q /CSP cs /P0 scn\n"
           b"%d 0 0 %d 0 0 cm\n"          # unit square -> full page
           b"/Im0 Do\n"
           b"Q\n" % (W, H))
cs = zlib.compress(content)
cnum = pdf.add(b"<< /Length %d /Filter /FlateDecode >>\nstream\n" % len(cs) + cs + b"\nendstream")
page = pdf.add(b""); pages = pdf.add(b""); cat = pdf.add(b"")
pdf.objs[page] = (b"<< /Type /Page /Parent %d 0 R /MediaBox [0 0 %d %d] "
                  b"/Resources %s /Contents %d 0 R >>" % (pages, W, H, resources, cnum))
pdf.objs[pages] = b"<< /Type /Pages /Kids [%d 0 R] /Count 1 >>" % page
pdf.objs[cat] = b"<< /Type /Catalog /Pages %d 0 R >>" % pages
pdf.write("shade-stencil.pdf", cat)
