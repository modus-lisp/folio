#!/usr/bin/env python3
"""Hand-author tiling-pattern test PDFs (ISO 32000-1 §8.7.3.1, /PatternType 1),
compared against pdfium in the conformance harness.

  tile-colored.pdf   : a rect filled with a COLORED (PaintType 1) tiling pattern
                       whose cell draws a red square + blue square (a checker).
  tile-uncolored.pdf : one UNCOLORED (PaintType 2) stencil pattern (a centred dot)
                       filling two rects in two different scn colours — proves the
                       stencil-recolour path.
  tile-matrix.pdf    : a colored tiling pattern with a non-identity /Matrix (a 30°
                       rotation + scale), so the cell grid is rotated on the page.
"""
import zlib, io, math

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

def stream_obj(pdf, dict_prefix, raw):
    z = zlib.compress(raw)
    return pdf.add(dict_prefix + (b" /Filter /FlateDecode /Length %d >>\nstream\n" % len(z))
                   + z + b"\nendstream")

def one_page(pdf, content, resources, W, H):
    cnum = stream_obj(pdf, b"<<", content)
    page = pdf.add(b""); pages = pdf.add(b""); cat = pdf.add(b"")
    pdf.objs[page] = (b"<< /Type /Page /Parent %d 0 R /MediaBox [0 0 %d %d] "
                      b"/Resources %s /Contents %d 0 R >>" % (pages, W, H, resources, cnum))
    pdf.objs[pages] = b"<< /Type /Pages /Kids [%d 0 R] /Count 1 >>" % page
    pdf.objs[cat] = b"<< /Type /Catalog /Pages %d 0 R >>" % pages
    return cat

W = H = 200

# ---------------------------------------------------------------------------
# (1) colored tiling pattern — a 20x20 cell: red square + blue square (checker)
# ---------------------------------------------------------------------------
pdf = PDF()
cell = (b"1 0 0 rg 1 1 8 8 re f\n"
        b"0 0 1 rg 11 11 8 8 re f\n"
        b"0 0.6 0 rg 11 1 8 8 re f\n"
        b"0.9 0.7 0 rg 1 11 8 8 re f\n")
pat = stream_obj(pdf,
    b"<< /Type /Pattern /PatternType 1 /PaintType 1 /TilingType 1 "
    b"/BBox [0 0 20 20] /XStep 20 /YStep 20 /Matrix [1 0 0 1 0 0] "
    b"/Resources << >>", cell)
res = b"<< /Pattern << /P0 %d 0 R >> >>" % pat
content = (b"/Pattern cs /P0 scn\n"
           b"20 20 160 160 re f\n")
root = one_page(pdf, content, res, W, H)
pdf.write("test/tile-colored.pdf", root)

# ---------------------------------------------------------------------------
# (2) uncolored (PaintType 2) stencil pattern — a centred dot; two rects, two colours
# ---------------------------------------------------------------------------
pdf = PDF()
# cell: a filled disc (approximated by a square+corners) — NO colour operators
stencil = (b"4 4 12 12 re f\n")     # a solid 12x12 square, centred in a 20x20 cell
pat = stream_obj(pdf,
    b"<< /Type /Pattern /PatternType 1 /PaintType 2 /TilingType 1 "
    b"/BBox [0 0 20 20] /XStep 20 /YStep 20 /Matrix [1 0 0 1 0 0] "
    b"/Resources << >>", stencil)
res = (b"<< /Pattern << /P0 %d 0 R >> "
       b"/ColorSpace << /PCS [/Pattern /DeviceRGB] >> >>" % pat)
content = (b"/PCS cs 0.85 0.1 0.1 /P0 scn\n"     # red stencil
           b"10 10 85 180 re f\n"
           b"/PCS cs 0.1 0.2 0.85 /P0 scn\n"     # blue stencil
           b"105 10 85 180 re f\n")
root = one_page(pdf, content, res, W, H)
pdf.write("test/tile-uncolored.pdf", root)

# ---------------------------------------------------------------------------
# (3) colored tiling pattern with a non-identity /Matrix (rotate 30 deg, scale 1.4)
# ---------------------------------------------------------------------------
pdf = PDF()
th = math.radians(30.0); sc = 1.4
c, s = math.cos(th)*sc, math.sin(th)*sc
mat = b"[%.5f %.5f %.5f %.5f 0 0]" % (c, s, -s, c)
cell = (b"0.85 0.2 0.1 rg 2 2 16 6 re f\n"       # a red bar
        b"0.1 0.3 0.8 rg 2 12 16 6 re f\n")      # a blue bar -> stripes, rotated
pat = stream_obj(pdf,
    b"<< /Type /Pattern /PatternType 1 /PaintType 1 /TilingType 1 "
    b"/BBox [0 0 20 20] /XStep 20 /YStep 20 /Matrix " + mat + b" "
    b"/Resources << >>", cell)
res = b"<< /Pattern << /P0 %d 0 R >> >>" % pat
content = (b"/Pattern cs /P0 scn\n"
           b"20 20 160 160 re f\n")
root = one_page(pdf, content, res, W, H)
pdf.write("test/tile-matrix.pdf", root)
