#!/usr/bin/env python3
"""Hand-author a non-separable blend-mode test PDF (ISO 32000-1 §11.3.5.3):
Hue / Saturation / Color / Luminosity.  Four cells, each two overlapping saturated
rects composited with one non-separable /BM.  The overlap is pixel-compared to
pdfium (the blend must use SetLum/SetSat over the whole RGB triple).

  nonsep-blend.pdf
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

MODES = ["Hue", "Saturation", "Color", "Luminosity"]
CELL = 60; PAD = 8; W = CELL*len(MODES); H = 90
pdf = PDF()
gs_refs = [pdf.add(b"<< /Type /ExtGState /BM /%s >>" % m.encode()) for m in MODES]
gs_res = b"".join(b"/GS%d %d 0 R " % (i, r) for i, r in enumerate(gs_refs))
gs_normal = pdf.add(b"<< /Type /ExtGState /BM /Normal >>")

CB = (0.90, 0.45, 0.10)   # warm/orange backdrop
CS = (0.20, 0.35, 0.85)   # cool/blue source (blended)
buf = [b"1 1 1 rg 0 0 %d %d re f\n" % (W, H)]
buf.append(b"/GS%d gs\n" % (len(MODES)))   # Normal for backdrops (gs_normal index)
for i, m in enumerate(MODES):
    x = i*CELL
    buf.append(b"/GS%d gs %.3f %.3f %.3f rg %d %d 44 55 re f\n"
               % (len(MODES), CB[0], CB[1], CB[2], x+PAD, 20))
    buf.append(b"/GS%d gs %.3f %.3f %.3f rg %d %d 44 40 re f\n"
               % (i, CS[0], CS[1], CS[2], x+PAD-4, 8))
buf.append(b"/GS%d gs\n" % (len(MODES)))
# gs indices: GS0..GS3 = modes, GS4 = normal
res = b"<< /ExtGState << %s/GS%d %d 0 R >> >>" % (gs_res, len(MODES), gs_normal)
root = one_page(pdf, b"".join(buf), res, W, H)
pdf.write("test/nonsep-blend.pdf", root)
