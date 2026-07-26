#!/usr/bin/env python3
"""Hand-author transparency-GROUP test PDFs (ISO 32000-1 §11.4.5-11.4.7): a form
XObject carrying /Group << /S /Transparency ... >> is composited among its members
onto the group's own initial backdrop, and the RESULT composited onto the page once
at the constant alpha.  Painting the members directly onto the page would make a
semi-transparent group double-composite against the page and wash out / show the
wrong overlap colour.

  group-iso.pdf      : an ISOLATED group (/I true) at ca=0.5 over a yellow backdrop,
                       containing an opaque red rect then an opaque blue rect that
                       overlap.  The group composites red+blue (blue wins the
                       overlap) on a transparent backdrop, THEN 0.5 over yellow — so
                       the overlap is blue-over-yellow, NOT a red+blue double blend.
  group-knockout.pdf : a KNOCKOUT group (/K true, /I true) at ca=0.5 with the same
                       two opaque overlapping rects.  Each element composites with
                       the group's initial backdrop, so the later (blue) element
                       knocks out the earlier (red) where they overlap — top wins.
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

W, H = 200, 130

# the group's members: opaque red rect then opaque blue rect, overlapping
GROUP = (b"1 0 0 rg 20 20 110 80 re f\n"      # red
         b"0 0 1 rg 70 40 110 70 re f\n")     # blue (overlaps the red)

def build(path, isolated, knockout):
    pdf = PDF()
    # transparency-group form XObject
    grp = b"<< /S /Transparency /I %s /K %s >>" % (
        b"true" if isolated else b"false", b"true" if knockout else b"false")
    fm = pdf.add(b"<< /Type /XObject /Subtype /Form /FormType 1 "
                 b"/BBox [0 0 %d %d] /Group %s /Length %d >>\nstream\n%s\nendstream"
                 % (W, H, grp, len(GROUP), GROUP))
    gs = pdf.add(b"<< /Type /ExtGState /ca 0.5 /CA 0.5 >>")
    # page content: yellow backdrop, then the group at ca=0.5
    content = (b"1 1 0 rg 0 0 %d %d re f\n" % (W, H)) + b"/GS0 gs /Fm0 Do\n"
    res = (b"<< /ExtGState << /GS0 %d 0 R >> /XObject << /Fm0 %d 0 R >> >>"
           % (gs, fm))
    cnum = pdf.add(b"<< /Length %d >>\nstream\n%s\nendstream" % (len(content), content))
    page = pdf.add(b""); pages = pdf.add(b""); cat = pdf.add(b"")
    pdf.objs[page] = (b"<< /Type /Page /Parent %d 0 R /MediaBox [0 0 %d %d] "
                      b"/Resources %s /Contents %d 0 R >>" % (pages, W, H, res, cnum))
    pdf.objs[pages] = b"<< /Type /Pages /Kids [%d 0 R] /Count 1 >>" % page
    pdf.objs[cat] = b"<< /Type /Catalog /Pages %d 0 R >>" % pages
    pdf.write(path, cat)

build("group-iso.pdf",      isolated=True, knockout=False)
build("group-knockout.pdf", isolated=True, knockout=True)
