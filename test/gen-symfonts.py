#!/usr/bin/env python3
"""Hand-author a Symbol + ZapfDingbats test PDF (ISO 32000-1 §9.6.6.2, Annex
D.5/D.6).  The two standard-14 symbolic fonts have NO Latin encoding: their byte
codes select mathematical/Greek glyphs (Symbol) and dingbats (ZapfDingbats) through
their own built-in encodings.  A non-embedded Symbol/ZapfDingbats font must resolve
through those built-in encodings, not StandardEncoding, so the codes render as
•/arrows/Greek/math and ✂/✓/✗/stars instead of ASCII letters.

  symdingbats.pdf : line 1 in /Symbol (Greek abgd + ForAll/Exists/infinity/
                    arrowright/plusminus/multiply), line 2 in /ZapfDingbats (a
                    spread of scissors/pencils/crosses/stars/checkmarks/circles).
"""
import io

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

def lit(codes):
    """A PDF literal string from raw byte codes, octal-escaping the unsafe ones."""
    out = bytearray(b"(")
    for c in codes:
        if c in (0x28, 0x29, 0x5C):        # ( ) \
            out += b"\\" + bytes([c])
        elif 0x20 <= c < 0x7F:
            out += bytes([c])
        else:
            out += ("\\%03o" % c).encode()
    out += b")"
    return bytes(out)

W, H = 360, 140

# Symbol built-in codes: alpha beta gamma delta | forall exists infinity
# arrowright plusminus multiply  (Adobe Symbol encoding)
SYM = [0x61,0x62,0x63,0x64, 0x20, 0x22,0x24,0xA5, 0x20, 0xAE,0xB1,0xB4]
# ZapfDingbats built-in codes: a spread across scissors/pencils/crosses/stars/
# checkmarks/circles (Adobe ZapfDingbats encoding, codes 0x21..0x7E)
ZAPF = [0x22,0x25,0x27,0x2A,0x2B,0x34,0x35,0x36,0x6C,0x6E,0x74,0x75,0x77]

pdf = PDF()
fsym  = pdf.add(b"<< /Type /Font /Subtype /Type1 /BaseFont /Symbol >>")
fzapf = pdf.add(b"<< /Type /Font /Subtype /Type1 /BaseFont /ZapfDingbats >>")
content = (b"BT /F0 26 Tf 20 90 Td " + lit(SYM) + b" Tj ET\n"
           b"BT /F1 26 Tf 20 40 Td " + lit(ZAPF) + b" Tj ET\n")
res = b"<< /Font << /F0 %d 0 R /F1 %d 0 R >> >>" % (fsym, fzapf)
cnum = pdf.add(b"<< /Length %d >>\nstream\n%s\nendstream" % (len(content), content))
page = pdf.add(b""); pages = pdf.add(b""); cat = pdf.add(b"")
pdf.objs[page] = (b"<< /Type /Page /Parent %d 0 R /MediaBox [0 0 %d %d] "
                  b"/Resources %s /Contents %d 0 R >>" % (pages, W, H, res, cnum))
pdf.objs[pages] = b"<< /Type /Pages /Kids [%d 0 R] /Count 1 >>" % page
pdf.objs[cat] = b"<< /Type /Catalog /Pages %d 0 R >>" % pages
pdf.write("symdingbats.pdf", cat)
