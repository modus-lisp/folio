#!/usr/bin/env python3
"""folio-vs-pdfium conformance harness (M7).

The reusable fidelity oracle: over a two-tier CORPUS (feature micro-tests +
real-world PDFs) render each page in folio and in pypdfium2 (Chrome's exact PDF
engine) at a matched scale, score fidelity (structural SSIM + dark-ink geometry),
CLASSIFY each divergence (match / AA-hinting / unsupported-codec / encrypted /
bug), write montages for the notable ones, and print a ranked report.

Run:  python3 conformance.py            # full corpus
      python3 conformance.py micro      # just the feature micro-tests
      python3 conformance.py real       # just the real-world PDFs
      python3 conformance.py --no-render # reuse existing folio PNGs

folio renders via test/render-batch.lisp (sbcl).  Output PNGs + montages land in
test/conf-out/ (gitignored).
"""
import os, sys, subprocess, math
import numpy as np
import pypdfium2 as pdfium
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
HOME = os.path.expanduser("~")
OUT = os.path.join(HERE, "conf-out")
os.makedirs(OUT, exist_ok=True)

def P(*a):  # path in test/
    return os.path.join(HERE, *a)

# ---- the corpus -------------------------------------------------------------
# (name, pdf-path, page0, scale, tier, note)
MICRO = [
    ("a-rect",        P("a-rect.pdf"),        0, 2.0, "vector fill (re/f)"),
    ("b-flate",       P("b-flate.pdf"),       0, 2.0, "flate content stream"),
    ("c-xrefstm",     P("c-xrefstm.pdf"),     0, 2.0, "xref stream + objstm"),
    ("d-array",       P("d-array.pdf"),       0, 2.0, "content array join"),
    ("text-test",     P("text-test.pdf"),     0, 2.0, "text positioning"),
    ("real-sample",   P("real-sample.pdf"),   0, 2.0, "embedded TrueType"),
    ("real-simple-cff",P("real-simple-cff.pdf"),0, 3.0, "embedded CFF/Type1C"),
    ("type1-embed",   P("type1-embed.pdf"),   0, 3.0, "embedded Type1 (FontFile/eexec)"),
    ("real-cid-tt",   P("real-cid-tt.pdf"),   0, 3.0, "Type0 CIDFontType2"),
    ("real-cid-cff",  P("real-cid-cff.pdf"),  0, 3.0, "Type0 CIDFontType0"),
    ("real-cid-keyed",P("real-cid-keyed.pdf"),0, 3.0, "CID-keyed CFF (CJK)"),
    ("img-jpeg",      P("img-jpeg.pdf"),      0, 2.0, "DCTDecode image"),
    ("img-flate",     P("img-flate.pdf"),     0, 2.0, "flate RGB image"),
    ("img-mask",      P("img-mask.pdf"),      0, 2.0, "ImageMask stencil"),
    ("img-smask",     P("img-smask.pdf"),     0, 2.0, "soft-mask alpha"),
    ("img-indexed",   P("img-indexed.pdf"),   0, 2.0, "indexed palette"),
    ("img-inline",    P("img-inline.pdf"),    0, 2.0, "inline image (BI)"),
    ("ccitt-micro",   P("ccitt-micro.pdf"),   0, 4.0, "CCITT Group-4 fax image"),
    ("annot-stamp",   P("annot-stamp.pdf"),   0, 2.0, "annotation appearance (/AP)"),
    ("blend-modes",   P("blend-modes.pdf"),   0, 3.0, "ExtGState blend modes (/BM)"),
    ("smask-fade",    P("smask-fade.pdf"),    0, 3.0, "soft mask (/SMask luminosity)"),
    ("smask-image",   P("smask-image.pdf"),   0, 3.0, "image under /SMask (vignette)"),
    ("shade-axial",   P("shade-axial.pdf"),   0, 3.0, "axial gradient (sh, type-2 fn)"),
    ("shade-radial",  P("shade-radial.pdf"),  0, 3.0, "radial gradient (sh, blends-of-circles)"),
    ("shade-stitch",  P("shade-stitch.pdf"),  0, 3.0, "axial + stitching fn (type-3)"),
    ("shade-pattern", P("shade-pattern.pdf"), 0, 3.0, "shading pattern fill (PatternType 2)"),
    ("shade-sampled", P("shade-sampled.pdf"), 0, 3.0, "axial + sampled fn (type-0)"),
    ("shade-ps",      P("shade-ps.pdf"),      0, 3.0, "axial + PostScript-calc fn (type-4)"),
    ("shade-smask",   P("shade-smask.pdf"),   0, 3.0, "gradient under a soft-mask group"),
]
REAL = [
    ("arxiv-p0",   P("real-arxiv.pdf"),          0, 1.5, "academic paper (title)"),
    ("arxiv-p2",   P("real-arxiv.pdf"),          2, 1.5, "academic paper (figure)"),
    ("bert-p0",    os.path.join(HOME,"sa-1810.04805.pdf"),0,1.5,"academic paper"),
    ("gpt3-p0",    os.path.join(HOME,"sa-2005.14165.pdf"),0,1.5,"academic paper"),
    ("instructgpt",os.path.join(HOME,"sa-2203.02155.pdf"),0,1.5,"academic paper"),
    ("llama-p0",   os.path.join(HOME,"sa-2302.13971.pdf"),0,1.5,"academic paper"),
    ("gov-w9",     P("gov-w9.pdf"),              0, 1.5, "government form (AcroForm)"),
    ("annots",     os.path.join(HOME,"cand-2.pdf"),0,1.5,"links/annotations doc"),
    ("scan-flate", os.path.join(HOME,"cand-3.pdf"),0,1.0,"scanned bulletin (Flate img)"),
    ("scan-ccitt", P("scan-ccitt.pdf"),          0, 1.0, "scanned CCITT G4 (codec gap)"),
    ("enc-aes256", P("enc-aes-256.pdf"),          0, 2.0, "encrypted AES-256 (empty pw)"),
    ("enc-userpw", P("enc-userpw.pdf"),           0, 2.0, "encrypted, real password"),
]

# ---- scoring ----------------------------------------------------------------
def to_gray_arr(img, size):
    return np.asarray(img.convert("L").resize(size, Image.BILINEAR), dtype=np.float64)

def ssim(a, b):
    """Global SSIM over two equal-size grayscale arrays."""
    mu_a, mu_b = a.mean(), b.mean()
    va, vb = a.var(), b.var()
    cov = ((a - mu_a) * (b - mu_b)).mean()
    c1, c2 = (0.01*255)**2, (0.03*255)**2
    return ((2*mu_a*mu_b + c1)*(2*cov + c2)) / ((mu_a**2 + mu_b**2 + c1)*(va + vb + c2))

def windowed_ssim(a, b, win=16):
    """Mean SSIM over non-overlapping windows (more sensitive to layout)."""
    h, w = a.shape
    vals = []
    for y in range(0, h - win + 1, win):
        for x in range(0, w - win + 1, win):
            vals.append(ssim(a[y:y+win, x:x+win], b[y:y+win, x:x+win]))
    return float(np.mean(vals)) if vals else ssim(a, b)

def ink_geom(img, thresh=160):
    g = np.asarray(img.convert("L"))
    h, w = g.shape
    ys, xs = np.where(g < thresh)
    if len(xs) == 0:
        return None
    return dict(cx=xs.mean()/w, cy=ys.mean()/h, ink=len(xs)/(w*h),
                bbox=(xs.min()/w, ys.min()/h, xs.max()/w, ys.max()/h))

def render_pdfium(pdf, page, scale, out_png):
    doc = pdfium.PdfDocument(pdf)
    img = doc[page].render(scale=scale).to_pil().convert("RGB")
    img.save(out_png)
    return img

def montage(folio_img, pdfium_img, path):
    a = folio_img.convert("RGB"); b = pdfium_img.convert("RGB")
    h = min(a.height, b.height, 900)
    def fit(im):
        return im.resize((int(im.width*h/im.height), h), Image.BILINEAR)
    a, b = fit(a), fit(b)
    diff = Image.fromarray(
        (255 - np.abs(to_gray_arr(a,(b.width,h)) - to_gray_arr(b,(b.width,h)))
         ).astype(np.uint8)).convert("RGB")
    canvas = Image.new("RGB", (a.width + b.width + diff.width + 20, h), (200,200,200))
    canvas.paste(a, (0,0)); canvas.paste(b, (a.width+10,0))
    canvas.paste(diff, (a.width+b.width+20,0))
    canvas.save(path)

def classify(status, sw, geom_f, geom_o):
    if status.get("err"):
        return "BUG (render error: %s)" % status["err"], 0.0
    if status.get("enc") == "UNSUPPORTED":
        return "ENCRYPTED (graceful blank surface)", None
    deferred = status.get("deferred")
    if deferred and deferred != "-":
        return "UNSUPPORTED CODEC (%s)" % deferred, None
    if geom_o is None:
        return "match (both empty)", 1.0
    if geom_f is None:
        return "BUG (folio blank, pdfium has ink)", sw
    dcx = abs(geom_f["cx"]-geom_o["cx"]); dcy = abs(geom_f["cy"]-geom_o["cy"])
    ratio = geom_f["ink"]/geom_o["ink"] if geom_o["ink"] else 0
    if sw >= 0.88:
        return "match", sw
    if sw >= 0.70 and dcx < 0.02 and dcy < 0.02 and 0.45 <= ratio <= 1.25:
        return "AA/hinting (acceptable)", sw
    return "DIVERGENCE (investigate)  d=(%.3f,%.3f) inkR=%.2f" % (dcx,dcy,ratio), sw

# ---- driver -----------------------------------------------------------------
def run_folio(entries):
    manifest = os.path.join(OUT, "manifest.txt")
    with open(manifest, "w") as f:
        for name, pdf, page, scale, note in entries:
            f.write("%s %s %d %s\n" % (name, pdf, page, scale))
    print("... rendering %d pages in folio via sbcl ..." % len(entries), flush=True)
    r = subprocess.run(["sbcl","--script",P("render-batch.lisp"), manifest, OUT],
                       capture_output=True, text=True, timeout=1200)
    status = {}
    for line in r.stdout.splitlines():
        if not line.startswith("STATUS"): continue
        parts = line.split()
        key = parts[1] + "|" + parts[2]
        d = {}
        for tok in parts[3:]:
            if tok == "ok": d["ok"] = True
            elif "=" in tok:
                k,v = tok.split("=",1); d[k] = v
        status[key] = d
    if not status:
        print("!! folio render produced no STATUS lines; stderr tail:")
        print("\n".join(r.stderr.splitlines()[-15:]))
    return status

def main():
    args = [a for a in sys.argv[1:]]
    do_render = "--no-render" not in args
    args = [a for a in args if a != "--no-render"]
    which = args[0] if args else "all"
    entries = []
    if which in ("all","micro"): entries += [(n,p,pg,s,note,"micro") for (n,p,pg,s,note) in MICRO]
    if which in ("all","real"):  entries += [(n,p,pg,s,note,"real") for (n,p,pg,s,note) in REAL]

    ent4 = [(n,p,pg,s,note) for (n,p,pg,s,note,tier) in entries]
    status = run_folio(ent4) if do_render else {}

    rows = []
    for (name,pdf,page,scale,note,tier) in entries:
        key = "%s|%d" % (name,page)
        st = status.get(key, {})
        folio_png = os.path.join(OUT, "folio-%s-p%d.png" % (name,page))
        if not os.path.exists(pdf):
            rows.append((tier,name,note,"MISSING PDF",None,None)); continue
        try:
            opng = os.path.join(OUT, "pdfium-%s-p%d.png" % (name,page))
            oimg = render_pdfium(pdf, page, scale, opng)
        except Exception as e:
            # pdfium cannot open a password-protected doc either — folio's graceful
            # blank surface is the correct behaviour here, not a divergence.
            if st.get("enc") == "UNSUPPORTED":
                rows.append((tier,name,note,"ENCRYPTED (graceful blank; pdfium needs password too)",None,st))
            else:
                rows.append((tier,name,note,"pdfium error: %s"%str(e)[:40],None,None))
            continue
        if not os.path.exists(folio_png):
            rows.append((tier,name,note,"folio no PNG (%s)"%st.get("err","?"),None,None)); continue
        fimg = Image.open(folio_png)
        # match sizes for SSIM
        size = (min(fimg.width,oimg.width,700), min(fimg.height,oimg.height,900))
        fa = to_gray_arr(fimg, size); oa = to_gray_arr(oimg, size)
        sw = windowed_ssim(fa, oa)
        gf, go = ink_geom(fimg), ink_geom(oimg)
        cls, score = classify(st, sw, gf, go)
        # montage the notable ones
        if ("DIVERGENCE" in cls) or ("BUG" in cls) or ("UNSUPPORTED" in cls) or ("ENCRYPTED" in cls):
            try: montage(fimg, oimg, os.path.join(OUT, "montage-%s-p%d.png"%(name,page)))
            except Exception: pass
        rows.append((tier,name,note,cls,sw,st))

    # ---- report ----
    for tier in ("micro","real"):
        trows = [r for r in rows if r[0]==tier]
        if not trows: continue
        print("\n" + "="*96)
        print("  %s TESTS  (folio vs pdfium)" % tier.upper())
        print("="*96)
        print("  %-16s %-30s %-8s %s" % ("name","feature","SSIM","classification"))
        print("  " + "-"*92)
        # sort worst-first within tier
        def sk(r): return r[4] if r[4] is not None else -1
        for (_,name,note,cls,sw,st) in sorted(trows, key=sk):
            sws = ("%.3f"%sw) if sw is not None else "  -  "
            print("  %-16s %-30s %-8s %s" % (name, note[:30], sws, cls))
    # summary
    micro = [r for r in rows if r[0]=="micro" and r[4] is not None]
    if micro:
        good = sum(1 for r in micro if "match" in r[3] or "AA" in r[3])
        print("\n  micro-tests faithful: %d/%d (mean SSIM %.3f)" %
              (good, len(micro), np.mean([r[4] for r in micro])))
    print("\n  montages + PNGs in:", OUT)

if __name__ == "__main__":
    main()
