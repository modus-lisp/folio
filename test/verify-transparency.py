#!/usr/bin/env python3
"""Render the transparency fixtures in folio and pdfium and compare: whole-page
SSIM plus, for blend-modes, the sampled RGB of each cell's blended overlap region
(folio vs pdfium)."""
import os, subprocess, sys
import numpy as np
import pypdfium2 as pdfium
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "conf-out"); os.makedirs(OUT, exist_ok=True)
def P(*a): return os.path.join(HERE, *a)

TESTS = [
    ("blend-modes", P("blend-modes.pdf"), 0, 3.0),
    ("smask-fade",  P("smask-fade.pdf"),  0, 3.0),
    ("smask-image", P("smask-image.pdf"), 0, 3.0),
]
MODES = ["Normal","Multiply","Screen","Darken","Lighten","HardLight","Difference","Exclusion"]

def render_folio(entries):
    man = os.path.join(OUT, "tr-manifest.txt")
    with open(man, "w") as f:
        for n,p,pg,s in entries: f.write("%s %s %d %s\n" % (n,p,pg,s))
    subprocess.run(["sbcl","--script",P("render-batch.lisp"), man, OUT],
                   capture_output=True, text=True, timeout=600)

def ssim(a, b):
    a=a.astype(np.float64); b=b.astype(np.float64)
    mu_a,mu_b=a.mean(),b.mean(); va,vb=a.var(),b.var()
    cov=((a-mu_a)*(b-mu_b)).mean(); c1,c2=(0.01*255)**2,(0.03*255)**2
    return ((2*mu_a*mu_b+c1)*(2*cov+c2))/((mu_a**2+mu_b**2+c1)*(va+vb+c2))

def main():
    render_folio(TESTS)
    for name,pdf,page,scale in TESTS:
        doc = pdfium.PdfDocument(pdf)
        oimg = doc[page].render(scale=scale).to_pil().convert("RGB")
        opng = os.path.join(OUT, "pdfium-%s-p%d.png" % (name,page)); oimg.save(opng)
        fpng = os.path.join(OUT, "folio-%s-p%d.png" % (name,page))
        fimg = Image.open(fpng).convert("RGB")
        w = min(fimg.width, oimg.width); h = min(fimg.height, oimg.height)
        fa = np.asarray(fimg.resize((w,h)).convert("L"), dtype=np.float64)
        oa = np.asarray(oimg.resize((w,h)).convert("L"), dtype=np.float64)
        print("\n== %-14s  SSIM(L) = %.4f   (folio %dx%d, pdfium %dx%d)" %
              (name, ssim(fa,oa), fimg.width,fimg.height, oimg.width,oimg.height))
        if name == "blend-modes":
            fa3 = np.asarray(fimg.resize((w,h))); oa3 = np.asarray(oimg.resize((w,h)))
            CELL = 60*scale*w/(60*len(MODES)*scale)  # cell px width in resized frame
            cw = w/len(MODES)
            # overlap region of each cell ~ where source rect covers backdrop rect:
            # sample near the middle-bottom of the source rect over the backdrop.
            sy = int(h*0.62)
            print("   cell            folio-RGB        pdfium-RGB      dE")
            for i,m in enumerate(MODES):
                sx = int((i+0.5)*cw)
                f = fa3[sy,sx][:3].astype(int); o = oa3[sy,sx][:3].astype(int)
                de = int(np.abs(f-o).max())
                flag = "" if de<=6 else "  <-- diff"
                print("   %-12s   (%3d,%3d,%3d)   (%3d,%3d,%3d)   %d%s" %
                      (m, f[0],f[1],f[2], o[0],o[1],o[2], de, flag))

if __name__ == "__main__":
    main()
