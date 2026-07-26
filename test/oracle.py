#!/usr/bin/env python3
"""pdfium (Chrome's engine) oracle: render a PDF page to PNG, and compare the
dark-text region geometry (bbox + centroid) between pdfium and folio's PNG."""
import sys
import pypdfium2 as pdfium
from PIL import Image

def render_pdfium(pdf_path, out_png, scale=2.0):
    doc = pdfium.PdfDocument(pdf_path)
    page = doc[0]
    bmp = page.render(scale=scale)
    img = bmp.to_pil().convert("RGB")
    img.save(out_png)
    return img

def dark_geom(img, thresh=160):
    """Bounding box + centroid + count of 'ink' pixels (luma < thresh), all in
    fractions of width/height so two different pixel sizes compare directly."""
    px = img.convert("L").load()
    w, h = img.size
    minx, miny, maxx, maxy = w, h, -1, -1
    sx = sy = n = 0
    for y in range(h):
        for x in range(w):
            if px[x, y] < thresh:
                minx = min(minx, x); maxx = max(maxx, x)
                miny = min(miny, y); maxy = max(maxy, y)
                sx += x; sy += y; n += 1
    if n == 0:
        return None
    return dict(w=w, h=h, n=n,
                bbox=(minx/w, miny/h, maxx/w, maxy/h),
                cx=sx/n/w, cy=sy/n/h, ink=n/(w*h))

def fmt(g):
    if g is None: return "  (no ink)"
    return ("  bbox=(%.4f %.4f %.4f %.4f) centroid=(%.4f %.4f) ink=%.5f"
            % (g['bbox'][0], g['bbox'][1], g['bbox'][2], g['bbox'][3], g['cx'], g['cy'], g['ink']))

if __name__ == "__main__":
    pdf, folio_png, ora_png = sys.argv[1:4]
    ora = render_pdfium(pdf, ora_png)
    fol = Image.open(folio_png)
    go = dark_geom(ora); gf = dark_geom(fol)
    print("PDF:", pdf)
    print("pdfium:"); print(fmt(go))
    print("folio :"); print(fmt(gf))
    if go and gf:
        print("  d-centroid=(%.4f %.4f) d-bbox=(%.4f %.4f %.4f %.4f) ink-ratio=%.3f"
              % (abs(go['cx']-gf['cx']), abs(go['cy']-gf['cy']),
                 *(abs(a-b) for a, b in zip(go['bbox'], gf['bbox'])),
                 gf['ink']/go['ink'] if go['ink'] else 0))
