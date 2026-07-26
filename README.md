# folio

**A PDF renderer in pure Common Lisp.** Clean-room — no FFI, no Poppler, no
MuPDF, no pdfium. folio takes a PDF file and turns its pages into pixels: it
parses the file structure, interprets the content streams, and paints vector
graphics, text, and images through the sibling pure-CL graphics stack.

The CL ecosystem can *generate* PDFs ([`cl-pdf`](https://github.com/mbattyani/cl-pdf))
but has had no native library that *renders* them. folio fills that gap — the
whole pipeline, from byte-level object parsing through the graphics/text model to
a straight-alpha RGBA bitmap, implemented from scratch in Lisp.

The bar is not "draws something"; it is matching the reference. Every page is
scored against **pdfium** (Chrome's PDF engine, via
[`pypdfium2`](https://github.com/pypdfium2-team/pypdfium2)): across a diverse
72-page real-world corpus (academic papers, government forms, scanned documents,
transparency and colour-managed art) folio's **median windowed-SSIM is 0.956**.

```
scribe   (fonts + rasterizer + linear-light compositing)
   |
 gesso    (2D vector graphics: paths, fill, stroke, images, blend/mask compositing)
   |
folio     (PDF parse + content-stream interpreter + font/image/shading engines)
```

## What it renders

| Area | Coverage |
|------|----------|
| **File structure** | classic `xref` + `trailer`, `/Prev` incremental updates, `/Type /XRef` cross-reference **streams** (predictors, `W`-widths, `/Index`), `/Type /ObjStm` compressed **object streams**; scan-recovery when the xref is broken/missing. |
| **Filters** | Flate (+ PNG/TIFF predictors), LZW, ASCIIHex, ASCII85, RunLength. |
| **Graphics** | full path model (lines, Béziers, rectangles), non-zero/even-odd fill, stroking with line width/dash under the CTM, clipping, the `q`/`Q` graphics-state stack, `/Rotate`. |
| **Colour** | DeviceGray/RGB/CMYK, CalGray/CalRGB, **Lab**, ICCBased (by `/N` or `/Alternate`), Indexed, **Separation** and **DeviceN** (tint-transform functions). |
| **Text** | the complete §9 text model (`BT`..`ET`, all positioning/state operators, `Tj`/`TJ`/`'`/`"`, render modes). Glyphs are painted as filled outlines through gesso, so arbitrary text matrices (rotation/skew/negative scale) render correctly. |
| **Fonts** | **Type1** (embedded eexec `FontFile`, own charstring interpreter incl. seac/flex/hint-replacement), **TrueType** (`FontFile2`), **CFF / Type1C** (`FontFile3`, own Type2 interpreter), **Type0 / CID** composite fonts (CIDFontType0 and CIDFontType2, `/CIDToGIDMap`, `/W`+`/DW` metrics), **predefined CJK CMaps** (Adobe-Japan1/GB1/CNS1/Korea1, incl. non-embedded fallback), **Type3** glyph procedures, the standard-14 substitutes, and **Symbol / ZapfDingbats**. Synthetic bold for weight-only substitutes. |
| **Images** | JPEG (`DCTDecode`, baseline + progressive, **CMYK/YCCK**), Flate/raw in every colour space and bit depth, `ImageMask` stencils, `/SMask` and stencil `/Mask` alpha, `/Decode` arrays; scanned-document codecs **CCITT Group 3/4** (T.4/T.6) and **JBIG2** (MQ arithmetic: generic + symbol-dict/text + refinement regions, `/JBIG2Globals`). |
| **Transparency** | `ExtGState` constant alpha, **soft masks** (luminosity + alpha), all **16 blend modes** (12 separable + Hue/Saturation/Color/Luminosity), isolated / knockout transparency **groups**. |
| **Shadings & patterns** | axial (type 2) and radial (type 3) gradients, function-based (type 1); colour functions type 0/2/3/4 (sampled, exponential, stitching, PostScript calculator); **shading patterns** and **tiling patterns** (colored + uncolored). |
| **Form & annotations** | annotation appearance streams (`/AP`), and **AcroForm field recognition** (a queryable model of every field: type, name, value, flags, options, widgets, on-states). |
| **Encryption** | the standard security handler at the empty password: RC4 (V1/V2, R2/R3), AES-128 (AESV2), AES-256 (AESV3, R6) — self-contained MD5/RC4/AES/SHA-2. |

## Usage

```lisp
(asdf:load-system "folio")

;; Parse a PDF from an octet vector.
(defvar *doc*
  (folio:parse-pdf
    (with-open-file (s "paper.pdf" :element-type '(unsigned-byte 8))
      (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
        (read-sequence b s) b))))

(folio:page-count *doc*)                       ; => number of pages

;; Render a page (0-based) to a PNG at 2x device scale.
(folio:render-page-to-png *doc* 0 "page0.png" :scale 2.0)

;; Or render to an in-memory straight-alpha RGBA bitmap (a pigment:img).
(let ((img (folio:render-page 0 :scale 1.5)))   ; (render-page doc n &key scale)
  (pigment:img-w img) (pigment:img-h img) (pigment:img-rgba img))

;; Page geometry and resources.
(let ((pg (folio:get-page *doc* 0)))
  (folio:page-mediabox pg)                      ; (x0 y0 x1 y1) in points
  (folio:page-rotate pg)
  (folio:page-content-bytes pg))                ; decoded content-stream octets

;; Encryption status.
(folio:encryption-status *doc*)                 ; :none | :decrypted | :unsupported

;; Interactive form (AcroForm) recognition.
(when (folio:has-acroform-p *doc*)
  (folio:describe-form *doc*)                   ; print a field table
  (dolist (f (folio:form-fields *doc*))
    (list (folio:form-field-type f)
          (folio:form-field-name f)
          (folio:form-field-value f))))
```

folio is also integrated into [`loom`](https://github.com/modus-lisp), the pure-CL
browser: `application/pdf` responses (and `%PDF-` files) open inline on a
scrollable page canvas, like a browser's built-in viewer.

## Architecture

The pipeline is **parse → interpret → paint**:

- **Parse** (`lexer`, `object`, `filters`, `xref`, `doc`, `encrypt`) — a
  byte-oriented tokenizer and recursive-descent object model; the cross-reference
  machinery (classic + stream + object streams, `/Prev` chains, scan-recovery);
  the stream-filter chain; the standard-security-handler decryptor; and the page-
  tree walk that yields a page's inherited geometry and decoded content bytes.
- **Interpret** (`content`) — a content-stream interpreter that maps the PDF
  graphics state onto a gesso 2D context (the CTM, `q`/`Q`, paths, colour, clip,
  text and the full font machinery, `Do` XObjects, inline images, `gs`).
- **Paint engines** (`font`, `cff`, `type1`, `cmap`, `image`, `ccitt`, `jbig2`,
  `shading`, `pattern`) — folio's own font interpreters, image codecs, gradient
  evaluators, and pattern tilers, all emitting into gesso, which composites (fills,
  strokes, glyph outlines, image blits, soft masks, blend modes) onto scribe's
  analytic-coverage rasterizer.

## Dependencies

All pure-CL modus-lisp siblings, no FFI:

- [`gesso`](https://github.com/modus-lisp/gesso) — 2D vector graphics (the paint target).
- [`scribe`](https://github.com/modus-lisp/scribe) — font parsing, rasterization, compositing.
- [`pigment`](https://github.com/modus-lisp/pigment) — image codecs (JPEG decode).
- [`cram`](https://github.com/modus-lisp/cram) — zlib/DEFLATE inflate (Flate, xref & object streams).

## Bundled data

`data/cmaps.bin` (~2.8 MB) is a compact, preprocessed derivative of the public
[Adobe CMap resources](https://github.com/adobe-type-tools/cmap-resources) — the
predefined CJK CMaps (Adobe-Japan1/GB1/CNS1/Korea1) needed to map byte codes to
CIDs for named-CMap Type0 fonts. Only the codespace/`cidrange`/`cidchar`/`WMode`
data is kept. Distributed under Adobe's BSD-style licence; see `data/cmaps.LICENSE`.

## Conformance

The oracle is **pdfium** via `pypdfium2` — the same engine Chrome ships. The
harness renders each page in both folio and pdfium at a matched scale and scores
windowed SSIM plus dark-ink geometry, then classifies each divergence (match /
antialiasing-hinting / unsupported-codec / encrypted / bug).

```sh
pip install pypdfium2 pillow numpy
cd test && python3 conformance.py            # full corpus
          python3 conformance.py micro       # feature micro-tests only
```

The feature micro-fixtures (one per capability) ship in `test/` and run out of
the box; the real-world tier references PDFs that are **not** redistributed — the
generated ones are reproducible with the `test/gen-*.py` scripts, and you can drop
in your own papers. Missing PDFs are skipped with a note, so the suite always runs
on whatever is present.

## Limitations

Honest not-yet list:

- **JPX / JPEG2000** (`JPXDecode`) — not decoded.
- **Non-isolated transparency groups** — approximated as isolated (backdrop
  removal per §11.4.5 is not modelled); matters only for non-normal blends against
  the page backdrop.
- **Complex-script shaping** — no Arabic/Indic joining or ligature shaping.
- **Vertical (`-V`) CJK layout** — glyph mapping and `WMode` are parsed, but
  vertical positioning / `W2` metrics are not applied.
- **JBIG2** Huffman-coded and halftone regions (the arithmetic generic +
  symbol/text path, which covers the vast majority of scanned PDFs, is done).
- **Mesh shadings** (types 4–7, Coons/tensor).
- Text is antialiased but **not hinted**, so glyph coverage is slightly lighter
  than pdfium's stem-darkened output — a rendering-style difference, not a defect.

## License

MIT © ynniv. See [`LICENSE`](LICENSE). Bundled CMap data under Adobe's licence
(`data/cmaps.LICENSE`).
