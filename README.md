# folio

A pure Common Lisp **PDF renderer**.  The CL ecosystem can *generate* PDFs
([`cl-pdf`](https://github.com/mbattyani/cl-pdf)) but has no native library that
*renders* them — folio fills that gap.  It parses a PDF file down to decoded
content streams and (in later milestones) paints the pages through the sibling
pure-CL graphics stack.  No FFI, no Poppler, no MuPDF: everything is native Lisp.

## Status

**Milestone 0 — the parser — is done.**  folio reads the PDF file structure,
the object model, the cross-reference table (classic *and* stream), object
streams, incremental updates, and the stream-filter chain, and walks the page
tree to hand back a page's inherited geometry and its concatenated, decoded
content-stream bytes.  Rendering (the content-stream interpreter → gesso) is
Milestone 1.

## Dependencies

- [`gesso`](../gesso) — 2D vector graphics (the page-painting target, M1+).
- [`scribe`](../scribe) — text/font rendering (glyph painting, M1+).
- [`pigment`](../pigment) — image codecs (embedded-image XObjects, M1+).
- [`cram`](../cram) — pure-CL zlib inflate (FlateDecode / xref & object streams).

The render dependencies are wired now but only `cram` is exercised in M0.

## What the parser handles (M0)

| Area | Notes |
| ---- | ----- |
| Objects | numbers, literal + hex strings, names (`#xx` escapes), arrays, dicts, booleans, null, indirect references, streams. |
| File structure | `startxref` from the tail, classic `xref` tables + `trailer`, `/Prev` incremental-update chains. |
| Xref streams | `/Type /XRef` — Flate + PNG/TIFF predictors, `W`-width entry records, `/Index`. |
| Object streams | `/Type /ObjStm` — Flate-decoded, compressed objects resolved through the `(objstm, index)` indirection. |
| Filters | FlateDecode (+ predictors 2, 10–15), LZWDecode, ASCIIHexDecode, ASCII85Decode, RunLengthDecode. Image filters (DCT/CCITT/JBIG2/JPX) are left as raw bytes for the future image module. |
| Page tree | `/Type /Pages` → `/Kids` with inherited `/Resources /MediaBox /CropBox /Rotate` down to leaf `/Type /Page`. |
| Robustness | if the xref is broken/missing, the whole file is scanned for `N G obj` (object streams expanded, `/Root` synthesized from the `/Type /Catalog` object). |

## API

```lisp
(folio:parse-pdf bytes)          ; octet vector -> document
(folio:page-count doc)           ; integer
(folio:get-page doc n)           ; 0-based -> page struct
(folio:page-mediabox page)       ; (x0 y0 x1 y1) doubles
(folio:page-resources page)      ; the resources dict
(folio:page-content-bytes page)  ; concatenated, DECODED content-stream octets
(folio:resolve doc ref-or-num)   ; follow an indirect reference (cached)
(folio:dict-get dict "Key")      ; dict lookup (keys are name strings)
```

The object model: dicts are hash-tables (name-string → value), arrays are
vectors, names are `pdf-name` structs, strings are `pdf-string` (octet vector),
references are `pdf-ref{num gen}`, streams are `pdf-stream{dict raw}`; PDF `null`
is the `pdf-null` singleton and booleans are `:true` / `:false`.

## Tests

`test/make-pdfs.lisp` hand-authors four minimal PDFs (uncompressed rectangle,
FlateDecode stream, PDF-1.5 xref-stream + object-stream, `/Contents` array).
`test/dump.lisp` loads folio and prints version / object count / page count /
MediaBox / decoded page-1 content for each test PDF plus any `real-*.pdf`
dropped alongside.

## License

MIT © ynniv.
