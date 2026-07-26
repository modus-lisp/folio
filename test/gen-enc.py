#!/usr/bin/env python3
"""Generate src/enc-data.lisp: the standard PDF encodings (code->glyph-name),
an Adobe-Glyph-List subset (name->unicode), and the standard-14 AFM widths
(name->width).  Sources: cl-pdf's encodings.lisp arrays + the standard-14 AFMs
(both under quicklisp), plus Python's cp1252/mac_roman codecs for unicode."""
import re, sys

CLPDF = "/home/claude/quicklisp/dists/ultralisp/software/mbattyani-cl-pdf-20230621073425"
ENC = CLPDF + "/encodings.lisp"
AFMDIR = CLPDF + "/afm/"

def extract_array(text, marker):
    i = text.index(marker)
    j = text.index("#(", i) + 2
    # read until the matching close paren of this array
    depth = 1
    k = j
    while depth:
        if text[k] == '(':
            depth += 1
        elif text[k] == ')':
            depth -= 1
            if depth == 0:
                break
        k += 1
    body = text[j:k]
    toks = re.findall(r'nil|"([^"]*)"', body)
    # re.findall with a group returns the group text ('' for nil matches)
    raw = re.findall(r'nil|"[^"]*"', body)
    out = []
    for t in raw:
        if t == 'nil':
            out.append(None)
        else:
            out.append(t[1:-1])
    assert len(out) == 256, (marker, len(out))
    return out

txt = open(ENC).read()
STD = extract_array(txt, 'StandardEncoding')
MAC = extract_array(txt, 'MacRomanEncoding')
WIN = extract_array(txt, 'WinAnsiEncoding')
SYM = extract_array(txt, 'SymbolEncoding')
ZAPF = extract_array(txt, 'ZapfDingbatsEncoding')

# --- name -> unicode (AGL subset) -------------------------------------------
agl = {}
def add_from(names, codec):
    for code, nm in enumerate(names):
        if nm is None:
            continue
        try:
            u = bytes([code]).decode(codec)
        except Exception:
            continue
        if len(u) == 1 and nm not in agl:
            agl[nm] = ord(u)

add_from(WIN, 'cp1252')
add_from(MAC, 'mac_roman')

# Manual supplements: Standard-only, ligatures, Greek (Symbol), common math.
MANUAL = {
    'space':0x20,'fi':0xFB01,'fl':0xFB02,'ff':0xFB00,'ffi':0xFB03,'ffl':0xFB04,
    'dotlessi':0x0131,'dotlessj':0x0237,'Lslash':0x0141,'lslash':0x0142,
    'OE':0x0152,'oe':0x0153,'quotesingle':0x27,'grave':0x60,'quoteright':0x2019,
    'quoteleft':0x2018,'minus':0x2212,'fraction':0x2044,'bullet':0x2022,
    'periodcentered':0xB7,'nbspace':0xA0,'sfthyphen':0xAD,'Euro':0x20AC,
    'trademark':0x2122,'partialdiff':0x2202,'Delta':0x0394,'product':0x220F,
    'summation':0x2211,'radical':0x221A,'infinity':0x221E,'integral':0x222B,
    'notequal':0x2260,'lessequal':0x2264,'greaterequal':0x2265,'plusminus':0xB1,
    'multiply':0xD7,'divide':0xF7,'degree':0xB0,'element':0x2208,'emptyset':0x2205,
    'universal':0x2200,'existential':0x2203,'logicaland':0x2227,'logicalor':0x2228,
    'logicalnot':0xAC,'arrowright':0x2192,'arrowleft':0x2190,'arrowup':0x2191,
    'arrowdown':0x2193,'arrowboth':0x2194,'angle':0x2220,'gradient':0x2207,
    'approxequal':0x2248,'equivalence':0x2261,'proportional':0x221D,'therefore':0x2234,
    'congruent':0x2245,'similar':0x223C,'union':0x222A,'intersection':0x2229,
    'propersubset':0x2282,'propersuperset':0x2283,'notsubset':0x2284,
    'reflexsubset':0x2286,'reflexsuperset':0x2287,'circleplus':0x2295,
    'circlemultiply':0x2297,'perpendicular':0x22A5,'dotmath':0x22C5,'aleph':0x2135,
    'club':0x2663,'diamond':0x2666,'heart':0x2665,'spade':0x2660,'lozenge':0x25CA,
    'florin':0x0192,'suchthat':0x220B,'asteriskmath':0x2217,'radicalex':0x203E,
}
# Greek: Symbol uppercase + lowercase names -> Greek unicode
GREEK = {
    'Alpha':0x391,'Beta':0x392,'Gamma':0x393,'Delta':0x394,'Epsilon':0x395,
    'Zeta':0x396,'Eta':0x397,'Theta':0x398,'Iota':0x399,'Kappa':0x39A,
    'Lambda':0x39B,'Mu':0x39C,'Nu':0x39D,'Xi':0x39E,'Omicron':0x39F,'Pi':0x3A0,
    'Rho':0x3A1,'Sigma':0x3A3,'Tau':0x3A4,'Upsilon':0x3A5,'Phi':0x3A6,'Chi':0x3A7,
    'Psi':0x3A8,'Omega':0x3A9,
    'alpha':0x3B1,'beta':0x3B2,'gamma':0x3B3,'delta':0x3B4,'epsilon':0x3B5,
    'zeta':0x3B6,'eta':0x3B7,'theta':0x3B8,'iota':0x3B9,'kappa':0x3BA,
    'lambda':0x3BB,'mu':0x3BC,'nu':0x3BD,'xi':0x3BE,'omicron':0x3BF,'pi':0x3C0,
    'rho':0x3C1,'sigma':0x3C3,'tau':0x3C4,'upsilon':0x3C5,'phi':0x3C6,'chi':0x3C7,
    'psi':0x3C8,'omega':0x3C9,'theta1':0x3D1,'phi1':0x3D5,'sigma1':0x3C2,
    'omega1':0x3D6,'Upsilon1':0x3D2,
}
for d in (MANUAL, GREEK):
    for k, v in d.items():
        agl.setdefault(k, v)

# --- standard-14 AFM widths --------------------------------------------------
AFMS = {
    'Helvetica':'Helvetica.afm','Helvetica-Bold':'Helvetica-Bold.afm',
    'Helvetica-Oblique':'Helvetica-Oblique.afm','Helvetica-BoldOblique':'Helvetica-BoldOblique.afm',
    'Times-Roman':'Times-Roman.afm','Times-Bold':'Times-Bold.afm',
    'Times-Italic':'Times-Italic.afm','Times-BoldItalic':'Times-BoldItalic.afm',
    'Courier':'Courier.afm','Courier-Bold':'Courier-Bold.afm',
    'Courier-Oblique':'Courier-Oblique.afm','Courier-BoldOblique':'Courier-BoldOblique.afm',
    'Symbol':'Symbol.afm','ZapfDingbats':'ZapfDingbats.afm',
}
afm_widths = {}
for key, fn in AFMS.items():
    d = {}
    for line in open(AFMDIR + fn, encoding='latin-1'):
        if not line.startswith('C '):
            continue
        m_w = re.search(r'WX\s+(-?\d+)', line)
        m_n = re.search(r'N\s+(\S+)\s*;', line)
        if m_w and m_n:
            d[m_n.group(1)] = int(m_w.group(1))
    afm_widths[key] = d

# --- emit Lisp ---------------------------------------------------------------
def lisp_names(arr):
    parts = []
    for nm in arr:
        parts.append('nil' if nm is None else '"%s"' % nm)
    # 8 per line
    lines = []
    for i in range(0, 256, 8):
        lines.append('  ' + ' '.join(parts[i:i+8]))
    return '#(\n' + '\n'.join(lines) + ')'

out = []
w = out.append
w(';;;; src/enc-data.lisp — GENERATED standard encodings, AGL subset, AFM widths.')
w(';;;; Regenerate with test/gen-enc.py.  Do not edit by hand.')
w('(in-package #:folio)')
w('')
for name, arr in (('+standard-names+', STD), ('+winansi-names+', WIN),
                  ('+macroman-names+', MAC), ('+symbol-names+', SYM),
                  ('+zapf-names+', ZAPF)):
    w('(defparameter %s' % name)
    w('  %s)' % lisp_names(arr))
    w('')

w('(defparameter +agl-data+')
w("  ;; (glyph-name . codepoint)")
items = sorted(agl.items())
lines = []
row = []
for nm, cp in items:
    row.append('("%s" . #x%X)' % (nm, cp))
    if len(row) == 4:
        lines.append('  ' + ' '.join(row)); row = []
if row:
    lines.append('  ' + ' '.join(row))
w("  '(" + '\n '.join(lines) + '))')
w('')

w('(defparameter +afm-width-data+')
w("  ;; (font-key (glyph-name . width) ...)")
for key, d in afm_widths.items():
    w('  ;; ' + key)
w("  '(")
for key, d in afm_widths.items():
    w('    ("%s"' % key)
    items = sorted(d.items())
    row = []
    lines = []
    for nm, wd in items:
        row.append('("%s" . %d)' % (nm, wd))
        if len(row) == 5:
            lines.append('     ' + ' '.join(row)); row = []
    if row:
        lines.append('     ' + ' '.join(row))
    w('\n'.join(lines) + ')')
w('    ))')
w('')

open('/home/claude/folio/src/enc-data.lisp', 'w').write('\n'.join(out))
print("wrote enc-data.lisp: agl=%d entries, afm fonts=%d" % (len(agl), len(afm_widths)))
