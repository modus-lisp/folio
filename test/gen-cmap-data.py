#!/usr/bin/env python3
"""Preprocess Adobe predefined CMap resources into folio's compact binary form.

Reads the plain-text PostScript CMap files (the Adobe cmap-resources set, as
shipped by poppler/ghostscript) and emits one small deflate-compressed binary
per CMap into data/cmaps/.  usecmap inheritance is NOT flattened here — the
parent name is stored and resolved at load time in Lisp (so shared base CMaps
are stored once).  Only the data folio needs is kept: /WMode, the codespace
ranges (byte widths + lo/hi), and the code->CID mapping (cidrange + cidchar).

Binary layout (big-endian), then zlib-compressed:
  magic  "FCM1"
  u8     wmode
  u8     usecmap-name length ; ASCII name
  u16    num codespace ranges ; each u8 nbytes, u32 lo, u32 hi
  u32    num cidranges        ; each u32 lo, u32 hi, u16 cid
  u32    num cidchars         ; each u32 code, u16 cid
"""
import os, re, struct, zlib, sys

SRC = "/usr/share/poppler/cMap"
BUNDLE = os.path.join(os.path.dirname(__file__), "..", "data", "cmaps.bin")

# registries to vendor (Identity-* are handled natively by folio)
REGS = ["Adobe-CNS1", "Adobe-GB1", "Adobe-Japan1", "Adobe-Japan2",
        "Adobe-Korea1", "Adobe-KR"]

hexre = re.compile(rb"<([0-9A-Fa-f]+)>")

def parse(path):
    data = open(path, "rb").read()
    wmode = 0
    usecmap = ""
    codespace = []   # (nbytes, lo, hi)
    cidranges = []   # (lo, hi, cid)
    cidchars = []    # (code, cid)
    lines = data.split(b"\n")
    i = 0
    n = len(lines)
    while i < n:
        line = lines[i]
        s = line.strip()
        m = re.search(rb"/WMode\s+(\d+)\s+def", line)
        if m:
            wmode = int(m.group(1))
        m = re.search(rb"/(\S+)\s+usecmap", line)
        if m:
            usecmap = m.group(1).decode("latin-1")
        if b"begincodespacerange" in s:
            i += 1
            while i < n and b"endcodespacerange" not in lines[i]:
                hs = hexre_all(lines[i])
                if len(hs) >= 2:
                    lo, hi = hs[0], hs[1]
                    nb = len(lo) // 2
                    codespace.append((nb, int(lo, 16), int(hi, 16)))
                i += 1
        elif b"begincidrange" in s:
            i += 1
            while i < n and b"endcidrange" not in lines[i]:
                parts = lines[i].split()
                hs = hexre_all(lines[i])
                if len(hs) >= 2 and parts:
                    cid = int(parts[-1])
                    cidranges.append((int(hs[0], 16), int(hs[1], 16), cid))
                i += 1
        elif b"begincidchar" in s:
            i += 1
            while i < n and b"endcidchar" not in lines[i]:
                parts = lines[i].split()
                hs = hexre_all(lines[i])
                if len(hs) >= 1 and parts:
                    cid = int(parts[-1])
                    cidchars.append((int(hs[0], 16), cid))
                i += 1
        elif b"beginbfrange" in s:
            # ToUnicode-style (the Adobe-*-UCS2 CMaps): map CID -> Unicode.
            # Stored in the SAME cidrange table (the "cid" field carries the
            # first Unicode of the range; %cidrange-lookup increments across it).
            # The <lo> <hi> [<u..>] array form is skipped (rare, non-CJK).
            i += 1
            while i < n and b"endbfrange" not in lines[i]:
                if b"[" not in lines[i]:
                    hs = hexre_all(lines[i])
                    if len(hs) >= 3:
                        cidranges.append((int(hs[0], 16), int(hs[1], 16),
                                          int(hs[2][:4], 16)))
                i += 1
        elif b"beginbfchar" in s:
            i += 1
            while i < n and b"endbfchar" not in lines[i]:
                hs = hexre_all(lines[i])
                if len(hs) >= 2:
                    cidchars.append((int(hs[0], 16), int(hs[1][:4], 16)))
                i += 1
        i += 1
    return wmode, usecmap, codespace, cidranges, cidchars

def hexre_all(line):
    return [m.group(1).decode() for m in hexre.finditer(line)]

def serialize(wmode, usecmap, codespace, cidranges, cidchars):
    out = bytearray()
    out += b"FCM1"
    out += struct.pack(">B", wmode & 0xFF)
    un = usecmap.encode("latin-1")[:255]
    out += struct.pack(">B", len(un)) + un
    out += struct.pack(">H", len(codespace))
    for nb, lo, hi in codespace:
        out += struct.pack(">BII", nb, lo, hi)
    out += struct.pack(">I", len(cidranges))
    for lo, hi, cid in cidranges:
        out += struct.pack(">IIH", lo, hi, cid & 0xFFFF)
    out += struct.pack(">I", len(cidchars))
    for code, cid in cidchars:
        out += struct.pack(">IH", code, cid & 0xFFFF)
    return zlib.compress(bytes(out), 9)

def main():
    os.makedirs(os.path.dirname(BUNDLE), exist_ok=True)
    total_raw = 0
    count = 0
    entries = []          # (name, blob)
    for reg in REGS:
        d = os.path.join(SRC, reg)
        if not os.path.isdir(d):
            continue
        for name in sorted(os.listdir(d)):
            path = os.path.join(d, name)
            if not os.path.isfile(path):
                continue
            # skip UTF-8 CMaps: multi-byte-boundary variable codespaces folio
            # does not segment (UCS2/UTF16/national cover the real corpus).
            if "UTF8" in name:
                continue
            wmode, usecmap, cs, cr, cc = parse(path)
            if not cs and not usecmap:
                continue
            blob = serialize(wmode, usecmap, cs, cr, cc)
            entries.append((name, blob))
            total_raw += os.path.getsize(path)
            count += 1
    # single indexed bundle: header + directory + concatenated blobs
    header = bytearray()
    header += b"FCMB"
    header += struct.pack(">H", len(entries))
    # first pass to size the directory so we can compute absolute offsets
    dirsize = 0
    for name, blob in entries:
        dirsize += 1 + len(name.encode("latin-1")) + 4 + 4
    base = len(header) + dirsize
    directory = bytearray()
    off = base
    for name, blob in entries:
        nb = name.encode("latin-1")
        directory += struct.pack(">B", len(nb)) + nb
        directory += struct.pack(">II", off, len(blob))
        off += len(blob)
    body = b"".join(blob for _, blob in entries)
    out = bytes(header) + bytes(directory) + body
    open(BUNDLE, "wb").write(out)
    print(f"wrote {count} cmaps -> {BUNDLE}")
    print(f"raw={total_raw/1e6:.2f}MB bundle={len(out)/1e6:.2f}MB")

if __name__ == "__main__":
    main()
