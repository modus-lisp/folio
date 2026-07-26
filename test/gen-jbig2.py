#!/usr/bin/env python3
# Build PDF fixtures from SerenityOS .jbig2 files (strip file header -> PDF
# embedded organisation).  Can split a symbol dictionary into a /JBIG2Globals
# stream to exercise the globals path.
import struct, sys, os

def u32(d,p): return struct.unpack('>I', d[p:p+4])[0]

def parse_segments(d, start):
    """Return list of (segnum, type, page_assoc, hdr_start, data_start, data_end)."""
    segs=[]; p=start; n=len(d)
    while p < n:
        h0=p
        number=u32(d,p); flags=d[p+4]; stype=flags&0x3f
        pa4=(flags>>6)&1
        refflags=d[p+5]; refcount=refflags>>5; pos=p+6
        if refcount==7:
            refcount=u32(d,pos-1)&0x1fffffff; pos+=3
            pos += (refcount+7)>>3
        refsize = 1 if number<=256 else (2 if number<=65536 else 4)
        pos += refcount*refsize
        if pa4: pageassoc=u32(d,pos); pos+=4
        else: pageassoc=d[pos]; pos+=1
        length=u32(d,pos); pos+=4
        data_start=pos; data_end=pos+length
        segs.append((number,stype,pageassoc,h0,data_start,data_end))
        p=data_end
        if stype==51: break
    return segs

def strip_header(d):
    assert d[:4]==b'\x97JB2', "not a jbig2 file"
    flags=d[8]; p=9
    if not (flags&2): p+=4  # page count present
    return p  # start of first segment

def reserialize(d, seglist):
    """Concatenate the raw header+data bytes of the given segments in order."""
    out=bytearray()
    for (num,st,pa,h0,ds,de) in seglist:
        out += d[h0:de]
    return bytes(out)

def build_pdf(path, w, h, jbig2, globals_bytes=None):
    objs=[]
    def add(b): objs.append(b); return len(objs)
    # image dict
    parms=b""
    gref=None
    if globals_bytes is not None:
        gnum=None  # fill later
    # We build objects manually with fixed numbering.
    # 1 catalog, 2 pages, 3 page, 4 image xobject, [5 globals], 6 content
    n_globals = 1 if globals_bytes is not None else 0
    img_num=4
    globals_num=5 if globals_bytes is not None else None
    content_num=(6 if globals_bytes is not None else 5)
    total=content_num
    pieces={}
    pieces[1]=b"<< /Type /Catalog /Pages 2 0 R >>"
    pieces[2]=b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
    pieces[3]=(("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 %d %d] "
                "/Resources << /XObject << /Im0 4 0 R >> >> /Contents %d 0 R >>")
               % (w, h, content_num)).encode()
    dp = b""
    if globals_bytes is not None:
        dp = (b" /DecodeParms << /JBIG2Globals %d 0 R >>" % globals_num)
    imgdict=(("<< /Type /XObject /Subtype /Image /Width %d /Height %d "
              "/BitsPerComponent 1 /ColorSpace /DeviceGray /Filter /JBIG2Decode")
             % (w,h)).encode() + dp + (b" /Length %d >>" % len(jbig2))
    pieces[img_num]=imgdict + b"\nstream\n" + jbig2 + b"\nendstream"
    if globals_bytes is not None:
        pieces[globals_num]=(b"<< /Length %d >>\nstream\n"%len(globals_bytes))+globals_bytes+b"\nendstream"
    content=(("q %d 0 0 %d 0 0 cm /Im0 Do Q") % (w,h)).encode()
    pieces[content_num]=(b"<< /Length %d >>\nstream\n"%len(content))+content+b"\nendstream"

    out=bytearray(b"%PDF-1.5\n")
    offsets={}
    for i in range(1,total+1):
        offsets[i]=len(out)
        out += (b"%d 0 obj\n"%i)+pieces[i]+b"\nendobj\n"
    xref_pos=len(out)
    out += (b"xref\n0 %d\n"%(total+1))
    out += b"0000000000 65535 f \n"
    for i in range(1,total+1):
        out += (b"%010d 00000 n \n"%offsets[i])
    out += (b"trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF"%(total+1,xref_pos))
    open(path,'wb').write(out)

def page_dims(d, seg_start):
    for (num,st,pa,h0,ds,de) in parse_segments(d, seg_start):
        if st==48:  # page info
            return u32(d,ds), u32(d,ds+4)
    return None

if __name__=='__main__':
    corpus=sys.argv[1]; outdir=sys.argv[2]
    # 1) generic
    d=open(os.path.join(corpus,'bitmap.jbig2'),'rb').read()
    s=strip_header(d); w,h=page_dims(d,s)
    build_pdf(os.path.join(outdir,'jbig2-generic.pdf'), w, h, d[s:])
    print('generic %dx%d'%(w,h))
    # 2) symbol/text (single stream)
    d=open(os.path.join(corpus,'bitmap-symbol.jbig2'),'rb').read()
    s=strip_header(d); w,h=page_dims(d,s)
    build_pdf(os.path.join(outdir,'jbig2-symbol.pdf'), w, h, d[s:])
    print('symbol %dx%d'%(w,h))
    # 3) globals: symbol dictionary in /JBIG2Globals, page-info+text in image stream
    segs=parse_segments(d,s)
    gsegs=[x for x in segs if x[1]==0]           # symbol dictionaries
    isegs=[x for x in segs if x[1]!=0 and x[1]!=51]  # rest (drop EOF)
    gbytes=reserialize(d,gsegs)
    ibytes=reserialize(d,isegs)
    build_pdf(os.path.join(outdir,'jbig2-globals.pdf'), w, h, ibytes, globals_bytes=gbytes)
    print('globals: %d global segs, %d image segs'%(len(gsegs),len(isegs)))
    for (num,st,pa,h0,ds,de) in segs:
        print('  seg#%d type=%d page=%d len=%d'%(num,st,pa,de-ds))
