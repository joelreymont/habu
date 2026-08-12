#!/usr/bin/env python3
"""protcost.py - attribute a boot's samples to the engine's mprotect stubs and
the (PROT-SPAN) guard.

Method, so the number can be re-derived:
  1. otool -tV the engine; find every `svc #0x80` whose preceding `mov x16,#0x4a`
     makes it an mprotect, and record the NEXT instruction address (the `ret`),
     which is where sample(1) lands while the thread is in the kernel.
  2. Run the engine under sample(1) at 1 kHz.
  3. Bucket samples: each mprotect stub by its own return address, the JIT'd
     region by dictionary record (dumped from the SAME process when asked).
Usage: protcost.py <engine> <program.f> [reps]
"""
import re, subprocess, sys, os, tempfile, collections

def mprotect_sites(binary):
    out = subprocess.run(["otool","-tV",binary],capture_output=True,text=True).stdout
    rows=[]
    for line in out.splitlines():
        m=re.match(r'^([0-9a-f]{16})\t(\S+)\s*(.*)$', line)
        if m: rows.append((int(m.group(1),16), m.group(2), m.group(3)))
    sites={}
    for i,(addr,mn,ops) in enumerate(rows):
        if mn=="svc" and ops.strip()=="#0x80":
            # walk back for the syscall-number move into x16
            num=None; size=None
            for j in range(i-1, max(-1,i-8), -1):
                a2,m2,o2=rows[j]
                if m2=="mov" and o2.startswith("x16,"):
                    num=o2.split("#")[-1]; break
            for j in range(i-1, max(-1,i-8), -1):
                a2,m2,o2=rows[j]
                if m2 in ("mov","movk") and o2.startswith("x1,") and "#" in o2:
                    size=o2.split("#")[-1]; break
            if num in ("0x4a","74"):
                ret = rows[i+1][0] if i+1 < len(rows) else None
                sites[ret]=size
    return sites

def sample_boot(engine, prog, out):
    p=subprocess.Popen([engine,prog],stdout=subprocess.DEVNULL,
                       stderr=subprocess.DEVNULL,stdin=subprocess.DEVNULL)
    subprocess.run(["sample",str(p.pid),"3","1","-f",out],
                   capture_output=True)
    p.wait()

def parse(out):
    txt=open(out).read()
    la=int(re.search(r'Load Address:\s+(0x[0-9a-f]+)',txt).group(1),16)
    sec=txt.split("Call graph:")[1].split("Total number in stack")[0]
    hits=collections.Counter(); tot=0
    for line in sec.splitlines():
        m=re.match(r'\s+(\d+) \?\?\?\s+\(in (.+?)\)\s+(?:load address \S+ \+ (0x[0-9a-f]+)\s+)?\[(0x[0-9a-f]+)\]', line)
        if not m: continue
        n=int(m.group(1)); addr=int(m.group(4),16); tot+=n
        hits[addr-la]+=n
    return la,hits,tot

if __name__=="__main__":
    engine,prog=sys.argv[1],sys.argv[2]
    reps=int(sys.argv[3]) if len(sys.argv)>3 else 5
    # sample addresses are header-relative; otool addresses are vmaddr with a
    # 0x100000000 image base, so subtract it to compare with sample offsets.
    sites={a-0x100000000:s for a,s in mprotect_sites(engine).items()}
    print("mprotect stubs (offset -> size operand):",
          {hex(k):v for k,v in sorted(sites.items())})
    agg=collections.Counter(); total=0
    with tempfile.TemporaryDirectory() as td:
        for i in range(reps):
            f=os.path.join(td,"s%d.txt"%i)
            sample_boot(engine,prog,f)
            la,hits,tot=parse(f)
            agg.update(hits); total+=tot
    print("samples over %d boots: %d (~%d ms/boot)"%(reps,total,total/reps))
    mp=0
    for off,size in sorted(sites.items()):
        n=agg.get(off,0); mp+=n
        print("  mprotect size=%s at +0x%x: %d samples = %.1f%% (%.1f ms/boot)"
              %(size,off,n,100*n/total,n/reps))
    print("  mprotect TOTAL: %d = %.1f%% (%.1f ms/boot)"%(mp,100*mp/total,mp/reps))

def guard_extent(binary):
    """(PROT-SPAN) is emitted immediately after the LPROTREC mprotect stub's ret
    (EMIT-PROT: LPROT body, LPROTREC body, then EMIT-PROT-SPAN) and ends at its
    own ret. Derive both ends from the disassembly so the range survives a
    rebuild instead of being pinned to a literal offset."""
    out = subprocess.run(["otool","-tV",binary],capture_output=True,text=True).stdout
    rows=[]
    for line in out.splitlines():
        m=re.match(r'^([0-9a-f]{16})\t(\S+)\s*(.*)$', line)
        if m: rows.append((int(m.group(1),16), m.group(2), m.group(3)))
    stubs=sorted(mprotect_sites(binary))
    start=stubs[-1]+4                      # instruction after the narrow stub's ret
    for a,mn,_ in rows:
        if a>=start and mn=="ret":
            return start-0x100000000, a-0x100000000+4
    return None
