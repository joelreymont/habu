---
title: "Profiler: a small predicate shows a third of a build's inclusive samples"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T10:52:28.704734+02:00"
---

Problem: under 'by inclusive' the profiler lists words that were not on the call path. In a warm engine build LINKER:CROSS? (src/habu/linker.f:133, a one-line predicate LOAD calls before LINKED?) shows 4,337 of 13,085 samples, between LINKED? and LOAD. A reader who does not know the code is misled.

Cause, shown by the program below: C-PROF-WALK (src/habu/prof.f:224-231) takes every cell between sp and the end of its 4 KiB block whose value lies in indexed code for a return address, and a tier-0 frame holds unwritten cells. CLOSE (src/habu/t0-arm64.f:371-374) saves x30 at entry sp - 16 and then lowers sp by the frame's slots (FRAME-CELLS x 8, rounded to 16) with a bare sub; a slot is written only when a value is spilled into it. A return address that a returned word's callee saved stays in such a slot. C-PROF-INCL-ONCE (prof.f:197-205) counts its word once per sample, and the first hit becomes the sampled word's caller edge (prof.f:204-206). Second path: prof.f:197 subtracts 4 before the lookup, so a stale cell holding a word's start address is credited to the word laid out just before it, which may never have run.

Measured at 1 ms with the program (bin/hb --load x.f): PW-P shows 0 exclusive and 1044 inclusive of 1044 samples although it returned before PW-R started, and PW-R's 489 samples carry a caller edge from PW-R-W, a word never called. With PW-MAIN-W (the slots written before the loop) PW-P is absent and the edge is PW-MAIN-W's. Disassembly: PW-A2 saves its return address into PW-P at S-32 (S = MAIN's sp); PW-R's sp is S-48 and its slot 2 is S-32, first written after the loop. Not established: that CROSS? in the build gets its samples by the first path rather than the second; which code leaves a word's start address in a slot.

```
: PW-L0 ( n -- n ) 1 + ;
: PW-A1 ( n -- n ) PW-L0 1 + ;
: PW-A2 ( n -- n ) PW-L0 2 + ;
: PW-P ( -- n ) 0 PW-A1 PW-A2 1 + ;
: PW-R ( -- n )
   0 begin PW-L0 dup 300000000 = until
   dup dup dup {: a:n b:n c:n d:n :}
   a b + c + d + ;
: PW-R-W ( -- n )
   0 0 0 0 {: a:n b:n c:n d:n :}
   0 begin PW-L0 dup 300000000 = until
   a + b + c + d + ;
: PW-MAIN ( -- )
   1000 prof-rate  0 prof-on  PW-P drop  PW-R drop  prof-off  prof-report ;
: PW-MAIN-W ( -- )
   1000 prof-rate  0 prof-on  PW-P drop  PW-R-W drop  prof-off  prof-report ;
PW-MAIN
```

Fix: the walk steps frame to frame instead of scanning. A tier-0 word that calls keeps its return address at sp + its slot bytes once the prologue has run, so the index must give each word's slot bytes and whether it calls. Not designed: the frames of primitives, tier-1 and kernel code, and a sample taken inside a prologue or epilogue. A filter that skips particular cells is a workaround. The design goes to Joel before a worker starts.

Acceptance: the program shows no row and no caller edge for a word that was not on the call path; the build profile (bin/hb --load tools/build-profile.f -- /tmp/e /tmp/p.txt /tmp/p.json on a warm out root) lists no such word; docs/debugging.md 'Limits' states any case left. Files: src/habu/prof.f, src/habu/prof-abi.f, src/habu/t0-arm64.f (the frame facts the index records), docs/debugging.md. Verify: the program's two runs; the build profile. Depends: none; habu-write-the-profiler-b7cacfe0 moves the report, not the walk. Ownership: src/habu/prof.f. Claim: unassigned; the cause hunt is done.
