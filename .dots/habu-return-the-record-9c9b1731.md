---
title: "Return the record from the seed gate's own lookup"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T14:31:18.703982+02:00"
---

PRIORITY 1 FOR THE BAKE ROUTE, found+priced by the widening riders lane (f85ce686): LAOTWIDGATE (habu2.f:4992-era) LINEAR-SCANS the whole dictionary for every patched site to recover the record LFIND had already located - 3.3us/site (0.45ns/record/site, measured with a proper control: same records, larger blob, 3200 fewer sites); at chain scale (14k sites x ~13.4k records) = 85ms PER BOOT, paid by every hb invocation since arm-seed - would eat ~26s of the battery's 323 boots, most of the lprot win. LFIND holds the record pointer at FIND-HMATCH and throws it away; returning it changes LFIND's register ABI across ~15 engine call sites in the hottest lookup path, AND the gate is a security boundary (TFAM 2b-v) - a caller cascade plus a design decision, not a hunk. The leaf's name-pool-index guess fixes nothing (that lookup is already O(1)). Files: src/habu/habu2.f (LFIND ABI + LAOTWIDGATE). Depends: none; gates the chain bake's per-boot cost.
