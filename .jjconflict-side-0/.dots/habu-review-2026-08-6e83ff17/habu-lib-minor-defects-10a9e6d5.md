---
title: lib minor defects
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:25.969049+02:00"
---

Problem: lib/sort.f:650-651 HS-NODE/HS-I globals make a sorting comparator corrupt the outer sort (also hashmap.f:508, stats.f:97, render.f:536, fmt.f:32-34); lib/stats.f:114-149 FMIN/FMAX/FPERCENTILE read a[0] with no len>=1 check; lib/process.f:216-222 PROC-STATUS>OUTCOME reports a stopped status (0x7f) as 'signaled 127'; lib/fs-mutate.f:138 'dup 0 > if' is dead, ATOMIC-WRITE-FILE (202-205) uses a fixed .tmp sibling and no fsync while content-key.f:575-598 has a second unique-temp atomic writer; lib/task.f:352 masks an underflow; lib/test/outcome.f:160-175 uses '1 0 T=' as the fail idiom; test/snap.f:260-266 throws E-TBL-BOUNDS for capacity while E-TEST-CAPACITY exists; lib/ptx/cg.f:22-28 mutates the global KABI at require time; lib/content-key.f:264-265 'write drop' x2 and 609-610 catch drop; lib/map.f:411-440 'full' variant means two things and MAP-SLOT-KEY-A! stores the caller's pointer; hashmap.f:519 'splitmix-style' is one xor-shift; 14 lib tests assert diagnostics by CONTAINS?. Acceptance: each fixed with a test or refuted. Files: as listed. Verify: lib tests. Depends: none. Ownership: lib. Claim: unassigned.
