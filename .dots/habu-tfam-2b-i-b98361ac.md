---
title: "TFAM 2b-i: boot latch + raw-write protection (land together)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T02:07:47.900971+02:00"
---

Sealing slice 1 of habu-tfam-2b-sealed-1b77662c. The friend latch is only sound if user source cannot forge it: today user stdin can do '5 CHECKER-PACKAGE-MODE !' and 'data-base <off> + !' (proven live). So the latch cell and checker-internal state must be write-protected FROM user source in the same change: census cat-3 raw writes (!, c!, +!, atomics, here/allot/,/c,, patch32) + checker-variable exposure + data-base/dbase@ leaked-pointer provenance. Latch seal chokepoint (validated): appended SEAL-FRIEND token in the cold-prefix generator at EMIT-COLD-PREFIX-SHARED/LCOLDPFX end (habu2.f ~801-806 after PFX-PROVIDE-FILES) and C-SOURCE-BAKED (~766); friend ON across PFX-LOAD-BASE-FILES (habu2.f 450-472). No per-file origin signal exists in include.f - the boot-latch-token approach is required. Design the protection mechanism first (see design scout artifact when it lands). Depends: TFAM 4 merge (checker.f serialization).
