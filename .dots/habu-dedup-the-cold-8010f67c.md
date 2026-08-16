---
title: Dedup the cold-prefix and stage2 checker text
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T01:17:37.103299+02:00"
---

Found by bake-chain-20's arena root-cause (2026-08-17): the boot source arena holds TWO copies of ~1.4MB of checker/core text - the cold prefix (LCOLDPFXB) plus the stage2 source's own BF-APPEND-CHECKER-BOOT/DECL/CORE re-include. Removing the duplication drops live from ~3.4MB to ~2.0MB and buys years of headroom instead of one doubling - but it is a real redesign: the stage2 source's hide.f / BFR-CHECK-OFF dance exists BECAUSE the prefix is already loaded. Probe that dance's actual dependency set first. Rider: the fix also halves the maker.f MK-SOURCE-CAP dictionary allot back to 4MiB, restoring Linux DATA-SIZE headroom (8 of 32MiB after the bump). Depends: the SOURCE-ARENA-CAP $800000 bump landing first (its ratchet numbers are the baseline).
