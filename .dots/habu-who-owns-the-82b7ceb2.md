---
title: Who owns the IR context scratch limit
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T19:51:52.573037+02:00"
---

Found by the spillclose lane (2026-08-12) building the remat wall fixture: 13 constants live across a loop refuse -6644 E-IR-CTX-SCRATCH (an IR context scratch capacity), while 12 compile and 14 hit the allocator's real wall -8508 - an unrelated refusal sitting between the two sides of a straddling pair, and nobody appears to own the limit (no dot names it). Establish: what the scratch capacity is, why 13-constants-across-a-loop exhausts it, whether it is a genuine capacity (then it deserves a census row per the capacities-get-rows argument on chain-census-test-lib) or a bug; size it against the cut (does any real tree definition hit it?). The remat fixture names and holds out the -6644 case with a reference to this dot. Files: src/compiler/native (IR context), tools/chain-census*.f if it earns a row. Depends: none.
