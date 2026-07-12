---
title: "maki: audit *-RAW projection call sites for boundary confinement"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-12T18:41:30.701548+02:00\""
---

R3 acceptance review (merge 2634198e panel) MEDIUM carry-forward. The TRUSTED refine/raw pairs (ROWS-RAW x~20 files, COLS-RAW x~22, DIM-RAW x~15, NODE>RAW x~13, SLOT>RAW x7) are package-MAKI-private but package-visible, so shape/identity roles de-nominalize wherever lowering/scheduling/traffic/checkpoint code applies them - a rows/cols swap AFTER projection is not checker-caught. Inherited from master's own R3 pattern, not merge-caused; refine direction (n->family) has zero cross-file callers except sanctioned PLAN-GATHER-ROWS. AUDIT: for every *-RAW/NODE>RAW/SLOT>RAW cross-file call site, confirm it sits at a genuine render/hash/table-index/FFI boundary (not shape reconstruction or arithmetic that re-enters typed space); relocate or wrap any site that computes with both a raw rows and raw cols in scope; consider per-file friend narrowing if the checker grows package sub-visibility. Evidence: r3-probe battery + reviewer inventories, 2026-07-12.
