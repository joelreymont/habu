---
title: Dictionary allot past ~32MB silently corrupts engine
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-19T20:17:11.407728+02:00\""
---

Discovered by the split-K lane 2026-07-19 (measured, not hypothesized): cumulative dictionary allot beyond ~32 MB silently corrupts the engine - no error, no bound check, wrong behavior downstream. The lane worked around it by moving mma-gemm-check's seven large host buffers to heap allocation (MGC-MAX 1024 proof needed ~28 MB of buffers). Silent corruption is a checker-integrity violation: the dictionary region must fail closed at its true capacity with a named error (E-DICT-FULL or existing equivalent) before any write lands outside the region. Do: (1) root-cause - find the dictionary region's actual size/limit and what allot does past it (overrun into adjacent mapping? wraparound?); (2) add the fail-closed bound at the allot sink (checker + native paths); (3) red-first regression allocating past the bound -> named error, plus a boundary case just under; (4) audit other large-allot users (tools/, test fixtures) for proximity to the bound. Territory: src/core or src/habu allot path + a regression; engine change -> CODELEN rows same-commit.

Claim: agent=allotbug workspace=.jj-ws/allotbug machine=spark
