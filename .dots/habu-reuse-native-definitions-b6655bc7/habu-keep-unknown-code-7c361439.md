---
title: Keep unknown-code probe outside host cells
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T16:40:41.431353+03:00"
closed-at: "2026-10-19T16:49:34.054803+03:00"
close-reason: Moved the existing unknown fixed-CODE probe from live DIVISION-PRIM at0x2D40 to unclaimed0x2D78. Exact production child changes67/-9441 to74, empty stdout and byte-exact incompatible fixed engine layout diagnostic. Independent Astra review clear; no production change or weakened assertion.
---

Moved the existing unknown CODE probe from DATA+$2D40, the real NATIVE-HOST-CELLS:DIVISION-PRIM marker, to named unclaimed header cell $2D78 after SRCLOC:OWNEDLEN-CELL and below LOCNAMES. Checked src (including lib), tools, test and bootstrap claimants and native/recovery band maps. The exact BAD-HOST-ROW child command through tools/native-build.f on candidate 4384e857 / map 6ba4a675 exited 67 with throw -9441 before; after it exits 74, stdout is empty and stderr exactly matches the unchanged incompatible-layout diagnostic (66.60s wall, 17.17s user). No production code or entry assertions changed. Active pending integrator independent Astra review, existing full-entry verification and integration.
