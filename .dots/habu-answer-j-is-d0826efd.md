---
title: Answer J-IS from window code
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T09:29:59.540376+02:00"
---

Rider of the pre-window ruling (0b01043c, 2026-08-12): J-IS (habu2.f:3129) emits C-DATA-ADDR-RAW for the dispatch cell straight into the is-site, so window code re-pointing a PREFIX defer would bake a pre-window cell address. Today it does not certify inside a colon body in the metabuild host (hook: non-certified definition at 'is') - protection by ACCIDENT, not rule. Decide and enforce: either window code may not re-point a prefix defer (a named refusal at J-IS when capture is active and the cell is below d0 - same classification the inliner decline uses), or the is-site records a name-keyed row like the pre-window CODE class. Probe which uses exist first. Files: src/habu/habu2.f (J-IS), src/habu/aot-capture.f. Depends: the inliner-decline implementation (0b01043c).
