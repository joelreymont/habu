---
title: Restore native regalloc fixture origin
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T09:30:50.508245+03:00"
closed-at: "2026-10-16T09:31:55.703142+03:00"
close-reason: "Added required origin -1 to shared A64IR move-wide and HIR constant fixture helpers. Native regalloc suite on engine 08e1ccc9 passes exit 0 (test: ok, real 0.44s)."
---

At frozen source 0248da8c with native engine 08e1ccc9, bin/hb --load test/compiler/native-regalloc.f exits 67, throw -8095 in DECL-SPILL-BODY at M-FREEZE. A64IR MOVE-ATTRS requires KEY-ORIGIN; hand-built M-MOVE-ATTRS supplies only IMM, SHIFT, ADDR. Add the plain literal origin to the shared test helper; rerun the standalone focused suite. No production verifier change.
