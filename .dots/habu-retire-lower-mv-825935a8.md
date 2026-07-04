---
title: Retire lower-mv-device-test TRANSPOSE fan-out workaround
status: open
priority: 3
issue-type: task
created-at: "2026-07-04T23:46:14.513523+02:00"
---

maki/lower-mv-device-test.f TP still uses the multi-use fan-out (TRANSPOSE >V H H RESIDUAL-ADD) to materialize a transpose - a workaround for the mat-flag gap now fixed (dot maki-fusion-plan-59caf199, commit 8542ba9e). FP-MAT-FLAG materializes a standalone/trailing movement model-output directly, so TP can become MODEL: TP ( x:4x8 -- y ) TRANSPOSE ; and still exercise the copy kernel. Left verbatim here because the assigned task required running the device test unchanged; simplify + re-verify on zed as a focused follow-up.
