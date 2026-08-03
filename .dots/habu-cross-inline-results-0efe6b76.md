---
title: Cross inline results back to cells
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T20:53:12.379902+02:00"
---

BLOCKING, destruction review of NINL. DO-INLINE (src/compiler/native/elaborate.f:2398-2412) replays a recorded callee body and runs CELL-CROSS-RUN over the arguments, but never reproduces the callee's RETURN-CROSS (elaborate.f:706-709), so a recorded body whose last operation leaves a double leaves a double on the caller's vector where the real call leaves a cell. Proven by probe: a caller that stores the inlined double result dies with E-NELAB-TYPE (-8580) when the callee is copied, compiles and runs when the identical callee is called. Acceptance depends on whether the optimisation fired, and the NINL row is one crossing short of describing the routine at its address (contradicts inline.f:62-66). Fix: run o CELL-CROSS-RUN over base after the token loop in DO-INLINE, mirroring RETURN-CROSS. Regression test: obligation-7-style case for the RESULT side (store the inlined double result) in test/compiler/native-inline.f; today obligation 7 covers arguments only.
