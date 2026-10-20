---
title: Preserve evaluate after checker reset
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T13:02:17.915838+03:00"
closed-at: "2026-10-19T13:10:03.034731+03:00"
close-reason: RESET-SOURCE intentionally retires prior helper effects. The codec now compiles and runs a fresh checked client against imported facts, verifying4104 without weakening reset or evaluate. Matching native codec E2E and independent Astra review pass.
---

Matching native checker-unit-codec E2E fails at CLIENT after production OWNER-RESET:SOURCE. Minimal reproduction: require src/lib/test.f and test/checker-owner-reset.f, reset, then s"4096 8 +" TEST-EVAL:N. It rejects nested evaluate with70 while direct evaluate after reset and TEST-EVAL:N without reset both return4104. Reduce and fix the checker reset/evaluation boundary without weakening typed top rules; existing codec E2E and C2 consumer must pass.
