---
title: Factor bootstrap move-wide emitter
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.528705+02:00\\\"\""
closed-at: "2026-06-25T13:51:37.253787+02:00"
close-reason: "completed: factored native/bootstrap LVMOVK move-wide emitters into frame/init/count/form/chunk/emit/fallback/return helper phases; added compiled literal coverage for zero, all-ones, MOVZ/MOVK, and MOVN/MOVK forms; tools/bootstrap-codegen-test.f, bin/hb test/engine-suite.f, trust-lint, stale-status-lint, and full native gate passed; local gforth 0.7.3 recovery probe exited 69 before generation and left bin/hb checksum unchanged; commit 31f1f6a1"
---

Finding F07. Evidence: docs/factorization-review.md:35; bootstrap/cg/jit.fs:144. Root cause: EMIT-VMOVK bundles MOVZ/MOVN/MOVK selection, chunk counting, first-instruction selection, continuation emission, and all-zero/all-one fallback. Fix: factor chunk extraction/counting and first/continuation move-wide emitters. Why: instruction selection policy should not be tangled with emission loops. Validate with JIT/build tests, native fixpoint, and full native gate.
