---
title: Factor mirrored number parser
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.525459+02:00\\\"\""
closed-at: "2026-06-25T13:38:12.891053+02:00"
close-reason: "completed: factored native/bootstrap EMIT-NUM into sign/base/dot/digit/int/frac/finish helpers; added hex, negative hex, and negative float engine-suite coverage; tools/bootstrap-codegen-test.f, bin/hb test/engine-suite.f, focused native engine gate, trust-lint, and full native gate passed; recovery launcher exited 69 on local gforth 0.7.3 locals probe without changing bin/hb; commit e0f933b6"
---

Finding F06. Evidence: docs/factorization-review.md:34; src/habu/habu1.f:1333 and bootstrap/cg/forth.fs:783. Root cause: native and bootstrap EMIT-NUM both bundle sign/base parsing, digit classification, integer and fraction accumulation, float conversion, and return shape. Fix: factor prefix parsing, digit step, fraction step, integer finish, float finish, and fail return in both mirrors in one change. Why: mirrored codegen refactors must land twice to avoid bootstrap/native drift. Validate with numeric parser coverage, bootstrap-codegen-test, native fixpoint, and full native gate.
