---
title: Unify bootstrap prefix file list
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.531848+02:00\\\"\""
closed-at: "2026-06-25T14:17:42.048262+02:00"
close-reason: "completed: unified native/bootstrap prefix path and load emission through PFX-FILES; fixed RCA-found dropped BL,/ZBYTES, tokens; trust-lint, bootstrap-codegen-test, engine-suite, focused build-helper-fixtures, full native gate passed; gforth 0.7.3 recovery probe exited 69 and preserved bin/hb sha256 d2d79b59c70a4de0d160b886ededde6941a92feb29bef3b105d26800b1d3793b; commit 63e5d2a56f4ac08b0dd93296df3f84c0719b59b7"
---

Finding F08. Evidence: docs/factorization-review.md:36; bootstrap/cg/forth.fs:1329 and bootstrap/cg/forth.fs:1055. Root cause: prefix file path bytes and host load sequence are manually mirrored. Fix: add a tiny prefix-file table/DSL that emits both path labels and load sequence; split Linux/macOS target file selection into focused helpers. Why: source load order must have a single source of truth. Validate with bootstrap-codegen-test, bootstrap recovery path if supported, native fixpoint, and full native gate.
