---
title: Factor checked ARM64 encoders
status: closed
priority: 3
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.573846+02:00\\\"\""
closed-at: "2026-06-25T14:56:43.399008+02:00"
close-reason: "completed: factored bootstrap/cg/asm-checked.fs through shared ARM64 layout combinators; added tools/asm-checked-test.f; wired it into build-helper-fixtures; validated focused asm test, bootstrap-codegen-test, trust-lint, stale-status-lint, engine suite, focused build-helper bundle, full native gate, and recovery probe rc 69 with bin/hb sha c7a2bdac0ac2c10bfd65cff251a8ba204f727803514524520a50ad9dbd77b4bb unchanged"
---

Finding F20. Evidence: docs/factorization-review.md:48; bootstrap/cg/asm-checked.fs:7. Root cause: checked ARM64 encoders repeat instruction layout arithmetic. Fix: add checked layout combinators such as register/register, immediate, move-wide, and load/store helpers. Why: instruction encoding should be typed and factored, not repeated bit juggling. Validate with assembler/disasm tests and native gate.
