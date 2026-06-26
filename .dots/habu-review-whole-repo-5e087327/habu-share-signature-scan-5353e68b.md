---
title: Share signature scan helpers
status: closed
priority: 3
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.563133+02:00\\\"\""
closed-at: "2026-06-25T16:37:03.437103+02:00"
close-reason: "completed: factored shared native/recovery signature scanner helpers; added tools/signature-scan-emitter-test.f; passed focused signature scanner test, bootstrap-codegen-test, build-helper bundle, dictionary/diagnostics/AOT-negative phases, trust-lint, stale-status-lint, filemap-lint, shadow-lint, engine suite, full native gate, and recovery-host probe rc 69 with unchanged bin/hb checksum a81b96e5501123cc0a42f8cc6beb9442ac78851b61fdb50ac488eb8bbf373879"
---

Finding F17. Evidence: docs/factorization-review.md:45; src/habu/habu2.f:1624 and src/habu/habu2.f:1021. Root cause: optional signature scanning in EM-INTERPRET-COLON resembles C-PARSE-TRUST-SIG. Fix: factor shared signature scanner/capture helper with required and optional entry points. Why: signature parsing should have one implementation surface. Validate with checker/trust signature tests and full native gate.
