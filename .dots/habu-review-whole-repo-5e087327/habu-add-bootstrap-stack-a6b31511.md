---
title: Add bootstrap stack effects
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.534993+02:00\\\"\""
closed-at: "2026-06-25T13:21:22.271908+02:00"
close-reason: "completed: bootstrap/cg/forth.fs, bootstrap/cg/regstack.fs, and bootstrap/cg/jit.fs now have definition-local stack effects; scans for definitions missing immediate comments are empty; bootstrap-codegen-test, trust-lint, and full native gate passed; gforth 0.7.3 recovery host exits 69 before touching bin/hb; commit b76f44b8"
---

Finding F09. Evidence: docs/factorization-review.md:37; bootstrap/cg/forth.fs:186, bootstrap/cg/regstack.fs:206, bootstrap/cg/jit.fs:144. Root cause: raw bootstrap emitter and token-handler definitions lack definition-local stack effects. Fix: add exact stack effects, including ( -- ) for token wordlist entries, before locals/prose comments. Why: bootstrap code is not exempt from stack-effect documentation and factoring standards. Validate with bootstrap-codegen-test and full native gate.
