---
title: Reject a top-level tick of an internal word statically
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T11:00:21.006387+01:00"
---

Problem: a top-level `' def-open` passes `tools/check.f -- --verify-only` (rc 0, no output); only the interpreter's run-time gate rejects it under --load (src/habu/interpreter.f XT-GATE and INTERNAL, :556-570, E-UNSAFE 'is an internal engine word'), while a body's `['] def-open` is rejected by the checker (E-CAP-TRUSTED). Acceptance: --verify-only rejects the top-level tick with the record --load prints, and XT-GATE is deleted unless a program reaches it. Depends: none. Claim: unassigned.
