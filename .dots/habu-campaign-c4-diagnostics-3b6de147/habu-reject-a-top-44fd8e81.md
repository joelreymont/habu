---
title: Reject a constant with nothing to take in the checker
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:15.610608+01:00"
closed-at: "2026-11-16T13:54:13.985854+01:00"
---

Problem: `constant x` on an empty stack passes the checker's top-level take (src/core/check-walk.f TOP-TAKE-STEP) and is caught at run time by src/habu/interpreter.f DEF-CONSTANT's `depth 0= if UNDERFLOW then`, a runtime check. Acceptance: the checker rejects it at `constant` with one E-INPUT-UNDERFLOW record, rc 70, under `hb FILE`, stdin and check.f; the runtime test is gone; a test in test/export-package.f fails first. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w232.
