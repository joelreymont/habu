---
title: Leave a literal 0 a left body takes to the run
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T21:04:34.661046+01:00"
---

Problem: src/core/checker.f ZERO-AT? judges a literal 0 by the callee's recorded marks, and a body left to the run records none, so check.f --verify-only passes ': DV ( n n -- n ) MADE drop / ; : U ( -- n ) 1 0 DV ;' (MADE rendered) that the load rejects with E-DIV-ZERO. Acceptance: such a call is W-CHECK-DEFERRED. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ck-kind2.
