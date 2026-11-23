---
title: "Reject a using overflow in the verifier's replay"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T19:12:11.777468+01:00"
---

Problem: src/core/checker.f CHECKER-USING dies 'using stack overflow' (EXIT-STATUS:INTERNAL), reached by the verifier: tools/verify-source.f:2414 RECORD-USING -> checker.f OPEN-USING -> CHECKER-USING, with no USE-MAX test on that path; the interpreter rejects the same overflow at src/habu/interpreter.f:1226. Acceptance: a source with more nested usings than USE-MAX is rejected by the verifier with the interpreter's rejection; CHECKER-USING holds no internal die. Depends: none. Claim: unassigned.
