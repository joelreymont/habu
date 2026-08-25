---
title: Scope CROSS-N to the function being built
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T22:57:17.186891+02:00"
---

Cost-only, found by the quot-scope lane: CROSS-N reads whole-tape CALL-NEED, so a quotation body's own loop may carry counters even when only the ENCLOSING definition calls - the body pays crossing cost for calls outside it. Scope the need question to the function's tape range. No correctness impact measured. Files: src/compiler/native/elaborate.f. Depends: none.
