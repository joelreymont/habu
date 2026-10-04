---
title: Bound type and source text lengths at checker entries
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T13:57:08.459695+02:00"
---

Problem (lane 307 r4-namelen, c84f40d6): checker words that take type or source text read the caller's length before bounding it. CHECKER-LAYOUT-INFO (src/core/checker.f ~:5756, read ~:5760) and CHECKER-STORAGE-INFO / CHECKER-DYNAMIC-INFO (~:5851/~:5874, shared read ~:5834) set the parser cursor from the length and die rc 134 at the maximum cell; src/core/sumtype.f:271 TDECL-ARITY reads CHECKER-DEFFAMILY's arity token and its refusal prints it, neither bounded; CHECK, CHECK!, CHECK-CANDIDATE! and LOWER-CERT-HOOK:HOOK refuse rc 76 'token buffer too large' at the maximum cell and return on -1, with their pool marks unchecked. Acceptance: each refuses -1, its own text cap+1 and the maximum cell with its existing refusal (as c84f40d6 does for names with CK-NAME-SPAN?) and leaves the pool marks where they were; cases in test/name-length-test.f's shape (forked child, guard page), seen failing first. Files: src/core/checker.f, src/core/sumtype.f, test/name-length-test.f.
