---
title: "Retire a part's word with undefine inside its package"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T13:32:02.060472+02:00"
---

Problem (from tim via Maki, 2026-11-03; reproduced by carl on the installed hb, Habu 09565d43): inside a package, undefine resolves its target only at top level (src/habu/xref.f:576-581 UNDEFINE-NAME: XREF-FIND-TARGET-INDEX), while the checker (CHECKER-UNDEFINE) retires the part's symbol, so the engine and the checker retire different records. Probe u2.f: 'package QV public : HELLO ( -- n ) 2 ; ;package  package QU package QV public : HELLO ( -- n ) 1 ; ;package undefine QV:HELLO : T ( -- n ) QV:HELLO ; ;package  QV:HELLO . cr' dies E-UNDEFINED at T's QV:HELLO, rc 70 (the top-level QV:HELLO was retired). Probe u1.f: 'package QU package QV public : HELLO ( -- n ) 1 ; ;package undefine QV:HELLO ;package ." survived" cr' prints 'undefine: word not found', rc 70 (tim: also 'undefine B-P:MAKE' inside A). Acceptance: undefine resolves its path in the loader's current scope as name lookup does, and the engine and the checker retire the same record; u1 prints survived with rc 0; in u2 the part's HELLO is retired and the top-level QV:HELLO still prints 2; both probes become cases in the undefine/package test. Files: src/habu/xref.f (UNDEFINE-NAME, XREF-FIND-TARGET-INDEX), the checker's CHECKER-UNDEFINE resolution if it differs. Verify: the two probes through the real load path, the package and undefine tests. Depends: none. Ownership: unassigned. Claim: unassigned.

Consumers: Maki uses no undefine today.
