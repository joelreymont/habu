---
title: Reconcile the locals search direction between checker and engine
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T19:06:58.680066+02:00"
---

PRIORITY 1 - CERTIFIED MISCOMPILE, found by the locals-scope lane (2faa3d7a) while probing scoping. The checker's LOC-REF? (src/core/checker.f:8598) searches DOWNWARD from #LOC - innermost binding wins, standard lexical scoping per docs/forth.md. The engine's EMIT-LOC-FIND (src/habu/habu2.f:1602-1618) initialises to 0 and counts UP - outermost wins. They disagree whenever one name is bound twice in scope, and the disagreement CERTIFIES: : SY ( n -- n ) {: v:n :} v 0 > if 1 0= {: v:bool :} v if 1 else 2 then else 0 then ; then 5 SY . answers 1 (engine binds outer v:n, true) where the checker certified the reference against the inner v:bool (false, so 2). rc=0 - the checker certifies a program whose runtime meaning the engine disputes. Work: FIRST measure the tree-wide population of duplicate-live-name bindings; then rule - innermost wins (fix EMIT-LOC-FIND to search downward, matching the checker and the docs), or duplicate live names become a hard checker reject (making the disagreement unreachable). Either way a regression differential pins SY. The native chain refuses the shape meanwhile with the measured reason (see the 2faa3d7a landing). Files: src/habu/habu2.f, src/core/checker.f, docs/forth.md. Depends: none.
