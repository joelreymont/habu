---
title: Check successors stay inside their function
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:39:33.642974+02:00"
---

Full context: MODEL GAP from agent irverify 2026-07-30 (dot habu-verify-frozen-compiler-224d78ad, commit 015d64f7). The frozen-IR verifier catches a cross-function OPERAND as E-IR-VERIFY-SCOPE, but nothing checks that a terminator's SUCCESSOR block belongs to the same function as the operation - a branch into another function's block passes the existence and kind checks and freezes successfully. src/compiler/ir/verify.f owns the fix: the successor walk already resolves each successor block; compare its owning function (IR-FUN window containment, the same parent-as-window authority fun.f uses) against the terminator's function and refuse with a named code from the -8080..-8099 block. Hostile fixture: two functions, a branch from one into the other's block, must produce the named diagnostic; mutation deleting the compare must red exactly that fixture.
