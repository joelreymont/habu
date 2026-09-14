---
title: "Let a no-exit routine's memory order go unread"
status: closed
priority: 2
issue-type: task
created-at: "2026-08-13T17:32:52.411292+02:00"
closed-at: "2026-09-14T16:10:55+03:00"
close-reason: "Satisfied by 1dfebae69796: terminal memory order is accepted exactly for a reachable pure nonreturning region; native-again now requires both bare and memory-touching infinite loops to compile with a retained back edge"
---

Found by the again landing (c42f89ec): a begin...again body that neither calls nor touches memory threads no order through its loop, so the a64.dtake minted at entry is read by nothing and regalloc-verify.f ORDER-VALUE-CK (the k USES-AT 1 < rule) refuses E-A64RAV-ORDER. The correct statement, per the finding: a zero-reader order value is legal exactly when no terminator that LEAVES the routine is reachable from its defining block - REACH-FILL and RET-ORD in the same file already express reachability and leaving. This is the machine-dialect/spill owner's invariant (native-dead-path.f section 7 assigns this class there); a wrong validator rule ships miscompiles, so it was correctly NOT patched in the again lane. Population today: zero (every begin-again in the tree calls or touches memory); pinned with a live twin in test/compiler/native-again.f. Files: src/compiler/native/regalloc-verify.f. Depends: none.

Resolved on frozen source f0aed707. `TERMINAL-ORDER?` now admits the unread
token only when no reachable block returns or touches memory order, while the
ordinary exact-use checks remain in `ORDER-VALUE-CK`. The production
`NAG-BARE` definition compiles successfully and retains one back edge; its
memory-touching `NAG-MEM` control also compiles and retains one back edge.
Neither divergent body is executed.
Focused `test/compiler/native-again.f` passes with producer
`/home/joel/Work/habu/.jj-ws/cedar-closure-identity/bin/hb`, SHA256
8c1b07555a940b6ecf0ea1a42566631ef68aaef1a1200ae23e2d747a8d1fd39c.
