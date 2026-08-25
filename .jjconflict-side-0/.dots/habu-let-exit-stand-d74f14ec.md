---
title: Let exit stand before else
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T12:00:20.706678+02:00"
---

src/compiler/native/elaborate.f SK-ELSE and DO-ELSE refuse an `else` that follows an `exit` (E-NELAB-CTRL, test EXITELSE-CASE in test/compiler/native-elaborate.f). `if A exit else B then` is ordinary Forth: the first arm has already branched to the definition's one return block, so `else` has no block to close and the join ends up with a single predecessor - the second arm. The construction is: at `else` with EXIT-PENDING set, skip the arm's terminating branch and the join-width record, open the second arm from the `if`'s stub ordinal, and let `then` take the join width from the second arm alone; the skeleton counts no block for the `else` because `exit` already closed one. Both arms exiting leaves the join with no predecessor at all and must stay refused. The general unreachable-code case is habu-let-exit-leave-7e013b93.
