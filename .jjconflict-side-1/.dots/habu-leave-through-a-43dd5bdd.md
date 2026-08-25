---
title: Leave through a tail call from inside a branch
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T14:01:24.312925+02:00"
---

src/compiler/native/elaborate.f TAIL-SCAN only calls a definition a tail caller when its body has no control flow at all: no 'exit' (EXIT-USED) and one block (NB). A definition whose final call is the last operation of ONE ARM - the guarded shape a tail-recursive Forth word really has, and the shape src/core/checker.f's early-exit ladders are full of - is a real tail call this lane does not take. Taking it means deciding the question per RETURNING BLOCK rather than per definition, which changes what src/compiler/native/select.f's TAIL-AT? asks (it currently reads the one block's last two operations) and what src/compiler/native/regalloc-verify.f's VTAIL-CK measures (it currently allows a tail branch only as the terminator of the single block RET-ORD found). Self tail recursion arrives with it: a guarded self-call in tail position is a loop the machine can make with a branch to block zero, which the dialect's a64.tailcall cannot name because its target is an address and not a block.
