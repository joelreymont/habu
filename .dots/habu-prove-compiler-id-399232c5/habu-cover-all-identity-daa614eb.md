---
title: Cover all identity families in parity vectors
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T13:23:17.871896+02:00"
---

Full context: the compiler identity parity gate runs every schema vector row through PACK-SOURCE and the SOURCE-* wrappers. Eight of the nine packed families - FUN, BLOCK, OP, VALUE, TYPE, ATTR, SYMBOL, SPAN - are covered only by the positive round-trips in test/compiler/ir-id.f, and their wrapper bodies are not in the gate's frozen token-run set. So a change to BLOCK-CHECK alone passes the parity gate today. The wrappers are mechanically identical, which is why this was not caught, and also why it is cheap to close. Required result: either run every vector row through every family, or freeze all 36 wrapper bodies as exact token runs the way SERIAL-NEXT, TRY-SERIAL, CHECK-N and the others already are. Prefer running the rows - it is executable rather than textual, and matches how the rest of the gate works. Acceptance: mutating any one family wrapper - for example changing BLOCK-CHECK's bound test - fails the gate, demonstrated by mutation on at least three different families; the gate stays green unmutated; run time stays acceptable.
