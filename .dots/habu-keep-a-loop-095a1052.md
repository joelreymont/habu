---
title: Keep a loop-carried quotation as general as the body used it
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T10:52:44.007484+01:00"
---

Problem: src/core/checker.f CF-UNTIL, CF-AGAIN, CF-REPEAT, CF-LOOP and CF-LEAVE join a loop's back or leave edge with its entry row through SUNI-JOIN once CF-CARRIED= has compared lineage and proofs only, so the join binds the row tails of a carried quotation cell the body already applied at a fresh instance (QAPP-FRESH?). ': T ( n -- ) [: ;] swap 0 ?do dup execute drop [: swap ;] loop drop ;' is accepted and '2 T' underflows at run time on master 89914723; the begin/while/repeat form too. Acceptance: those programs and the until and leave forms are rejected at the back edge after the narrowing (`loop` for the leave form); a loop that carries one quotation type around is still accepted. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-anyq.
