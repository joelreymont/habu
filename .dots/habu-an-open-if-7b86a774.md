---
title: An open if at ; names if in prose and ; in the packet
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:10:15.043646+02:00"
---

Problem: for `: T ( bool -- ) if ;` the prose line names `'if'` and the JSON packet's token is `;`: one refusal, two tokens. The base engine does the same; found by the control-flow reasons worker. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: prose and packet name the same token for a control structure still open at the end of a definition; which one is decided by the diagnostic contract in tools/diag-code.f (the token the refusal is placed at), and the other follows. Files: src/core/checker.f (CLOSER-REFUSE and the two `;` sites that latch MD-CF-OPEN), src/core/render.f. Verify: the case, prose and packet, on a private engine; a row in test/diag-position-test.f. Depends: habu-checker-refusals-that-3152182a landed. Ownership: the checker's diagnostics. Claim: unassigned.

More cases 2026-11-10 (bob), from the control-flow review: `: T ( -- ) drop 0 0= if ;` puts the prose at 'drop' and the packet at ';'; `: T ( n -- ) case ;` prints prose with no reason.

Case 2026-11-10 (bob), from the control-flow re-review: a do, 32 ifs, `loop`, then 32 thens. The packet refuses at `;` with "a control structure is still open at the end of the definition", not E-UNCHECKABLE at the 33rd if; the prose names `if` and gives no reason. Same tokens on the base.

Claim: agent=bob workspace=.jj-ws.noindex/habu-an-open-if-7b86a774
