---
title: "check.f throws FSM:E-SYNTAX on src/lib/net/model.f"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T11:31:19.734325+02:00"
---

Problem: 'bin/hb --load tools/check.f -- src/lib/net/model.f </dev/null' in the installed tree dies with 'uncaught throw code -9660' (FSM:E-SYNTAX, src/lib/fsm.f:22), rc 67, on master 7eb193e5 and 17fa7af8; --verify-only ends with two W-CHECK-DEFERRED packets and no throw, so the run stage throws; check.f on fsm.f and json.f is clean; 'bin/hb --load src/lib/net/model-test.f' gives 18659 checks ok. Not known: which of model.f's three FSM:DEFINE tables the run stage refuses, or why it reads them differently from a plain load. Reported by phil 2026-11-10; the reproducer is the first command. On the installed engine and master 204743ce that command is rc 0 with no output, in the installed tree and in a workspace of master. Acceptance: check.f on model.f ends without an uncaught throw, with the cause fixed in the layer that is wrong (check.f's run stage, fsm.f or model.f). Files: tools/check.f and what the cause names. Verify: the reproducer. Depends: none; waits behind incremental builds by Joel's words of 2026-11-10. Ownership: carl. Claim: unassigned.
