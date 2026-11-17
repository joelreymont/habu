---
title: Run a str value at top level as one value
status: closed
priority: 2
issue-type: task
created-at: "2026-11-17T23:03:37.938160+01:00"
closed-at: "2026-11-18T07:29:30.766147+01:00"
---

Problem: at top level a str is not one value. On the installed hb, /tmp/jerry-strfx/a.f ('TYPED-VARIABLE PV str' then 's" abcde" >str PV !') is E-EVAL-RESIDUE 'this file leaves 1 cell', and /tmp/jerry-strfx/b.f (a definition stores the str, then top level 'PV @ str>') is E-MISMATCH '`str>` takes str; the stack holds @str.slot0'; the same fetch and store inside definitions (/tmp/jerry-str/probe/pv2-unmarked.f) print 5. The top-level row reads literals as two cells (check-walk.f TOP-READ-LIT, TOP-LIT-STEP) and counts a string as 2 cells (top-row.f:13). Acceptance: both files run at top level as in a definition (rc 0, print 5 for b.f's tail); a test fails first. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-strtop.
