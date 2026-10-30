---
title: "fix oi-dead: settle the does> preflight skip"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:49.796808+02:00"
closed-at: "2026-10-31T12:11:58.224163+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510, two generations converge, edit matrix F53/F105/F156 only, as master"
---

Problem: habu2.f CAPTURE-IMMEDIATE 'no-does' path and definers.f TOKEN-IS? skip may be unreachable (no user word can be spelled does>); runtime-regression-test.f:1769-1771 says tier 1 captures it unrun, false now. Acceptance: skip removed, candidate rebuilt, oi/rr/definer rows green -> delete it and its comments; else keep it with a case that reaches it; fix the rr comment. Workspace: .jj-ws.noindex/jerry-oidead, brief /tmp/jerry-big/fix-oidead-brief.md. Claim: jerry.

Worker launched 2026-10-30 23:00 in .jj-ws.noindex/jerry-oidead on lqwprrlr.

Measured: the does> immediate skip is live in both CAPTURE-IMMEDIATE and DEF-IMMEDIATE (alias-record can name an immediate does>; outer-interpret DOES-CALLED fails without either twin). Commit vktznxps in jerry-oidead corrects the two test comments only; awaits the fix-batch review.
