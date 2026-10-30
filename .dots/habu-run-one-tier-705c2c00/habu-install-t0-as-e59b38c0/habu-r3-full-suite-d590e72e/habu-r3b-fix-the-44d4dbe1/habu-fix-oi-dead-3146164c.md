---
title: "fix oi-dead: settle the does> preflight skip"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:49.796808+02:00"
---

Problem: habu2.f CAPTURE-IMMEDIATE 'no-does' path and definers.f TOKEN-IS? skip may be unreachable (no user word can be spelled does>); runtime-regression-test.f:1769-1771 says tier 1 captures it unrun, false now. Acceptance: skip removed, candidate rebuilt, oi/rr/definer rows green -> delete it and its comments; else keep it with a case that reaches it; fix the rr comment. Workspace: .jj-ws.noindex/jerry-oidead, brief /tmp/jerry-big/fix-oidead-brief.md. Claim: jerry.
