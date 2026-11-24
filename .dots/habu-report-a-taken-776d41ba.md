---
title: "Report a taken FUNCTION: name as the load does"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T18:08:38.962907+01:00"
---

Problem: PROCESS-SYMBOLS, : GP ( -- n ) 1 ; then FUNCTION: GP getpid ( -- n ) left unclosed: the load's only record is E-UNCLOSED, the check's first record is E-DUPLICATE-DEFINITION at the name. Acceptance: the check's records equal the load's for this program; a case in test/ffi-callback-verify.f or the FUNCTION: verify test holds it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/carl-taken.
