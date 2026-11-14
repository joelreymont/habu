---
title: "Keep a callback's body type across a require"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T19:06:02.617417+01:00"
---

Problem (read in review, not run): src/lib/ffi-callback.f holds a callback's body type as a transient schema node (BODY-TYPE) from CALLBACK: to ;CALLBACK; the end of a file required between them ends transient nodes, so the dispatch and NAME-BODY may be typed from a dead node. The text generator it replaced had no such limit. Acceptance: a CALLBACK: declaration with a require among its clauses builds and runs, as an ffi-callback test. Depends: none. Claim: unassigned.
