---
title: Reject a stale caught row as a stale read
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T14:03:42.786481+01:00"
---

Problem: `: T3 ( R [ R -- R ] -- R n ) catch ;` is rejected E-MISMATCH, not E-STALE-READ: U-ROW (src/core/checker.f:5446) fails R against stale<R> in its occurs branch without STALE-NOTE, so no MD-STALE-READ is latched (hunt: /tmp/carl-mmtxt-report.md). Acceptance: T3 is E-STALE-READ with a row form of its reason, label and help (return the cells where the code is 0); a catch-stale case shows it. Depends: none. Open (review of syvpyrwo): a step that passes the declared row's stale wrapper on, `: M2 ( R [ R -- R ] -- R ) dup >r catch drop r> TAKE ;` with `: TAKE ( R [ R -- R ] -- R ) execute ;`, still gets the cell form ('stale cell: this reads a cell', help 'bind the value to a local'): STALE-ROW-READ? (src/core/render.f:1935) takes the row form only at the end check, and the self-stale arms (src/core/checker.f:5462, :5470) record the same stale row as the cell-take arm (:5475); they need their own mark so a step names its token ('stale row: `execute` takes cells `catch` left stale'). Claim: unassigned (its commit is on the parked stack lmrmpkys..xvroprym above tqsnwlwz, workspace .jj-ws.noindex/carl-infname).
