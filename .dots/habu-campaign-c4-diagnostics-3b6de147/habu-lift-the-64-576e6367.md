---
title: Lift the 64-entry caps of at-exit and fork-child
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:38.789756+01:00"
closed-at: "2026-11-16T13:54:13.956783+01:00"
---

Problem: src/lib/at-exit.f:81 and src/lib/fork-child.f:32 throw E-LAYOUT-BOUNDS bare when a program registers a 65th entry, ending the run with an uncaught throw. Acceptance: no fixed cap: a program registers as many as it needs, or the limit is a checked one with a record. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w212
