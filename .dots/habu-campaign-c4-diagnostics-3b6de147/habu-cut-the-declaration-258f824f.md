---
title: "Cut the declaration layer's unread events and checks"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T16:36:01.288434+01:00"
closed-at: "2026-11-16T04:07:36.535056+01:00"
---

Problem: after 0d896f36, src/core/decl-event.f DEV-ROW dies on an id range its callers hold, DEV-K-DECL and DEV-K-ARITY are written and never read, and the public event words are one-line aliases of private bodies; src/core/structure-make.f keeps E-SM-FAM (its help says no declaration reaches it), SM-REQUIRE-READABLE and SM-REQUIRE-UNGENERATED, run-time checks of engine invariants. Acceptance: each check no program's declaration reaches is deleted with its code; the declaration suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
