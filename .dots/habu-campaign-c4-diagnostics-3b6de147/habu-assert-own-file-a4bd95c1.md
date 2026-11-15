---
title: Assert own-file places without pinned lines
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:40:21.608745+01:00"
closed-at: "2026-11-16T04:07:36.671783+01:00"
---

Problem: test/engine-suite.f:479-488,:2270, linear-suite.f:263, cast-negative-suite.f:512 and postpone-define.f:163-217 pin line numbers of their own file, so a line inserted above breaks them (W88). Acceptance: each asserts its record's place against the line the record shows; a blank line inserted at the top of engine-suite.f leaves the engine suite passing. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
