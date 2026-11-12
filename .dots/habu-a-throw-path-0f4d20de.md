---
title: "A throw path's same-type swap keeps its cells' evidence"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T15:08:45.240662+02:00"
---

Problem: the checker keeps throw-path evidence on two cells of the same type that a callee swapped before throwing: ': SWAPFD ( fd fd -- fd fd ) swap -77 throw ;' then reading the caught cells as fd loads rc 0 and runs with them swapped. This contradicts docs/forth.md 'Callee throw evidence' (:2605); test/catch-stale-suite.f covers only swaps of different types (R1, WSWAPT). Acceptance: the caught cells of a same-type swap are stale (refused when read as fd), as a different-type swap's are; a catch-stale-suite case fails before the fix. Depends: none. Claim: unassigned.
