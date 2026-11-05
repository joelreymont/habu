---
title: "A throw path's same-type swap keeps its cells' evidence"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T15:08:45.240662+02:00"
---

Problem: the checker keeps throw-path evidence on two cells of the same type that a callee swapped before throwing: ': SWAPFD ( fd fd -- fd fd ) swap -77 throw ;' then reading the caught cells as fd loads rc 0 and runs with them swapped (probes /tmp/carl-signal-rev/probe/stale-g.f, stale-h.f, stale-i.f). This contradicts docs/forth.md 'Callee throw evidence'; test/catch-stale-suite.f covers only swaps of different types (R1, WSWAPT). Found by the signal review (ba1b7627). Acceptance: the caught cells of a same-type swap are stale (refused when read as fd), as a different-type swap's are; a catch-stale-suite case fails before the fix. Files: src/core/checker.f, test/catch-stale-suite.f. Verify: bin/hb --load test/run.f -- catch-stale; full suite (checker). Depends: none. Ownership: unassigned. Claim: unassigned.
