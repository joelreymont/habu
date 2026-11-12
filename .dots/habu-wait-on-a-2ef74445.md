---
title: Call PROCESS:POLL at the hand-composed poll sites
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:42:59.667553+02:00"
---

Problem: six sites compose `1 ms ms >MS PROCESS:DEADLINE-AT PROCESS:POLL-RESTART` by hand where PROCESS:POLL ( n ms -- count ) now does it: tools/lsp-test-lib.f, test/gate-signal-test.f, test/gate-pool.f, test/capture-tree-test.f, test/check-signal-test.f, test/db/pg-kill-test.f. Acceptance: each site whose handling of a timeout and a failed poll is POLL's calls POLL; the suite passes. Depends: none. Claim: unassigned.
