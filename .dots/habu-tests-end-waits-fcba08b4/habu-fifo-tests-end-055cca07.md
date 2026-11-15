---
title: FIFO tests end their waits by a clock
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:22.989476+01:00"
closed-at: "2026-11-13T21:29:13.733982+01:00"
---

Problem: TEST-FIFO:EOF-WITHIN (src/lib/test/fifo.f) has no no-deadline value; test/capture-tree-test.f and test/db/pg-kill-test.f end waits by deadlines. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wFIFO.
