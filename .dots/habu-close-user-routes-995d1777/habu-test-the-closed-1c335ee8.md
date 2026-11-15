---
title: Test the closed routes under both loaders
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T19:39:41.057670+01:00"
---

Problem: the dot's programs and the census probes have no test, and some tests call the writers directly. Acceptance: test/engine-private-test.f rejects each program as undefined under --load and tools/check.f; a test that called a private writer states its case as source or is deleted (test/AGENTS.md hand-built state). Depends: the writers dot, the scope dot. Claim: unassigned.
