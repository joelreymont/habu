---
title: fd and fs tests end their waits by a clock
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:23.043045+01:00"
closed-at: "2026-11-13T21:29:13.709618+01:00"
---

Problem: fd-test.f, fs-mutate-test.f, fs-test.f, fs-list-test.f, fs-cause-test.f, memory-test.f and arena-test.f end waits by deadlines. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wFS.
