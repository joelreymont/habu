---
title: Other library tests end their waits by a clock
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:23.052412+01:00"
closed-at: "2026-11-13T21:29:13.758260+01:00"
---

Problem: error-test.f, ffi-test.f, jsonl-test.f, vocabulary-test.f, test/assert-test.f and codesign-test.f end waits by deadlines. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wLIB.
