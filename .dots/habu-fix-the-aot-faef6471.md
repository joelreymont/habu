---
title: Fix the AOT chain capture byte budget
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:24:03.744261+03:00"
---

Problem: SUITE aot-chain-capture (test/aot-chain-capture-suite.f) is red on the pinned engine fE and on the guard-free engines: the artifact-row child dies aot-file: encoded sections exceed their byte budget (src/habu/aot-file.f lines 272, 429, 466) exit 75, then aot-data-sites and the CODE-sites-size case fail. Cause not yet found: the fixed section budget is smaller than what the current window encodes, or the encoding grew. Acceptance: root cause named, the budget derived from the content or the encoding reduced, suite green on the line. Files: src/habu/aot-file.f, test/aot-chain-capture-suite.f. Verify: bin/hb --load test/aot-chain-capture-suite.f. Depends: none. Ownership: hazel line. Claim: unassigned.
