---
title: Run the tier-0 sets of compile-floor on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.214175+02:00"
---

Problem: tools/compile-floor.f skips the tier-0 sets on x86-64 although src/habu/t0-x64.f exists (prune finding, 2026-10-30). Acceptance: the tier-0 sets run on x86-64, or the skip states the T0 gap it waits on beside the code. Files: tools/compile-floor.f. Verify: compile-floor on pulsar-ua-106 reports tier-0 rows. Depends: none. Ownership: tools/compile-floor.f. Claim: unassigned.
