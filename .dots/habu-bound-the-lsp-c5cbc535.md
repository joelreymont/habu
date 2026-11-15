---
title: Bound the test rows' wall deadlines by CPU
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:39:18.138408+02:00"
---

Problem: some test deadlines are wall clocks that host load alone can exhaust, so a test fails in a saturated suite and passes alone. tools/lsp-test-lib.f:739-740 CONVERSATION-MS 8 s and CHECKED-MS 60 s bound each conversation's wait; lsp-boundary reported TIMEOUT-UNDER-LOAD at load average ~90 and passed alone. tools/check-core.f:48 CHK-DEADLINE-MS 120000 bounds check.f's run and verifier children (check-core.f CHK-RUN-LATE :958): `tools/check.f -- tools/build-profile.f` under load exited 70 at 136 s where 67 was expected. A trivial `bin/hb --load tools/check.f -- ok.f` child takes 1.5 s user and 4 s wall at load 24. Acceptance: each inner deadline is bounded by CPU use or by the test's hang guard, so load alone cannot fail it; a saturated suite runs lsp-boundary to PASS. Depends: none. Claim: unassigned.

Consumers: none meets this today.
