---
title: "Test optimization: cut native gate wall time"
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:57:55.711568+02:00"
---

Problem: campaign only; do not dispatch this parent. The full test suite (`bin/hb --load test/run.f`) took 787 s wall on an idle host: 99 s of serial setup (15 s cold engine, 84 s private whitebox build), then a pool phase of 688 s against an 8-slot ideal of 456 s for 3647 s of test work, because the pool started tests in registry order and each waited for a free slot. The speed work has landed on master: 9e5643a1 "Order gate rows longest first after the splits", 02b52c09 "Start private whitebox build early", 0b00fee1 "Delete PTX-BARRIER!" and c8d47bef "Scale the gate's spawned per-suite walls with load"; no open child is speed work. What is missing is a run that shows the reduced wall time. The other open dots in this directory were filed here later; they are independent of wall time, keep their own acceptance and do not block closing this parent.

Acceptance: one green full suite run on current master, on an idle host, records its wall time below 787 s, and this dot closes with that time and the commit.

Files: none.

Verify: first, the full suite `bin/hb --load test/run.f` on a master tip with its wall time; the run test/AGENTS.md requires once on each landing tip can serve.

Depends: none.

Ownership: unassigned. Claim: unassigned.
