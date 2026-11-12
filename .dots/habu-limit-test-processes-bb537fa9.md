---
title: Limit test processes across concurrent runs
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T13:06:08.562948+02:00"
---

Problem: each test run takes a fixed number of process slots with no view of other runs on the machine: Habu's suite pool takes 8 on macOS (test/gate-pool.f:52 GT-POOL-MACOS-DEFAULT) and each row may spawn its own tree; Tender's corpus pass takes 4 (tender src/cli.f:26 DEFAULT-JOBS; 3 in test/corpus.f:21). Nine lanes running the Tender corpus suite beside two Habu suite runs put 111 hb processes on 12 cores, load 211: an engine build needing 17.3 s of CPU took 162.7 s of wall and a suite row waited 456 s on 111 s of CPU. Acceptance: N suite runs started together on one machine keep the count of running test processes at one machine-wide limit, each run waiting for a slot instead of oversubscribing; a lone run is as fast as now; a run killed mid-way frees its slots. Design first: where the limit lives so Tender's pool can use it too. Depends: none. Claim: unassigned.
