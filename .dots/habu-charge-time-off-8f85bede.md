---
title: Charge time off the CPU to no word
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T15:49:34.570423+01:00"
---

Problem: the sampler arms ITIMER_REAL (src/habu/prof.f:897) and weighs each sample by the wall time since the last (C-PROF-WEIGHT :703-714), so time the process spends descheduled or in a page fault is charged to the pc that resumes. Under load the profile of the same work grows on whatever word runs next: Maki's import plan put 1838 ms on `!` at load 32-39 (17.3 s run) against 838 samples at load 14 (10.1 s run) (Maki hunt habprof, 2026-11-17, /tmp/tim-habprof/plan-faults.out, store-faults.out). Acceptance: a CPU-bound workload profiled at load 30 gives each word's share within run-to-run spread of its share at rest, and time off the CPU is reported apart from every word. Depends: none. Claim: unassigned.
