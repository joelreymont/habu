---
title: Judge the compile floor by the fastest run
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-30T23:11:02.319841+02:00\""
closed-at: "2026-09-30T23:39:19.630744+02:00"
close-reason: "kvolvnwq: the gate judges the least B run and the fastest single definition (least-* floor fields); green 6/6 under induced load where the median gate was red 4/6; a budget below this host's least stays red; Fable ACCEPT"
---

Problem: test/compile-floor-gate.f compares the median of tools/tier-bench.f's three timed runs (field 3 of each B line, test/compile-floor-gate.f:86) with its budget. On a shared machine two of three runs can be slowed by other processes: the r4-reap lane's full native suite (2026-09-30, other agents' gates running) went red on tier-1 B lines 15654 5378 1936, median 5378 against T1-LINES-BUDGET 3500, while the fastest run, 1936, sits in the pinned ten-run range 1911..1946. The row already runs in the drained SEQ group, so the pool is not the neighbour. Interference only adds time and a regression slows every run, so the least run is the estimate of cost that a doubled cost still exceeds. Acceptance: the gate judges the least of the runs for every B budget (and compile-floor.f's floor lines by the same rule if they carry repeated runs); tier-bench.f's median stays for the numbers people quote; the budgets and their comment are restated against the least run where that changes their basis; shown on this machine: the row passes under induced load that makes the median check fail, and a budget below the pinned range still fails. Files: test/compile-floor-gate.f, tools/compile-floor.f if it applies. Verify: the gate row alone and under load. Depends: none. Ownership: the compile floor gate's statistic.
