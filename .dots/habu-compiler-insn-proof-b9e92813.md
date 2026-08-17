---
title: compiler-insn-proof overruns its budget on this host
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T01:22:55.565449+02:00"
---

Pre-existing, proven by single-prefix-4 (2026-08-18): gate-stdlib -- proof reds TIMEOUT-UNDER-LOAD at ran=133,395ms on master 6069ed26 and 133,331ms on the lane (64ms FASTER) - reruns reproduce, so a genuine budget overrun on this machine, not load. Diagnose: what grew past the budget (the proof corpus? the checker under it?) and whether the budget derives from anything; fix the cause or re-derive the budget by the tree's ratchet method - never a bump for green.
