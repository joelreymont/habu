---
title: compiler-insn-proof overruns its budget on this host
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T01:22:55.565449+02:00"
---

Pre-existing, proven by single-prefix-4 (2026-08-18): gate-stdlib -- proof reds TIMEOUT-UNDER-LOAD at ran=133,395ms on master 6069ed26 and 133,331ms on the lane (64ms FASTER) - reruns reproduce, so a genuine budget overrun on this machine, not load. Diagnose: what grew past the budget (the proof corpus? the checker under it?) and whether the budget derives from anything; fix the cause or re-derive the budget by the tree's ratchet method - never a bump for green.

Measured 2026-08-18 (thecut-2, macOS M-series host): standalone
`bin/hb --load test/gate-stdlib.f -- proof` reds compiler-insn-proof
TIMEOUT-UNDER-LOAD on PRISTINE MASTER - 140.7s master / 141.9s candidate against
the 120000ms nominal in test/gate-stdlib-lib.f, whose comment records 99543ms
quiescent on another host. The same slice is green inside test/run.f. So the
standalone entry cannot pass on this host on any tree: either the nominal is
wrong for this host class or the slice needs run.f's load export. A time budget
is host-relative; re-derive before retuning.
