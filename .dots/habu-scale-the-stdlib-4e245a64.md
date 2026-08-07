---
title: Scale the stdlib per-suite spawn timeout with load
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T14:11:31.249848+02:00"
---

STDLIB-GATE:SUITE-TIMEOUT-MS (test/gate-stdlib-lib.f) is a FIXED 120000 constant. Every other per-suite budget in the gate scales with the measured load factor: test/run-lib.f TR-LOAD-PCT-EXPORT computes it, floors it at TR-CAL-MAX-PCT (300%) whenever a nested pool oversubscribes the box, and LOAD-PCT-DEFAULT+ exports HB_LOAD_PCT so lib/test/budget.f T-BUDGET-MS stretches with contention. The stdlib gate's spawn timeout consumes none of that, and TR-PHASE-BASE does not put HB_LOAD_PCT in a SPAWNED phase's environment at all (only the resident path sets it, in TR-PHASE-RESIDENT-SETUP).

EVIDENCE (macOS arm64, 2026-08-07, after the proof slice landed as phase 40). Quiescent, standalone: bin/hb --load ... test/gate-stdlib.f -- proof --pool-slots 2 -> compiler-insn-proof PASS at 99543ms, whole slice 137s, rc 0. Through test/run.f with a SECOND full gate running on the same box (load average 35-48): RED compiler-insn-proof kind=TIMEOUT-UNDER-LOAD ran=120145ms sat=1/2 waits=23, and a second run at load 43 gave ran=120179ms. A 21 percent stretch over the quiescent wall is exactly what the load factor exists to absorb, and the suite gets none of it: at a 100s quiescent wall it has 20 percent headroom against a constant, where a resident suite would have had 300 percent.

FIX (structural, not a bigger constant): give the spawned slices the same load input the resident ones get - add HB_LOAD_PCT to TR-PHASE-BASE's environment - and make SUITE-TIMEOUT-MS a word that scales the base budget by it, the way T-BUDGET-MS does. Falsify by running the proof slice under a synthetic HB_LOAD_PCT and checking the deadline moves. Do NOT simply raise the constant: that removes the hang detector's meaning for the 180 fast suites that share it.

BLOCKS: the proof phase cannot be green on a contended host until this lands.
