---
title: Make profiler sample totals exact
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-19T20:15:02.873821+02:00\""
---

Frozen review f7ed6085, major profiler correctness defect. src/habu/prof.f:52 increments PROF-TOT and line 53 branches to dump-and-exit as soon as total reaches PROF-LIM, before lines 54-64 attribute the interrupted PC to a word or PROF-OTHER. Reproduction on the production stdin path: define a long PROF-BUSY loop, run 1 prof-on PROF-BUSY, and the process exits 99 with empty output; limit 2 prints PROF-BUSY 1. Thus every automatic report claims N samples but its buckets contain N-1, and limit 1 records none. Fix: attribute every delivered sample first, increment the total exactly once, then test the limit and dump; preserve signal-return behavior below the limit. Acceptance: limits 1, 2, and a larger deterministic run satisfy sum(word counters)+other=PROF-TOT=limit; limit 1 names the interrupted bucket; manual prof-report preserves the same invariant; bootstrap mirror, macOS/Linux signal-context tests, and full debug gates pass.

Claim: agent=profiler workspace=.jj-ws/fable-profiler machine=spark (shared with habu-bound-profiler-counter-235c5f48 - one lane, one landing, both dots)
