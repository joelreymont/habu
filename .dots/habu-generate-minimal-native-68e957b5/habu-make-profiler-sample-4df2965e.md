---
title: Make profiler sample totals exact
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-19T20:15:02.873821+02:00\\\"\""
closed-at: "2026-07-21T12:17:33.875498+02:00"
close-reason: "Landed 9aceba4f: the profiler no longer drops its final sample - attribution now happens BEFORE the limit check, restoring the exact invariant sum(word counters) + other == total, including the limit-reaching sample. Red-first against a truly-unfixed engine built from the parent source: limit-1 runs produced EMPTY output (the lone sample always lost) and limits 1/2/5 summed to N-1; fixed engine attributes all N. Tests wired into the debug gate phase; bootstrap mirror carries the same reorder; instruction-count-neutral so no size movement"
---

Frozen review f7ed6085, major profiler correctness defect. src/habu/prof.f:52 increments PROF-TOT and line 53 branches to dump-and-exit as soon as total reaches PROF-LIM, before lines 54-64 attribute the interrupted PC to a word or PROF-OTHER. Reproduction on the production stdin path: define a long PROF-BUSY loop, run 1 prof-on PROF-BUSY, and the process exits 99 with empty output; limit 2 prints PROF-BUSY 1. Thus every automatic report claims N samples but its buckets contain N-1, and limit 1 records none. Fix: attribute every delivered sample first, increment the total exactly once, then test the limit and dump; preserve signal-return behavior below the limit. Acceptance: limits 1, 2, and a larger deterministic run satisfy sum(word counters)+other=PROF-TOT=limit; limit 1 names the interrupted bucket; manual prof-report preserves the same invariant; bootstrap mirror, macOS/Linux signal-context tests, and full debug gates pass.

Claim: agent=profiler workspace=.jj-ws/fable-profiler machine=spark (shared with habu-bound-profiler-counter-235c5f48 - one lane, one landing, both dots)
