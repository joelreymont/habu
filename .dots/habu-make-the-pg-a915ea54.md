---
title: Make the pg one-bound deadline test hold under load
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T23:22:54.173777+01:00"
---

Problem: src/lib/pg-test.f F702 'one bound covers every wait of an operation' got 0 instead of E-DEADLINE in 2 of 4 runs of test/run.f -- sparql pg on spark at load 70 on 20 cores; 0 of 14 runs alone. Reading: aio.f WAIT-PASS takes events before EXPIRE, so a late kernel wait whose descriptor is readable ends ready past its deadline (not shown). Acceptance: the cause named and fixed where it is; the case passes in test/run.f under that load. Depends: none. Claim: unassigned.
