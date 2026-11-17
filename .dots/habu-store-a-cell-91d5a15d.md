---
title: Store a cell inline in compiled code
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T15:49:34.581609+01:00"
---

Problem: in compiled code `!` (and `c!`) is a BL to an 8-instruction FPRIM with a frame, while `@` is inlined (habu1.f:4073; t0.f:2423 'T0 has no inline store'); in a store loop the call is about 70% of the time. Reproducer (Maki hunt habprof, 2026-11-17): /tmp/tim-habprof/store-faults.f, output store-faults.out: 64M stores to one resident page take 124.8 ms, 86 of 120 samples in `!`; in Maki's import plan `!` held 838 of 10,080 samples (8.3%), at least 0.6 s of it call cost. Acceptance: a compiled `!` and `c!` cost what an inlined `@` costs in that loop, with every check a checked store makes kept. Depends: none. Claim: unassigned.
