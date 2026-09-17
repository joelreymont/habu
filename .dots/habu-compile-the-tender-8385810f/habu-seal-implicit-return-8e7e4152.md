---
title: Seal implicit return rows against undeclared reads
status: closed
priority: 1
issue-type: task
created-at: "2026-09-14T13:31:34.147154+03:00"
closed-at: "2026-09-16T14:34:49.721799+03:00"
close-reason: "superseded by habu-campaign-c4-diagnostics-3b6de147: Residue: undeclared r@ and 2r@ reads of the implicit return row must be rejected before execution."
---

Audit C4 independently reproduces on current candidate SHA46921f52: : W ( -- ptr n ) r@ ; : V ( -- n ) W @ ; V segfaults rc134 from checked source. 2r@ leaks return storage. XG-READ-VAR and implicit return-row setup permit borrowing below declared row. Fix checker ownership of implicit return row; preserve explicit balanced >r/r>/r@ cases, reject undeclared r@/2r@ and pointer laundering before execution. Original probe scratchpad path in RESTART.md. No patch at restart.
