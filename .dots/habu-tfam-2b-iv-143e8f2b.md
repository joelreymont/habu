---
title: "TFAM 2b-iv: Gforth absence-parity fixtures"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T02:07:47.913545+02:00"
---

Sealing slice 4. bootstrap/cg/forth.fs has NO package intrinsic, no CHECKER-* mutators, no atomics/ffi/stat-readlink-getdirentries-poll/snap-rebase (census discrepancy 5, verified 0 rg hits each). Parity = fixtures proving ABSENCE stays absent (a guard-bypass surface cannot silently appear in the mirror), not new guards. Also mirror the latch/seal for the surfaces Gforth DOES have (wordlist creation, raw stores, execute/compile sinks: forth.fs 544, 288, 2214). Depends: 2b-i.
