---
title: "Size the chain's per-function tables from the function"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T20:04:14.713005+02:00"
---

The largest remaining compiler-refusal class (thecut-1's census, 2026-08-18): 40 of the 75 real refusals are FIXED-SIZE TABLE ceilings - E-A64SEL-CAP 20 (values per function exceed the selector's map), E-IR-CTX-SCRATCH 9, E-IR-OP-CAP 5, E-A64EFF-SEQ 4 (in+out positions overflow one cell), E-A64RA-CAP 2, E-A64COMB-CAP 1. One coherent fix: the derived-caps discipline applied to the chain's own per-function maps - size from the function being compiled (grow-on-demand arenas or measured-bound derivation), refusals become impossible-by-construction where structural, stay named where a real bound exists (E-A64EFF-SEQ may be a FORMAT bound - probe before growing). Each ceiling's fix mutation-backed; the census re-run is the acceptance (40 -> 0 or named-and-owned). Cut Phase B; bigger than every language gap combined.
