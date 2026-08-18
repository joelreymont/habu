---
title: "Size the chain's per-function tables from the function"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T20:04:14.713005+02:00"
---

The largest remaining compiler-refusal class (thecut-1's census, 2026-08-18): 40 of the 75 real refusals are FIXED-SIZE TABLE ceilings - E-A64SEL-CAP 20 (values per function exceed the selector's map), E-IR-CTX-SCRATCH 9, E-IR-OP-CAP 5, E-A64EFF-SEQ 4 (in+out positions overflow one cell), E-A64RA-CAP 2, E-A64COMB-CAP 1. One coherent fix: the derived-caps discipline applied to the chain's own per-function maps - size from the function being compiled (grow-on-demand arenas or measured-bound derivation), refusals become impossible-by-construction where structural, stay named where a real bound exists (E-A64EFF-SEQ may be a FORMAT bound - probe before growing). Each ceiling's fix mutation-backed; the census re-run is the acceptance (40 -> 0 or named-and-owned). Cut Phase B; bigger than every language gap combined.

CAPACITY IS REACHED FIRST ON MOST SHAPES, MEASURED 2026-08-18 (thecut-2, at the
derived pool NABI:SCRATCH). Widening the pool from a run of eighteen to the
machine's twenty-four did not move the register wall out of the way - it moved
the two walls INTO each other, and the migration context's 512K mapping now
bites before the allocator does on three of the four shapes
tools/codegen-spill-probe.f measures. Every number below is a throw code from
the production entry, one migration per count:

  loads inside a loop      20 compile | 21 -6644 | 22,23,24 -8508 | 25+ -6644
  constants across a loop  15 compile | 16..19  -6644 | 20+ -8508
  constants inside a loop  43 compile | 44+ -6644            (no register wall)
  64-bit constants inside  20 compile | 21+ -8508            (no capacity wall)
  crossing a call          9 or 10 compile | then -8508      (no capacity wall)

Two DIFFERENT capacity walls hide behind the one code, and a fix that moves one
will not move the other: at 21 loads the allocator gets through, plans five
re-emissions, and the module A64SPILL:REWRITE writes is the one that does not
fit (four live modules); at 25 loads nothing is allocated at all, because
selecting and combining the body alone already exhausts the mapping. So the
E-IR-CTX-SCRATCH 9 rows in the census above are not one population.

AND THE LESSON FOR PRESSURE FIXTURES, which is why this is on this leaf: a
fixture whose body is transcribed cannot follow a wall that moves. Every body in
codegen-spill-probe.f is now BUILT from its count, so moving a wall means
changing a number and nothing else - and every case asserts which CODE it earns,
because "refused" no longer identifies which wall was hit. Any fixture this dot's
work moves should be written the same way before it is moved.
