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

SCOUTED AND RULED (2026-08-19; full map in the scout report, verified anchors):

THE DIAGNOSIS SHARPENED. -8366 E-A64SEL-CAP is a MULTI-population code: nine
throw sites in select.f over at least five tables (value map VSLOT:359, dstack
slot :419, block ordinals :1328, branch edges :1454, if-conversion :1836/1843,
width :2674). The census's 20 rows are the VALUE MAP, but the only landed
fixture (native-match.f:1291) pins the BLOCK wall - the class this dot targets
has no pin. And the error text lies: VSLOT indexes IR-ID:VALUE-LOCAL, a
MODULE-wide ordinal never renumbered per function (op.f:905, regalloc.f:319
says "the MODULE's count"), so "more values in one function" is wrong - one
definition plus its quotations spends one shared budget.

THE SHARED ROOT. NFROZEN:VMAX 256 / NFROZEN:BMAX 64 (frozen.f:27,31) own the
sel/ra/comb value+block ceilings - one owner, one change, three codes. The IR
plan already permits D-VALS 512 against the selector's 256. The tree's own
doctrine is written at build.f:169-183 ("Ceilings are commitments, not
allocations") with the landed D-SYMS 256->512 example IN THIS DOT'S ERROR CODE.

RULED FIX SHAPE (smallest sufficient form, per the doctrine already landed):
1. Raise the frozen pair (VMAX to at least the IR's D-VALS ceiling and headroom;
   BMAX measured) and the plan ceilings that E-IR-OP-CAP names - commitments.
2. Bound every O(VMAX) clear and check by the DERIVED per-module count
   (IR-OP:FVALUES, readable at SELECT entry before any VBIND - regalloc's
   VALS-N!/N-VALS at :319-323 is the landed half of this discipline; selector
   and combine still check the constant per access and clear the full range
   per function). The raise must cost nothing on functions that do not use it.
3. Storage stays static dictionary allot - growing these tables out of the
   shared 512K mapping would FEED the E-IR-CTX-SCRATCH wall the interleave
   measurement says bites first. Raising MAP-BYTES is its own decision with
   its own attribution (context.f:73-105 changelog discipline).
4. Fix the -8366 error text (module, not function) and pin the VALUE-MAP wall
   with a generated-from-count fixture asserting its code (spill-probe
   discipline); the existing block-wall pin stays separate.
5. E-A64EFF-SEQ is FORMAT-bound, confirmed against the leaf's suspicion: 10
   positions PER SIDE (in and out are separate cells, a64-effect.f:272-333);
   growing it is a packing schema change with a DIGEST consequence. It stays
   a named refusal; not part of this raise.
6. Re-derive the spill-probe walls after (generated bodies: moving a wall is
   changing a number) and re-measure the interleave - E-IR-OP-CAP relief
   converts directly into CTX-SCRATCH pressure.

ACCEPTANCE. Census class TOTAL (capacity bucket 40 -> 0-or-named), never
per-code counts - LESSONS.md: the census's pressure-class codes swap run to
run over one tree; totals are stable. The 20 names need a small caller over
chain-census-core DEFS/DEF-CODE (:1356-1412) - build it first, it is the
instrument the acceptance reads. habu-own-the-tranche-7be822ae (same class,
two size-ranked leads) folds into this dot.
