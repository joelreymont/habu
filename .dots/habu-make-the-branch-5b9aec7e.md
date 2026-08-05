---
title: Make the branch reach checks reachable
status: open
priority: 3
issue-type: task
created-at: "2026-08-01T16:19:20.883213+02:00"
---

src/compiler/native/emit.f B-WORD, BZ-WORD and BCOND-WORD each refuse a displacement that does not fit their form's field (E-A64EMIT-REACH), and none of the three can fire: NFROZEN's ceilings are BMAX 64 blocks and VMAX 256 values, and at most three instructions per operation, so the longest routine the chain can build is about 1300 instructions against an imm19 reach of +/-262144 and an imm26 reach of +/-33554432. Deleting the BCOND-FITS? line from BCOND-WORD reddens no suite (measured 2026-08-01 on the compare-branch fusion lane). What is pinned today is the PREDICATE - test/compiler/native-a64ir.f REACH-CASE asserts each of B-FITS?, BZ-FITS? and BCOND-FITS? at its exact edges, and widening BCOND-BITS from 19 to 26 reddens it - but the emitter's use of the predicate has no executed witness. Wanted: either a fixture that builds a module long enough to overflow imm19 (raising the block ceiling for one hostile case, or driving the emitter with a hand-built module whose layout is stretched) and asserts E-A64EMIT-REACH by name, or a written argument in emit.f that the ceilings make the three refusals structurally unreachable, with the arithmetic stated and a regression that reddens if a ceiling is raised past a reach.
