---
title: "Assert the mirror's DATA bands are disjoint"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T05:25:37.885330+02:00"
---

Problem: the Gforth mirror (bootstrap/cg/forth.fs, jit.fs) places its DATA cells and bands by hand-built constants with no build-time check that they are disjoint, unlike native's CLAIMS-ASSERT; the r4-lvf and r4-mirror lanes each found an overlap (SNAPSTK on the compile cells, frame 0 on checker.f's declaration-owner cells at $360/$368) that only a deep nest exposed at run time. Acceptance: building the mirror refuses, naming both claims, when any two of its DATA cells or bands overlap, including the cells src/ code reads at fixed offsets on every engine; every current claim is registered; a forced overlap in a scratch copy is refused at build time; recovery output unchanged (cmp). Files: bootstrap/cg/forth.fs, bootstrap/cg/jit.fs, the mirror's layout words. Verify: Gforth check-only recovery, forced-overlap scratch run. Depends: habu-keep-the-mirror-3e10d406. Ownership: mirror DATA layout check.
