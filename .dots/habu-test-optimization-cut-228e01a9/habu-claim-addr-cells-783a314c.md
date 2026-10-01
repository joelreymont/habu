---
title: Claim ADDRESS-CELLS and the profiler band in data-claims.f
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T11:53:46.838753+02:00"
---

Problem: src/habu/data-claims.f's header (r4-mirror rumuymvu 3eccdd11) lists four declared claims left out of DATA-CLAIMS: the $1A0 seal fixture poke cell (no constant names it), ADDRESS-CELLS:LOCK-CELL ($1A8) and INDEX-CELL ($36C8), and the profiler band [DATA-SIZE - PROF-CNT-BYTES, DATA-SIZE) (src/habu/prof-abi.f PROF-BAND-AT). Review 120 measured that nothing blocks the two ADDRESS-CELLS rows: data-claims.f already requires a non-layout module (regalloc-abi.f), address-cells.f depends only on layout.f and is in the engine (habu2.f:7) and the fixpoint buffer (tools/build-fixpoint.f, appended after data-claims.f). CLAIMS-ASSERT cannot refuse an overlap with an unclaimed cell. Acceptance: LOCK-CELL, INDEX-CELL and the profiler band (for the target's DATA-SIZE) are rows; the header's out list holds only what no constant names; a forced overlap with each new row is refused at build naming both (seen failing first); g1 == g2 with .names, two-gen converged; build-fixpoint-source-test and data-claims-build pass. Files: src/habu/data-claims.f, tools/build-fixpoint.f.
