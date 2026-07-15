---
title: Migrate legacy -V4 callers to typed .V4 vocabulary
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T23:16:28.391336+02:00"
---

Residual from habu-ptx-m10-vectorization-f394cfe1 (2026-07-15): the typed vec4 surface (vspan/vtile families, V4-ALIGN alignment obligation, LOAD.V4/STORE.V4/arith) coexists with the legacy hyphenated -V4 words (tile-v4.f + cg-vec.f) that keep the SCALAR tile type and prove nothing about alignment. Migrate every legacy caller - maki fusion codegen (FENCED: lower-*/fusion territory - coordinate), tools/ptx/* kernels (saxpy-v4 etc.), device-gold flagships if they use -V4 - to the typed vocabulary, then retire the untyped words so vec4 memory access is checker-proven everywhere (the M10 goal fully realized). Byte-identity of emitted PTX per migrated kernel is the review bar (proven possible: SAXPY-V4A == SAXPY-V4). Acceptance: zero -V4 callers repo-wide, legacy words deleted or reduced to the typed implementation, all device goldens green, saxpy-test pins re-derived if register numbering shifts. Files: maki fusion codegen files (coordinate fences), tools/ptx kernels, lib/ptx/tile-v4.f retirement. Verify: ptx groups, maki/test.f, on-device suite. Ownership: ptx typed-DSL adoption.
