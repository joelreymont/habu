---
title: Unify the diverged node walkers in the checker
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:25:10.802411+03:00"
---

Problem: src/core/checker.f walks the same 8-tag node shape in six places (337 lines) in two dispatch idioms, and two of the copies have diverged into a latent bug: VREC-COPY (about 3086-3155) and E-COPY* (about 5471-5551) differ in that E-COPY* persists TVK@ on the T-VAR arm and E-INST-FROM (about 6381) restores it while VREC-COPY/VREC-INST (about 3206) do not, so a TVK-RAW variable loses its RAW kind through a VREC round trip; S-ROW uses E-ROW-KIND where the other uses raw TVK@ (Opus audit 2026-09-16, cut 4). Also cut 7 (CF-ENTRY/CFN-ENTRY and CFB-ENTRY/CFBN-ENTRY in habu2.f 4566-4638 differ by one and three lines), cut 9 (the six-body return-row sextet RS->R..RS2R@ at checker.f 2517-2559) and cut 10 (NP-COLLECT-TERM / NP-INVARS-WALK 12909-12928 / 13025-13045, 98 tokens, one word differs). Acceptance: one walker per traversal with the arm behaviour parameterised, the TVK-RAW divergence fixed with a regression that round-trips a RAW variable through VREC and reads its kind back; the four duplicate pairs collapsed to one body each with the differing line as a parameter; checker suites, type-family suites and the full gate green; engine byte fixpoint. Files: src/core/checker.f, src/habu/habu2.f, test/. Verify: the regression; test/type-family-suite.f, test/type-decl-suite.f, test/checker-*.f; tools/native-build.f fixpoint; test/run.f. Depends: none. Ownership: checker. Claim: unassigned.
