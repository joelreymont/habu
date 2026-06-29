---
title: Review CASE use
status: open
priority: 2
issue-type: task
created-at: "2026-06-29T17:53:47.268080+02:00"
---

Problem: CASE/OF/ENDOF/ENDCASE is now supported and checked, but older source still encodes multi-arm equality dispatch and enum/opcode selection as IF/ELSE chains or manual branches. Fix: audit src/, lib/, tools/, and tests for applicable multi-arm value dispatch; replace only clear equality-dispatch cases with checked case forms, preserving control-flow and stack effects. Priority areas: src/habu/habu1.f/habu2.f emitter dispatch, src/core/checker.f token/type/render selection, lib/ptx/cg*.f opcode/render tables, test/gate-* generated source helpers, and docs examples. Acceptance: audit note records converted vs intentionally-kept sites; converted words have typed stack effects and no deeper stack juggling; case misuse regressions still pass; focused tests for touched modules plus build-fixpoint-test pass. Files: src/habu/habu1.f, src/habu/habu2.f, src/core/checker.f, lib/ptx/cg*.f, test/gate-dictionary-lib.f, docs/forth.md.
