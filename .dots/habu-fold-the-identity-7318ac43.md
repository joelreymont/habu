---
title: Fold the identity pointer-field read in the JIT
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T19:18:06.308075+03:00"
---

Problem: `0 ptr-field` is the identity on an address, so every read of a pointer-valued global at tier 0 pays a literal push and two calls for a type; ptr-field plus its two ;does bodies are 9.5 percent of tier-0 compile time (tools/tier0-profile.f, jit-path lane, 2026-09-16) and a share of every program's runtime. Acceptance: the JIT folds a `0 ptr-field` sequence (an immediate-fold row in the keyword and VOPI-ENTRY machinery of src/habu/habu2.f and src/habu/jit.f) so the read compiles to the plain load, with the seed mirrored; a fixture pins the emitted shape with tools/jitdump.f; tier0-profile and tools/tier-bench.f before and after; byte fixpoint; test/run.f. Files: src/habu/habu2.f, src/habu/jit.f, bootstrap/cg/forth.fs, test/. Verify: as above plus the stage0 chain. Depends: habu-use-pre-and-1830972f lands first (same emitters). Ownership: JIT emitters. Claim: unassigned.
