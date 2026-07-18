---
title: Canonicalize typed native IR
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T11:52:36.913344+02:00"
blocks:
  - habu-lower-native-emission-cbc7f99b
---

Context: typed IR alone still permits correct but redundant code. Fix: add deterministic pre-encoding passes for dead register stores, overwritten loads, redundant frame saves/restores, same-state protection transitions, constant/address rematerialization, repeated straight-line sequences, unreachable blocks, and branch simplification. Each rewrite must preserve the shared ARM64 effect schema and CFG proof; no heuristic fallback. Acceptance: mutation fixtures for every rewrite produce smaller byte-identical-semantics code; current xpad emitters eliminate unnecessary x10/x12/x13 frames automatically; optimized output passes independent emitted-CFG verification. Additional prerequisite: habu-type-native-protection-c26d8323. Files: ARM64 IR pass file(s), tools/lint/clobber-lint.f or successor verifier, test/engine-suite.f. Verify: pass snapshots, differential execution, clobber verifier, native fixpoint.
