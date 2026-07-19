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

Measured 2026-07-19 addendum: a fresh-fixpoint census found 32 standard framed words
in the 4,296-record startup dictionary. Every one ends in a call immediately before
the restore/return epilogue; none is a call-free leaf frame. After direct `BL` lowering
lands, tail-call canonicalization can restore LR/SP and branch to the callee, removing
4 bytes per site, 128 bytes in this startup image. Pin this census as a structural
regression and require the pass to remove all eligible sites without changing dynamic
calls or exception/unwind behavior.
