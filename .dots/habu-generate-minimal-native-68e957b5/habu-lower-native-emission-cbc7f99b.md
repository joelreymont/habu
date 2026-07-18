---
title: Lower native emission through typed IR
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T11:52:19.524294+02:00"
blocks:
  - habu-type-native-protection-c26d8323
---

Context: src/arch/arm64/icode.f writes encoded words immediately, preventing whole-routine reasoning about repeated loads, frames, calls, labels, and protection effects. Fix: define a compact package-scoped ARM64 machine IR for instructions, labels/fixups, calls, register effects, frame slots, control flow, no-return edges, and protection-state effects; validate it, then encode only validated IR. Preserve zero-allocation/cached construction and deterministic byte identity. Acceptance: one representative compiler routine and one primitive lower through IR to byte-identical code; invalid labels, effects, frames, and transitions reject before encoding; direct encoder bypass is unavailable to migrated emitters. Direct prerequisite: habu-type-native-protection-c26d8323, which transitively requires the ARM64 effect schema. Files: src/arch/arm64/icode.f, new single-concern ARM64 IR file, src/habu/habu1.f, src/habu/habu2.f, FILEMAP. Verify: IR snapshots, negative fixtures, clobber CFG check, native fixpoint.
