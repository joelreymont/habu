---
title: Self-host the x86_64 engine to a byte fixpoint
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.547997+03:00"
blocks:
  - habu-pass-the-full-07316f5a
  - habu-run-build-fixpoint-02e96bda
---

Problem: the cross-built engine is the recovery artefact; the release artefact is the engine that rebuilds itself on the ThinkPad to a byte fixpoint.
Acceptance: on the ThinkPad, `tools/native-build.f` with the cross-built engine as host (no shadow), then the chain to gen 5; `bytes gen 2 vs 3 0`; the gate green on the fixpoint engine; a stripped application built there runs; hashes and host identity recorded in `docs/x86-64.md`; the x86 engine pinned as a second release artefact with its fixpoint numbers in `docs/bootstrap.md`; the closing documentation pass over `docs/x86-64.md`, `docs/porting.md`, `docs/bootstrap.md` and `docs/gate.md`, and `INTEL.md` rewritten again (D1a).
Files: `docs/x86-64.md`, `docs/bootstrap.md`, `docs/porting.md`, `docs/gate.md`, `INTEL.md`.
Verify: ThinkPad: generations 2 and 3 compared with `cmp` (0 bytes differ); `bin/hb --load test/run.f` on the fixpoint engine; the stripped application runs.
Depends: habu-pass-the-full-07316f5a (G2).
Route: Alder (shared: docs/bootstrap.md, docs/porting.md, docs/gate.md).
Ownership: krait (Intel lane).
Claim: unassigned.
