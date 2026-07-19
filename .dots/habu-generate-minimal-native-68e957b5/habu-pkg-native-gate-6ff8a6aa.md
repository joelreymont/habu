---
title: Package native gate support
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:30:00.378488+02:00"
---

Frozen revision f7ed6085 leaks 1,671 gate/test-support globals into resident and worker images. Exact groups: run-lib.f/run-files.f/run-resident.f 426 globals (175 extended names); gate-pool/common/runner and global gate-stats portions 633 (243 extended); diagnostics/dictionary/engine/stdlib/inline/strict/debug phase libraries 612 (289 extended). run.f and worker aggregation load these structurally, so the bloat is resident, not dead tests. gate-stats already demonstrates the correct GATE-PROCESS package for one slice while 165 GS-* words remain ambient. Create owners TEST-RUN, GATE-POOL, GATE-STATS, GATE-COMMON, GATE-RUNNER, GATE-DIAGNOSTICS, GATE-DICTIONARY, GATE-ENGINE, and GATE-STDLIB. Public surface is only orchestration entry points and narrow cross-owner contracts; workers reopen TEST-RUN where shared private scheduling is intentional; phase internals are private. Remove TR-/GT-POOL-/GE-/GR-/GS-/GDX-/GD- forwarding globals. This controller must be split by owner before dispatch. Acceptance: resident, standalone phase, pool, stats, diagnostics, dictionary, engine, stdlib, cold/hot-cache, and full native gates retain exact behavior/output/budgets; old/private names reject; qualified APIs resolve; dictionary-name/JIT/DATA/CODELEN and process image measurements shrink without slowing the gate beyond its verdict bands; package/host/filemap/dot lints pass.
