---
title: Own transient storage across capture and snapshot
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"\\\\\\\"2026-09-13T10:15:29.737848+03:00\\\\\\\"\\\"\""
closed-at: "2026-09-16T14:34:48.803821+03:00"
close-reason: "done: The dynamic-buffer registry owns transient storage across capture and snapshot, with the lifecycle and its tests in the tree [src/core/dynamic-storage.f plus test/dynamic-buffer-registry.f, -capture.f, -tasks.f; docs/bootstrap.md documents RESERVE/RELEASE and the capture/persist release points]"
---

Plan: [PLAN.md](../../PLAN.md). Claim: Astra implementation complete in `cedar-transient`; Cedar independently reviewed and ran capture/concurrency regressions; integrated gate pending.

Own dynamic-storage.f, layout-buffer.f DBUF control-record generation, snap.f/snap-lib.f lifecycle entry and aot-capture.f dynamic-release hooks; exclude capture interface/signatures. Register first live allocation and unregister release; reserve after restore registers again. Use private handle/generation in control record for bounded lookup/removal, reserve registry capacity before allocation publication and update handles on removal. Writer buffers survive outside captured value until write. Remove redundant per-pass lists only after coverage. Verify precompiled reserve after first/second restore, dirty buffer absent from manual lists, failure/empty/double cleanup, registry/snapshot/app-image tests.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.


Implemented allocation-time membership with a private control-record slot, bounded swap-removal, and synchronized registry publication/removal for independent pthread tasks. Allocation and copying remain outside that lock; capture retains the stopped-task contract. AOT releases only captured controls for a retained runtime, or the whole replacement runtime registry. Snapshot releases the full registry immediately before DATA copy. Native pass counters still reset; redundant buffer release lists are removed.

Focused checks pass on private native product SHA-256 `cdac89e02a340a7734da980dac2325efe71d4ee2cc29ddfdf85d0b4a8fbbde0a`: dynamic-buffer, dynamic-buffer-registry, dynamic-buffer-capture, dynamic-buffer-tasks, certify-dynamic-buffer, image-lifecycle, image-lifecycle-tasks, native-session, app-image, snapshot-writer. This includes allocation refusal without control publication, partial-range refusal, repeated captures preserving outside writer buffers, and original reserve words used after both snapshot restores. The new concurrent-buffer test exposed a race before locking and passes with synchronization. Full integrated gate and recovery-stage execution remain pending.
