---
title: "Own transient storage across capture and snapshot"
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T10:15:29.737848+03:00"
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own dynamic-storage.f, layout-buffer.f DBUF control-record generation, snap.f/snap-lib.f lifecycle entry and aot-capture.f dynamic-release hooks; exclude capture interface/signatures. Register first live allocation and unregister release; reserve after restore registers again. Use private handle/generation in control record for bounded lookup/removal, reserve registry capacity before allocation publication and update handles on removal. Writer buffers survive outside captured value until write. Remove redundant per-pass lists only after coverage. Verify precompiled reserve after first/second restore, dirty buffer absent from manual lists, failure/empty/double cleanup, registry/snapshot/app-image tests.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
