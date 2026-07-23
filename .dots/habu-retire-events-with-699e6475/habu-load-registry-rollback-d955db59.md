---
title: Load registry rollback composer
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T07:47:55.552231+02:00"
blocks:
  - habu-compose-registry-rollback-7e5742b7
---

Problem: the new CHECKER-REGISTRY-SCOPE composer must load exactly once after type-schema, type-family, declaration-event, and field ownership in native builds, bootstrap recovery, and fixpoint refresh. Owner: source composition only. Add the canonical require/provide/path rows and labels to both native and bootstrap generators, source inventory, boot pin, cache-key inputs, and FILEMAP. Preserve identical load order across installed bin/hb, generated compiler payloads, recovery bootstrap, and fixpoint; no duplicate installer, conditional existence fallback, shell glue, or generated artifact commit. Acceptance: removing or reordering any row makes the owning inventory or boot-pin test fail; native and recovered builds resolve the composer once; fixpoint is byte-stable. Files: src/habu/habu2.f, tools/build-fixpoint.f, bootstrap source list/generator owners, source inventory/boot-pin/cache-key tests, FILEMAP.md. Smallest check: source inventory plus generated prefix order proof, then native fixpoint/bootstrap owning gates.
