---
title: "Migration: tools to unified types"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:46:28.644705+02:00"
blocks:
  - habu-migration-core-records-77182600
  - habu-migration-core-variants-af8e09b4
---

Migrate tools declarations, generators, parsers, report schemas, and fixtures to STRUCTURE and payload-capable ENUM. Update emitted Habu source so no generator produces removed definers or positional variant payloads. Preserve deterministic output and tool protocols. Run checker, diagnostics, repair, build-fixpoint, source-list, lint, codegen, and artifact owning suites.
