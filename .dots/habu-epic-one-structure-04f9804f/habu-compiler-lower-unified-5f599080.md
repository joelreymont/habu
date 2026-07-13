---
title: "Compiler: lower unified type DSL"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:45:58.513536+02:00"
blocks:
  - habu-checker-certify-unified-5d56fe73
  - habu-lowering-hash-unified-586f7881
---

Implement byte-identical native, Gforth recovery, bootstrap mirror, AOT, fixpoint, and snapshot lowering for the unified STRUCTURE and ENUM event stream. Derive internal product/sum/tag-only kinds after parsing; do not branch on removed public definer names. Preserve package sealing, constructor WIDs, owner metadata, wide-layout operations, relocation, and deterministic image identity. Add bootstrap-codegen, AOT, snapshot, protected-WID, and fixpoint parity regressions.
