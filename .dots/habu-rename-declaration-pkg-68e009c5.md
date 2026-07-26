---
title: Rename declaration package to DECL-TX
status: active
priority: 1
issue-type: task
created-at: "2026-07-26T21:40:54.812333+02:00"
---

Problem: the package name DECLARATION-TRANSACTION is needlessly verbose at every call site. Required result: rename only the package identity to DECL-TX and the owning source file to src/core/decl-tx.f. Update every production require, qualified call, test package reopen, release-inventory token, FILEMAP entry, and current documentation reference that denotes this package. Preserve all public word tails, stack effects, error codes, behavior, load order, and transaction semantics. Do not abbreviate prose that describes declaration transactions, introduce aliases or forwarding shims, retain the old file, or combine semantic refactoring with the rename. Owner: the existing declaration transaction module and exact consumers. Dependencies: none. Acceptance: rg over tracked Forth, FILEMAP, and current docs finds no DECLARATION-TRANSACTION package identifier or old source path; generated declaration transaction, engine, bootstrap/load, release-inventory, host-lint, filemap-lint, typed-local diff lint, and package diff lint all pass through native bin/hb. The rename is one coherent commit and the workspace is clean.

Claim: agent=codex-decl-tx-rename workspace=.jj-ws/habu-rename-declaration-pkg-68e009c5
