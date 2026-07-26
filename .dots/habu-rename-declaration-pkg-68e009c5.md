---
title: Rename declaration package to DECL-TX
status: active
priority: 1
issue-type: task
created-at: "2026-07-26T21:40:54.812333+02:00"
---

Problem: the package name `DECLARATION-TRANSACTION` is needlessly verbose at
every qualified call site.

Required result: rename only the package identity to `DECL-TX`. Keep the
descriptive source path `src/core/declaration-transaction.f`. Update every
qualified production call, test package reopen, release-inventory token, and
current documentation reference that denotes the package identifier. Preserve
all public word tails, stack effects, error codes, behavior, load order, file
manifests, and transaction semantics. Do not abbreviate prose that describes
declaration transactions, introduce aliases or forwarding shims, rename the
source file, or combine semantic refactoring with this namespace change.

Owner: the existing declaration transaction module and exact consumers.
Dependencies: none. Acceptance: `rg` over tracked Forth and current docs finds
no `DECLARATION-TRANSACTION` package identifier. The generated declaration
transaction suite, engine suite, bootstrap/load path, release inventory,
host-lint, filemap-lint, typed-local diff lint, and package diff lint all pass
through native `bin/hb`. The namespace rename is one coherent commit and the
workspace is clean.

Claim: agent=codex-decl-tx-rename workspace=.jj-ws/habu-rename-declaration-pkg-68e009c5
