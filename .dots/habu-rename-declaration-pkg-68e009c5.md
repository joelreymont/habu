---
title: Rename declaration package to DECL-TX
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T21:40:54.812333+02:00"
closed-at: "2026-07-30T04:38:59.221944+02:00"
close-reason: "Cut at 6ae012a2: namespace-only churn does not enable owner construction, GPT-2, or the vLLM replacement; no alias or migration is needed."
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

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `codex-decl-tx-rename` and workspace `.jj-ws/habu-rename-declaration-pkg-68e009c5` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `src/core/declaration-transaction.f:20` still reads `package DECLARATION-TRANSACTION` and callers such as `tools/release-inventory.f:571` still use the long prefix. The dot stays active and is free to claim.
