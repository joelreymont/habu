---
title: Verify trusted-boundary evidence mechanically
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:03:51.655421+02:00"
---

The trust inventory classifies source sites but accepts a nonempty free-text Tests cell and cannot prove the cited test exists or exercises the claimed invariant. Replace assertion prose as authority with a package-owned machine-readable registry keyed by stable boundary identity. Each row names the exact invariant, capability owner, executable test identifiers, required positive/negative classes, and retirement condition. The gate resolves every test identifier through the canonical suite registry, observes that it executed, and requires an explicit assertion result bound to the boundary and invariant version. Generate TRUSTED.md from this registry or retain it only as a rendered view. Migrate the false TYPED-LINEAR claim only after its memory-safety owner lands. Reduce white-box test trust through typed fixture builders in bounded follow-up batches, preserving unavoidable metaprogramming as explicit boundaries. Add missing/stale/duplicate test IDs, test not scheduled, scheduled but assertion absent, invariant mutation, wrong owner, folded rows, and source-site drift tests. Files: trust registry/lint/inventory, canonical suite adapter, generated TRUSTED view and focused tests. Verify trust-lint, strict inventory, suite coverage, typed-local/package/host/filemap/dot lints, and full native gate.
