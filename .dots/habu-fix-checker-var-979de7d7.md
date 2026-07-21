---
title: Fix checker var binding across 3+ fresh atoms
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-21T00:24:18.053302+02:00\\\"\""
blocks:
  - habu-define-rigid-host-71b010a0
---

Pre-existing unifier limit found by the rigid-domains lane (2026-07-21), reproduced on the unmodified engine: consumer type-variable binding fails across 3+ co-resident fresh atoms in one family application, and a concrete arg between two fresh atoms breaks binding in some slot arrangements. Real usage never had >1 fresh atom per type before the rigid domains landed, so this was latent. It now BLOCKS habu-add-unique-bounded-527e05ca (span with >=3 rigid identities). Investigate the binding path (VREC-I-AK / E-I-AK instantiation order vs ATOM-OK? resolution), produce a minimal red fixture per failing arrangement, fix, and extend test/rigid-region-suite.f to 3+ co-resident identities. src/core/checker.f; CODELEN unaffected (checker loads from checkout).

Claim: RELEASED (agent=checkervar, Mac, 2026-07-21): competing spark claim agent=varbind observed at origin commit 7e08b382 minutes after the Mac claim was published; ceded to spark per the recorded ownership split (checker-internals blocker for the bounded-host epic, not a type-DSL chain stage). Mac lane was stopped early in analysis with NO edits; workspace removed. Spark claim below is the owner.

Claim: agent=varbind workspace=.jj-ws/fable-varbind machine=spark (owns the checker var-binding fix across 3+ co-resident fresh atoms: src/core/checker.f + rigid-region-suite extension; NOT a type-DSL chain stage - checker unification internals blocking the bounded-host epic)
