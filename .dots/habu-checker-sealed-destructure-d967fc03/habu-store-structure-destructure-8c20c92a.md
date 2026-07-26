---
title: Store structure destructure policy
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T20:49:46.951093+02:00"
---

Problem: the family registry has no authoritative destructure visibility, so a
declaration cannot distinguish a public record from an authority-carrying
record whose fields only its owning package may recover. Required result: add
one explicit scalar TFAM destructure-policy field with exactly PUBLIC and OWNER
codes, default PUBLIC. Provide sealed registry query and mutator hooks,
initialize every family, and carry the field through transaction snapshots,
rollback, registry growth, layout assertions, and the existing direct TFAM byte
copy path. Do not overload TF.DERIVE, layout policy, visibility, or spare bits;
do not add a parallel table, manifest, or default-on-read fallback. No source
syntax, generated-word behavior, or rebuilt-image proof changes belong in this
leaf. Owner: type-family registry representation and direct persistence only.
Dependencies: none. Acceptance: direct production registry fixtures prove the
default, set/read, invalid-code rejection, rollback byte identity, nested
declaration isolation, growth preservation, and complete-record byte copying.
Existing registry layout and snapshot suites remain green. Rebuilt-image
publication is owned by the dependent
`habu-prove-destructure-policy-e85db290` leaf.

Claim: agent=codex-destruct-registry workspace=.jj-ws/habu-store-structure-destructure-8c20c92a
