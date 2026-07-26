---
title: Store structure destructure policy
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T20:49:46.951093+02:00"
---

Problem: the family registry has no authoritative destructure visibility, so a declaration cannot distinguish a public record from an authority-carrying record whose fields only its owning package may recover. Required result: add an explicit scalar TFAM destructure-policy field with exactly PUBLIC and OWNER codes, default PUBLIC. Provide sealed registry query/mutator hooks, initialize every family, and include the field in transaction snapshots, rollback, registry layout assertions, baked/AOT capture and restore, and all native/bootstrap mirrors. Do not overload TF.DERIVE, layout policy, visibility, or spare bits. No source syntax or generated-word behavior changes in this leaf. Owner: type-family registry representation and persistence only. Dependencies: none. Acceptance: direct registry fixtures prove default, set/read, invalid-code rejection, rollback byte identity, nested declaration isolation, and AOT/fixpoint persistence on the real registry path; existing registry layout and snapshot suites remain green.

Claim: agent=codex-destruct-registry workspace=.jj-ws/habu-store-structure-destructure-8c20c92a
