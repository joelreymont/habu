---
title: "Tests: retire auxiliary enum fixtures"
status: active
priority: 1
issue-type: task
created-at: "2026-07-13T17:19:03.146351+02:00"
blocks:
  - habu-enum-expose-named-5bfe8bb0
---

Hard-delete the unused legacy numeric counter definers `ENUM+` and `ENUM4+` by
deleting `src/core/enums.f` and every live boot, build, cache, diagnostic,
manifest, test, package-exemption, and documentation edge that exists only to
load or describe that file. Preserve the canonical block-style `ENUM` type
declaration implementation and its real behavior tests. Add no removed-token
assertion, tombstone, lint, ledger, alias, shim, compatibility path, or
replacement mechanism. Ownership: the dead file and its complete live wiring.
Acceptance: exact candidate tree has no executable reference to
`src/core/enums.f`, `LPENUMS`, `ENUM+`, or `ENUM4+`; bootstrap recovery,
bootstrap codegen, build fixpoint, run-files, package/typed exact-diff, and the
owning type/dictionary gates pass. Checkpoint: baseline gates pass and a live
use census proves the two definers have zero callers. Claim:
agent=enum_hard_cut workspace=.jj-ws/habu-delete-enum
