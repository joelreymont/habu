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

Checkpoint-discovered gate repair: deleting `LPENUMS` from a source line that
also defines retained variables exposes a package-diff false positive. Repair
the existing gate structurally: compare reconstructed old/new definition-token
identities and enforce ownership only for definitions that are new or changed,
not unchanged sibling definitions sharing a changed line. A focused fixture
must accept deletion of one sibling while rejecting a changed or added sibling;
comments, strings, duplicates, or line reordering cannot satisfy identity.
Do not add a path exemption, placeholder definition, heuristic, or wider lint
framework.

STAKE ADDED (does-conv lane 2026-08-14): this deletion also closes
2 of the last 5 E-NFEED-SCAN census rows (ENUM+/ENUM4+ in
src/core/enums.f are does>-definers). The conversion lane
(a65e56e5) LEFT them to this owner rather than convert-then-delete.
Consumer list the deletion must sweep, measured (the zero-call-
sites claim was FALSE): test/gate-dictionary-lib.f ENUMS case;
tools/lint/def.f rows 33/34 + def-test.f; package-diff-lint
fixtures; create-axiom-test.f prose; 3 rows each in habu2.f and
bootstrap/cg/forth.fs; build-fixpoint / hb-build-lib / boot-pin /
diagnose-hb-core / bootstrap.sh / package-diff-lint-core
exemption; docs/forth.md x2.
