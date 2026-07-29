---
title: "CAST: v2 - family ownership rule for cast declarations"
status: active
priority: 2
issue-type: task
created-at: "2026-07-19T01:59:06.031631+02:00"
---

Prerequisite to IR-0.1 and tightening follow-up to the landed CAST declarer
`habu-checked-cast-primitive-92991136`. A cast into a resolved scalar-cell
family, including a parametric `NEWTYPE` instance, is legal only when one
engine-owned provider authenticates the live namespace record and actual
public/private definition wordlist as that family's exact declaring package.
`CHECKER-PACKAGE-*` is parser and verification mirror state, never authority;
only the private catch-safe source-verifier and check-runner scopes may select
it, after it exactly matches the live provider. Projection casts remain
unrestricted. Explicit cross-package grants remain deferred; IR-0.1 needs none.

This dot owns the shared authority repair in `src/core/checker.f`,
`src/core/type-family.f`, `src/core/sumtype.f`, `src/core/structure-decl.f`,
`src/core/enum-decl.f`, `src/habu/verify-source.f`, `src/habu/xref.f`, and
`tools/check-core.f`; its hostile and confinement gates are
`test/cast-negative-suite.f`, `test/compiler/ir-id.f`,
`test/type-decl-suite.f`, `test/type-export-suite.f`,
`tools/refine-lint-core.f`, `tools/refine-lint-test.f`, and `TRUSTED.md`.

Checkpoint: the family owner is the live namespace provider; production
entries are source verification, the check runner, and `CAST:` declaration.
The verified baseline accepts ordinary owned casts but the hostile foreign
owner mint reaches the declaration path. The interface change is one
provider-authenticated destination-owner check. Forbidden alternatives are
trust expansion, checker-mirror authority, runtime guards, value heuristics,
or a public grant. Focused acceptance is the cast-negative, type-declaration,
type-export, compiler-ID, and refine-lint suites; broader gates are trust,
package, typed-local, file-map, Maki, PTX standard library, fixpoint, bootstrap,
and the native publication gate.

Acceptance covers A-as-B owner spoofing, visibility spoofing, foreign private
lookup, poisoned mirrors under a legitimate owner, stale namespace records,
wordlist/current mismatches, catch-safe offline verification, normal package
verification, and sealed provider state. The 26 compiler-IR raw tails are
definable only as private `CAST:` words in `src/compiler/ir/id.f`'s `IR-ID`
window; all 19 compiler API packages reject raw definitions, aliases, and
exports, while unrelated or global same-tail role APIs such as global
`COUNT>N` remain distinct. No `IR-RAW` package is introduced.

Claim RELEASED 2026-07-29: the ir0 agent is long gone (previous session) and its workspace was empty and idle on a commit predating the current chain. Its primary work (habu-add-compiler-ir-21e976fc, src/compiler/ir/id.f) is landed; this cast dot's work is NOT — the three maki suites still fail with 7135 E-CAST-OWNER. Free to claim.

Update 2026-07-28 (orchestrator, suite-red mapping): the live E-CAST-OWNER 7135 rejection currently reds three maki suite phases via maki/extent.f:71 (CAST: IX>N) and :242 (CAST: >RED) inside package MAKI — maki/sampling-test.f, maki/db/artifact-test.f, and maki/eval/train.f all die rc=7135. The downstream maki cast-declaration repair (making extent.f's family casts satisfy the declaring-package authentication, or moving the cast declarations to the family's declaring package) is part of this dot's acceptance: the three maki phases must be green before it closes.
