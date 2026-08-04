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
package, typed-local, Maki, PTX standard library, fixpoint, bootstrap,
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

Claim: agent=cast-owner workspace=.jj-ws/habu-cast-v2-family-741e7bae

CORRECTIONS from the implementing lane (2026-07-29):
1. The CHECKER-SIDE HALF WAS ALREADY LANDED AND CORRECT. None of the files this
   record claims to own (checker.f, sumtype.f, structure-decl.f, enum-decl.f,
   verify-source.f, xref.f, tools/check-core.f) needed a line. CHECKER-AUTH-PACKAGE,
   the live-record provider installed by xref.f, the verifier-scope mirror proofs
   and CAST-OWNER? were all in place, and test/cast-negative-suite.f already
   covered owner spoofing, visibility spoofing, foreign private lookup, poisoned
   mirrors, stale records and sealed provider state. The ONE uncovered case was a
   family registered by the ENGINE in the global package — exactly what the live
   maki failure hit.
2. maki/extent.f CAST: IX>N never failed. Its output is a plain n, i.e. a
   PROJECTION, which CAST-OWNER? permits on its first line. Only >RED threw.
3. Moving >RED alone was IMPOSSIBLE: its input ix<e> lived in package MAKI while
   its output redx is core-registered, so the cast had no legal home in either
   package. The real defect was that one three-family substrate (ix, extprod,
   redx) had two owners. The repair unifies ownership: ix joins its siblings in
   the engine registration and the converter pair lives at global scope in
   lib/type/extent-role.f.
4. The process exit code for these failures is 67 (uncaught throw), not 7135;
   7135 is the throw code.

