---
title: "TFAM 14: enum families + legacy ENUM migration"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-03T23:36:48.956105+02:00\""
---

PLAN.md item 14. Retire/rename numeric ENUM/ENUM4 chain (src/core/enums.f) BEFORE reserving block ENUM ... END-ENUM; migrate call sites; block enums define checked constructors + exhaustive MATCH; duplicate/missing/bad variants reject. Gate 17n. Depends: TFAM 9-13.

## Naming decision: block form is `ENUM ... ;ENUM`

The dot text's "END-ENUM" predates the block-pair convention. Evidence for
`;ENUM`: docs/type-families.md uses `ENUM color ... ;ENUM` in every enum example
(§1, §4, §9.3, §23); PLAN.md item 14 acceptance says "block-style `ENUM color
... ;ENUM`" and the reservation list (line 438) says item 14 "reserves/migrates
`ENUM`/`;ENUM`"; the global block-pair rule and the landed `;SUMTYPE`/`;VARIANT`/
`;MATCH` precedent all use `FOO ... ;FOO`. The TFAM 9 lane resolved the identical
ENDMATCH-vs-;MATCH question in favor of `;MATCH`. Decision: `ENUM ... ;ENUM`.

## Audit findings (2026-07-09)

- Slice (a) is ALREADY DONE: commit `ulyunwtmrsww` "Move numeric ENUM chain
  behind legacy names" renamed the numeric definers to `ENUM+`/`ENUM4+`
  (src/core/enums.f) and migrated the only call sites (test/gate-dictionary-lib.f
  GD-ENUMS). `rg --pcre2 '\bENUM4?\b(?!\+)'` finds no remaining bare `ENUM`/
  `ENUM4` call site; the bare `ENUM` token is free to reserve.
- The enum-kind MACHINERY already exists (TFAM 9/10): `TK-ENUM` (kind 3,
  type-family.f:18), `TFAM-ENUM?` (219). Every `TFAM-SUM?` site is paired with
  `TFAM-ENUM?` — layout/width (227), construct kind gate (935), MATCH family-kind
  gate (963), layout predicate (221,995). So construct + exhaustive MATCH already
  lower enum-kind families; `docs/type-families.md` §9.3 states an enum is
  "equivalent to a zero-payload sum". Proven live by the `men` (SUMTYPE 0)
  fixtures M4 in type-match-suite.f.
- GAP: no defining word registers a `TK-ENUM` family. `SUMTYPE ... 0` registers
  `TK-SUM`. `TK-ENUM` is only created directly via `TFAM-DECL` in test suites.
  Item 14 therefore reduces to: add the `ENUM ... ;ENUM` surface syntax that
  lowers to the existing declaration path with kind `TK-ENUM`, arity 0, bare
  (payload-free) variant names — plus reject coverage and Gate 17n fixtures.
- Reuse surface: sumtype.f already has the full transactional declaration
  machinery — `TDECL-RUN` (rollback+report), `TDECL-FAMILY`, `TDECL-VARIANT-CLOSE`,
  `TDECL-CTOR-WORDS`/`TDGEN-*` (constructor generation), name gates
  (`TDECL-REQUIRE-FAMILY-NAME`, `TDECL-REQUIRE-VARIANT-NAME`), and the buffered
  collect/CTX pattern. The ENUM word reuses all of it; the only new code is the
  enum body reader (bare names, zero payload) + the two dispatch entry points.
- Two dispatch paths must both learn ENUM (mirror SUMTYPE): the engine word
  (sumtype.f, generates constructors) and the checked verify-source metadata path
  (verify-source.f `RECORD-SUMTYPE`/`RECORD-DEFINER?`), which needs a
  `PRIM: CHECKER-DEFENUM` effect model in checker.f (mirrors `PRIM: CHECKER-DEFSUM`,
  since verify-source is checked). AOT needs no change: the engine ENUM word is
  in the baked prefix the AOT maker loads.
- `enum`/`;enum` must join the reserved sets: `TDECL-KEYWORD?` (sumtype.f) so
  they cannot name a family/variant, and `RNL-RESERVED-DEFINER?`
  (tools/reserved-name-lint-core.f) next to sumtype/;sumtype.

## Slice plan

- (a) DONE upstream (commit ulyunwtmrsww): legacy rename + call-site migration.
- (b)+(c) ONE green engine commit — the `ENUM ... ;ENUM` surface plus its
  rejects and Gate 17n fixtures are one atomic, provable unit (the reject paths
  are intrinsic to the shared `TDECL-RUN` machinery, not a separable
  implementation slice; splitting would ship an engine change without its
  fixture proof). Contents:
  - sumtype.f: `ENUM`/`ENUM-COLLECT`/`CHECKER-DEFENUM`/`CHECKER-DEFENUM-BODY`/
    `TDECL-ENUM-VARIANT`/`TDECL-ENUM-VARIANTS`/`TDECL-ENUM-NOEND-BODY`; add
    `enum`/`;enum` to `TDECL-KEYWORD?`.
  - checker.f: `PRIM: CHECKER-DEFENUM` (name a u, body a u).
  - verify-source.f: `ENUM-END?`, `RECORD-ENUM`, dispatch entry.
  - tools/reserved-name-lint-core.f: reserve `enum`/`;enum`.
  - test/type-decl-suite.f: positive registration + constructor candidates +
    negative rejects (dup variant E-TFAM-DUP, empty E-TDECL-SYNTAX, missing
    ;ENUM E-TDECL-SYNTAX, uppercase variant E-TFAM-CASE, keyword/reserved/
    single-letter/family-collision variant E-TDECL-NAME, bad family name
    E-TFAM-CASE/E-TDECL-NAME, duplicate family E-TFAM-DUP), each with the
    transactional-rollback (TDT-BASE=) assertion.
  - test/type-match-suite.f: an `ENUM`-declared family MATCHed exhaustively
    (pass) and non-exhaustively (reject), proving enum-kind elimination.
  - Gate 17n: fixtures ride the existing gated GE-TYPE-DECL-SUITE +
    GE-TYPE-MATCH-SUITE (decl grammar → type-decl-suite; elimination →
    type-match-suite; "one concern per file"). No new core file, no
    srclist/result-cache churn; only sumtype.f/checker.f/verify-source.f content
    hashes change (engine rebuild + byte-fixpoint x2).
  - Proof: byte-fixpoint x2, full gate (bin/hb --load test/run.f), the six type
    suites + type-layout, maki/test.f, dot-dep-lint, typed-local-diff-lint,
    reserved-name-lint, host-lint, filemap-lint. No new TRUST/TRUSTED.md rows.
- (d) close the dot with the LANDED ledger.
