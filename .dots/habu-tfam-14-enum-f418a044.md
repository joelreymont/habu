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

## LANDED (2026-07-10)

Commits (bookmark maki-type-families, workspace .jj-ws/fable-tfam12):
- `3b1d8f06` TFAM 14: audit + slice plan (this dot body).
- `94682378` TFAM 14: enum families (ENUM ... ;ENUM) — the whole engine surface
  + fixtures + lints (slices b+c, one green unit; rejects are intrinsic to the
  shared TDECL-RUN machinery, not a separable implementation slice).
- (this commit) TFAM 14: close dot — ledger.

Slice (a) legacy rename/migration was ALREADY DONE upstream (commit
`ulyunwtmrsww` "Move numeric ENUM chain behind legacy names"): `ENUM+`/`ENUM4+`
+ the only call sites (test/gate-dictionary-lib.f GD-ENUMS). No bare `ENUM`/
`ENUM4` remained; the bare token was free.

What landed in `94682378` (7 files, +211/-4):
- src/core/sumtype.f: `ENUM`/`ENUM-COLLECT`/`TDECL-ENUM-NOEND-BODY` (engine word,
  generates constructors) + `CHECKER-DEFENUM`/`CHECKER-DEFENUM-BODY`/
  `TDECL-ENUM-VARIANT`/`TDECL-ENUM-VARIANTS` (registers TK-ENUM, arity 0, slots 0,
  zero-payload SUMV rows; reuses TDECL-VARIANT-CLOSE/TDECL-CTOR-WORDS/TDECL-RUN);
  `enum`/`;enum` added to `TDECL-KEYWORD?`; header updated.
- src/core/checker.f: `PRIM: CHECKER-DEFENUM` (name a u, body a u — mirrors
  CHECKER-DEFSUM) so the checked verify-source caller type-checks.
- src/habu/verify-source.f: `ENUM-END?`, `RECORD-ENUM` (metadata-only, mirrors
  RECORD-SUMTYPE), dispatch entry in `RECORD-DEFINER?`.
- tools/reserved-name-lint-core.f: reserve `enum`/`;enum`.
- test/type-decl-suite.f: positive ENUM registration + width + variant/tag +
  constructor candidates; 14 negative rejects (empty/no-term E-TDECL-SYNTAX,
  dup variant E-TFAM-DUP, uppercase/reserved-single-letter/keyword/family-
  collision variant + bad family name E-TFAM-CASE/E-TDECL-NAME, redeclare
  E-TFAM-DUP) each proving transactional rollback (TDT-BASE=), plus the enum
  diagnostic-prose packet ("bad enum declaration").
- test/type-match-suite.f: a real `ENUM`-declared family exhaustively matched,
  constructor→match round-trip, and non-exhaustive/duplicate/wrong-kind rejects.
- docs/census-tfam-14.md: IMPLEMENTED reconciliation banner (its §0 predated the
  landed deps).

Design decisions (evidence in the dot + census banner):
- Block form is `ENUM ... ;ENUM` (not END-ENUM) — docs §9.3/§23, PLAN item 14,
  block-pair rule, ;MATCH precedent.
- ENUM lives in sumtype.f, NOT a new enum-family.f (declines census C2): "one
  concern" is the shared ADT declaration grammar; a new core file would fragment
  the TDECL-*/TDGEN-* machinery and force srclist/FILEMAP/build-fixpoint/
  hb-build/prefix/result-cache edits for zero benefit.
- Gate 17n = fixtures in the already-gated type-decl + type-match suites (no new
  suite file, no gate/cache wiring churn).
- verify-source RECORD-ENUM is metadata-only like RECORD-SUMTYPE — it registers +
  rejects but does not materialize constructors under --all-errors (proven
  identical: `SRES:OK` is equally "undefined" via that path). Not a gap.

Proof (native macOS, workspace bin/hb fixpoint):
- Byte-fixpoint x2: build1 sha == build2 sha
  = `3c83eee4e86054a8e9d5f3c9249620f65fe8e02bca2227f56064d08fc50fb957`
  (new engine reproduces itself byte-identically).
- Full gate `bin/hb --load test/run.f`: rc=0, "PASS: native test suite
  (fixpoint + engine suite + checked hb + repl + hb-build) (30743ms <= 70000ms)";
  candidate-validate=1 (six type suites + type-layout ran on the fresh
  candidate); "PASS: native prop/debug gate phase" (axiom/prim census — covers
  the new PRIM: CHECKER-DEFENUM).
- Standalone suites on new engine: type-decl / type-match / type-family /
  type-ctor / type-linear / type-layout / type-family-rollback all "ok".
- maki/test.f rc=0 "test: ok". reserved-name-lint-test "ok". typed-local-diff-
  lint rc=0. dot-dep-lint rc=0 (0 findings). host-lint 0 findings. filemap-lint
  596 paths 0 findings.
- verify-source path: bad enum (dup variant) via --all-errors →
  "habu: bad enum declaration 'ebad': duplicate variant at 'red'".
- No new TRUST / TRUSTED: / set-check / TRUSTED.md rows.

Remaining work: none for item 14. bin/hb is gitignored (regenerable) — rebuild
from source. Next in the campaign sequence is item 15 (PRODUCT / value-record
migration), a separate dot.
