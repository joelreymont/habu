---
title: Render and diff compiler IR
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:55:16.439866+02:00\""
blocks:
  - habu-canonicalize-compiler-tables-e0c7f8f1
---

Full context: design sections 5.6 and 6.6 require deterministic diagnostic rendering and structural diff that are never parsed by compiler code. Render every frozen table/reference with stable names and source locations; diff semantic structure, not text. Acceptance: golden output is deterministic and read-only; a repository search/gate rejects compiler parsing of renderer output. Dependency: canonical tables.

Claim: agent=irrender workspace=.jj-ws/habu-render-and-diff-3d249719

## MEASURED

Two files, one concern each, per `docs/forth.md` § Files. `src/compiler/ir/render.f`
(package `IR-RENDER`) turns one frozen module into diagnostic text; it walks the
four interned tables in canonical order and spells every interned reference as a
canonical ordinal from `IR-CANON`, so the text depends on what the module means
and not on the order its tables were interned in. `src/compiler/ir/diff.f`
(package `IR-DIFF`) decides what two frozen modules differ by, and asks the
renderer to spell the rows it names, so text has one owner and comparison has
another. Error block `-8120..-8139` is split `-8120..-8129` renderer,
`-8130..-8139` diff, and the region map in `lib/errors.f` now names both owners.

The diff pairs the interned tables **by content** and the program tables **by
position**. That is the whole reason it is not a text diff: adding one symbol
whose bytes sort near the front renumbers every later canonical ordinal, so a
line-by-line comparison of two renders would report dozens of changed rows where
one symbol was added. `DIFF` answers the report length and the number of
differences, and zero differences is the equality predicate, so no caller reads
the report back.

Type spellings are delegated to `IR-TYPE:FRENDER`, which unfolds a row into its
structure and prints no ordinal, so it is already stable. Attribute spellings are
delegated to `IR-ATTR:FRENDER` for every kind whose content is self-contained;
only the three kinds that store a reference (symbol, type, record) are spelled in
`render.f`, because `IR-ATTR` spells those with the module-local ordinal it stores
and this stage must spell them canonically.

The no-parse gate is `tools/render-parse-lint.f` with core and fixtures. It is a
new lint rather than an extension of an existing one because no existing lint owns
"which packages a subsystem may reach": `namespace-lint` checks package ownership
inside `maki/`, `package-diff-lint` checks a diff's definition owners, and
`error-code-lint` checks code uniqueness; a cross-package reachability fence in any
of them would be a second unrelated policy in one tool. The rule is an existence
check, not a judgement about parsing: no source under `src/compiler/` other than
the two renderer-stage files may name a word of `IR-RENDER` or `IR-DIFF`, open
either package, or load either source. Code that cannot obtain rendered bytes
cannot parse them. Which bytes are code is decided by the shared lexer
`LINT-LEX`, so comments and string bodies never fire and a lexer defect is a named
refusal instead of a quiet zero.

The module fixture moved out of `test/compiler/ir-canon.f` into
`test/compiler/ir-module-fixture.f` (package `IR-FIXTURE`) and all three tests now
share it, with two new knobs: one extra interned symbol, and the tagged operation
carrying the other integer under its `z-tag` key. `ir-canon.f` still passes
unchanged in behaviour.

### Gates, all on the tree as committed

`test/compiler/ir-render.f`, `ir-diff.f`, `ir-encode.f`, `ir-canon.f`,
`ir-build.f`, `ir-verify.f` exit 0. `tools/error-code-lint.f`,
`tools/suite-coverage-lint.f`, `tools/suite-coverage-lint-test.f`,
`tools/render-parse-lint.f` (18 compiler files, 0 findings),
`tools/render-parse-lint-test.f` (22 fixtures) exit 0.
`tools/package-diff-lint.f` and `tools/typed-local-diff-lint.f` exit 0 on the
`jj diff --git` artifact against parent `6c293bca`. The real in-process
`stdlib/lint-tools` group reports `PASS: lint-tools/render-parse`; that group's
three other reds - `status`, `bootstrap-mirror`, `trusted-inventory` - were
measured red at parent `6c293bca` too, with the same 44 bootstrap-mirror findings
and the same `-2500` throw, and none of the new files appears in any of them.

### Mutation matrix

| # | Mutation | Expected | Measured |
|---|---|---|---|
| 1 | `render.f` `PUT-SYMS` walks module rows in insertion order instead of canonical order | determinism fixture reds | `ir-render` golden and "two topological build orders render to the same text" both red; restored green |
| 2 | `diff.f` pairs symbol rows by canonical ordinal (what a line-by-line text diff does) instead of by content | semantic-vs-text fixture reds | `ir-diff` "one added symbol is one difference, not a renumbered table" red; restored green |
| 3 | `render.f` `REC-PAIRS` drops the canonical key re-sort and emits stored insertion order | determinism fixture reds | `ir-render` "two topological build orders render to the same text" red; restored green |
| 4 | `render-parse-lint-core.f` decides membership by raw substring presence instead of the shared lexer | hostile comment and string fixtures red | 10 of 22 fixtures red, including all four hostile comment/string cases, the lower-case case, the two require cases, the other-package case and the fail-closed case; restored green |
| 5 | the lint's two-file ledger is emptied | the tree-wide gate fires | 6 findings over 20 compiler files naming `IR-RENDER:ATTR-TEXT` and the package openers; restored to 18 files, 0 findings |

### Honest gaps

`E-IR-RENDER-CAP` and `E-IR-DIFF-CAP` are unreachable from a checked caller today.
Both stages commit to the same row, name and keyed-list ceilings as `IR-CANON`, and
canonicalization must succeed before there is anything to render or compare, so
`IR-CANON` refuses first. The checks stay because they bound writes into
package-owned buffers, and `ir-render.f` proves the composed path does refuse a
300-byte symbol name - by `E-IR-CANON-CAP`, named as such in the test. If the
ceilings are ever allowed to diverge these become reachable; that is worth a dot
for whoever publishes a ceiling from `IR-CANON` so the two cannot drift.

The renderer's per-item words (`SYMBOL-TEXT`, `TYPE-TEXT`, `ATTR-TEXT`) and its
output cursor are one package-owned sink under the single-task compilation
discipline, the same arrangement `IR-TYPE`'s function-type stage and `IR-CTX`'s
staging window already use. One text is written at a time; there is no test for
concurrent use because there is no concurrency to test.

The project provides no ohsnap-style text snapshot for Forth output - `lib/test/snap.f`
snapshots cell arrays - so the golden is a checked expected-text builder in
`test/compiler/ir-render.f`, one `LINE` call per rendered line, compared with `T$=`.
That is the same shape a snapshot would have and it reads as the format's
description.

`render.f` and `diff.f` each carry their own small byte writer - append a byte,
append a span, unsigned and signed decimal, sixteen hex digits - and so do
`src/compiler/ir/type.f` and `src/compiler/ir/attr.f`, which had them first. The
two sinks here cannot be merged into one shared cursor, because the diff writes
its report while the renderer writes a spelling into a scratch span, so a single
cursor would be clobbered by the nested call. The duplication that CAN be removed
is the arithmetic: one owner publishing value-style emitters
(`( cur ptr u8 cap value -- cur )`, the shape `type.f` and `attr.f` already use
privately) that all four files call, each keeping only its own cursor. That
refactor adopts two files this dot does not own, so it belongs in a dot of its
own rather than half-done here.

A rendered edge line states the successor COUNT but lists only the predecessors.
That is deliberate rather than an omission: which blocks a block branches to is
already on its terminator operation's line, so listing them again would state one
fact twice, while the predecessor list exists nowhere else in the text. The diff
compares both the count and the predecessor list.
