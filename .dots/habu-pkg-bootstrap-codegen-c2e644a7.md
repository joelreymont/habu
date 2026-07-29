---
title: Package bootstrap-codegen-test and judge fixtures
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T00:46:44.515804+02:00\""
---

Full context: last package-gate blocker for landing the stage0 using commit (b9d5fca5) and the recovery prologue fix (88d258e1) in .jj-ws/habu-add-using-to-d815f0ab. Two finding classes on their combined artifact. (1) tools/bootstrap-codegen-test.f defines ~30 global BCG-TEST-* words plus BCG-MAIN with no package owner - it is an ordinary native test tool run by bin/hb, so packaging IS possible and raw global stems are forbidden by AGENTS.md; give it a real package with short tails and update any callers (check how tools/bootstrap.sh or suites invoke it - the entry word's name may be pinned somewhere; measure before renaming). (2) test/bootstrap-using-*-src.f fixture sources are compiled ONLY by the stage0 recovery engine inside tools/bootstrap.sh; the checker-hook fixture NEEDS a global CHECKER-USING because the engine finds it by bare lookup (an engine contract), and the shadow/caller fixtures deliberately prove TOP-LEVEL bare visibility - packaging them would destroy what they test. Decide the principled treatment: either package what can be packaged without changing the tested property, or add an exact-path-family category for stage0 recovery fixtures (same one-comparison-site row pattern as the mirror category, commit 2cceebce; justification: the fixture IS the gate's own input and its correctness authority is the bootstrap gate's whole-stream comparisons). Pin both directions in tools/package-diff-lint-test.f and falsify by mutation. Acceptance: the combined artifact of both stacked commits plus this one reports ZERO package findings; bootstrap-codegen-test.f exit 0 through its owning path; the bootstrap using gate fixtures still pass.

Claim: agent=bcgpkg workspace=.jj-ws/habu-add-using-to-d815f0ab
---
title: Package bootstrap-codegen-test and judge fixtures
status: active
priority: 1
issue-type: task
created-at: "2026-07-30T00:00:00.000000+02:00"
---

Two package-gate finding classes block landing this workspace's stack. (1) tools/bootstrap-codegen-test.f defines ~30 global BCG-TEST-* words plus BCG-MAIN with no package owner — it is an ordinary native test tool run by bin/hb, so packaging IS possible and raw global stems are forbidden; give it a real package with short package-local tails and update any callers (check how tools/bootstrap.sh or any suite invokes it — the entry word's name may be pinned somewhere; measure before renaming). (2) test/bootstrap-using-*-src.f fixture sources are compiled ONLY by the stage0 recovery engine inside tools/bootstrap.sh; the checker-hook fixture NEEDS a global CHECKER-USING because the engine finds it by bare lookup (an engine contract), and the shadow/caller fixtures deliberately prove TOP-LEVEL bare visibility — packaging them would destroy what they test. Decide the principled treatment: either package what can be packaged without changing the tested property, or add an exact-path-family category for stage0 recovery fixtures to tools/package-diff-lint-core.f. Pin both directions in tools/package-diff-lint-test.f following its conventions and falsify by mutation.

Claim: agent=bcgpkg workspace=.jj-ws/habu-add-using-to-d815f0ab

## MEASURED report 2026-07-30 (agent bcgpkg, workspace .jj-ws/habu-add-using-to-d815f0ab)

### Baseline: what the gate actually reported

The combined artifact is `jj diff --git --from f4162fa35ecb --to @`, where
f4162fa35ecb ("Claim stage0-using and hook-allowlist dots") is verified as
b9d5fca5's parent. Before this change `bin/hb --load tools/package-diff-lint.f`
reported 43 findings on it:

  * 40 on bootstrap/cg/forth.fs — the recovery mirror of the native engine.
  * 2 on tools/bootstrap-codegen-test.f — `BCG-TEST-BOOTSTRAP-PROLOGUE-
    UNCONDITIONAL` at 1254:3 and `BCG-MAIN` at 1520:3. Only two, because the
    lint reports CHANGED definitions and only those two lines were touched by
    the parent commits; the file's real global surface is about 45 words.
  * 3 on the stage0 fixtures — `CHECKER-USING` in
    test/bootstrap-using-checker-hook-src.f:13, and `BUS-SHADOW` (11) plus
    `BUS-CALLER` (27) in test/bootstrap-using-src.f. The other three
    `bootstrap-using-*-src.f` fixtures report nothing: everything they define is
    already inside a package.

The 40 mirror findings are NOT this lane's. This workspace's base predates the
mirror category: `rg 'MIRROR-EDIT\?|OLD-GLOBAL\?' tools/package-diff-lint-core.f`
finds nothing here, so the local lint over-reports them exactly as the contract
predicted. After this change the artifact reports 40 findings and every one of
them is on bootstrap/cg/forth.fs — measured as
`grep '^E-' | grep -vc 'bootstrap/cg/forth.fs'` = 0. All of this lane's own
findings clear; the remaining 40 are expected to clear at integration against
the lint that carries the mirror category.

### Part 1: tools/bootstrap-codegen-test.f now has a real package owner

No caller pins any word name. `BCG-MAIN` was referenced only by its own file at
line 1556. test/gate-stdlib-cases.f:624 and test/gate-stdlib-inline-lib.f:466
name the FILE path, and TRUSTED.md's witness rows name the file path too, so the
rename touched nothing outside the file.

The file already had five inner packages (CAD-NUM reopened, BCG-CAP,
BCG-MANIFEST, BCG-PREFLIGHT, BCG-USING) around a large global surface. That
global surface is now package BCG: the source-under-test buffer and the
substring assertion vocabulary in its public section, every individual check as a
private body, and one public `MAIN`. The hide-behaviour words became a sixth
sibling package, BCG-HIDE, matching BCG-PREFLIGHT and BCG-USING, and the two
duplicate-marker fixtures became BCG-DUP-EARLY and BCG-DUP-LATE.

Package name: `BCG` is admitted by the linter. The redundant-prefix rule compares
a tail against its package name and its file stem, so tails like `SRC`, `LOAD`,
`MUST-HAVE`, `PREFIX-LIST` and `MMAP-DIAG` are clean, and the file header now
spells out that BCG means "bootstrap codegen".

Each sibling package calls two or more of BCG's public words, so per
docs/forth.md they import once with `using BCG` and call them bare rather than
repeating `BCG:WORD`. Verified beforehand on a scratch fixture that `using` works
inside a package on this bin/hb, and that reopening a package restores private
visibility (docs/forth.md says so; measured to be sure, because BCG's private
assertion helpers are used from later BCG blocks).

BCG opens three times. The splits are forced, not stylistic: `include
src/habu/hide.f` publishes its BFR-* words globally and the `ndict@` watermark
has to be read at top level BETWEEN the two duplicate fixture records, and
packages do not nest. Each reopen carries a one-line comment saying why.

Proof that no check was lost or altered:

  * `bin/hb --load tools/bootstrap-codegen-test.f` exits 0.
  * The total assertion count is unchanged. Probing `T-CASE# @` at the end of the
    old file (from `jj file show -r 88d258e1`) and the new one gives 6241 in both.
  * Every assertion string literal is byte-identical. Extracting all 511 distinct
    `s" ..."` / `S\" ..."` literals with their multiplicities from both versions
    and diffing gives exactly four intended changes and nothing else: the three
    `s" BCGH-DUP-MARK"` and one `s" bcgh-dup-mark"` became `DUP-MARK`/`dup-mark`
    because the fixture word's tail was renamed and the engine stores the bare
    tail in the dictionary record, and `s" BCGH-ABSENT-MARKER"` became
    `s" BCG-NO-SUCH-MARKER"` since the BCGH- prefix no longer exists.
  * No dangling references anywhere: a pcre2 whole-token sweep for every old
    global name across the repository (archive docs excluded) finds none.

Mutation-tested the renamed dictionary lookups, since renaming a word that is
found by NAME STRING is the one change here that could silently stop testing:

  * Searching a name that does not exist (`DUP-MARK-ABSENT`) fails, exit 1.
  * Commenting out the `BCG-HIDE:MARK-MID` watermark call fails, exit 1.

One pre-existing vacuity found and closed. Renaming `package BCG-DUP-LATE` so the
duplicate record disappears left the suite GREEN: `FIRST-RECORD` only asserted
that the found index is below the watermark, which one record satisfies
trivially, so the duplicate that makes "the FIRST record wins" meaningful was
doing no work. `FIRST-RECORD` now names `BCG-DUP-EARLY:DUP-MARK` and
`BCG-DUP-LATE:DUP-MARK`, so removing either package fails the load
(`E-UNDEFINED: BCG-DUP-LATE:DUP-MARK`, exit 70) instead of passing. Re-measured:
that mutation is now fail-closed.

Ten bare locals in the moved assertion helpers became reportable once their lines
moved (the parent-only artifact is clean, so all ten are this change's).
They are lengths and indices, so they were typed `:n` rather than given an
exception comment. typed-local-diff-lint now exits 0.

### Part 2: the fixtures get a category, because packaging would delete the proof

Both fixture claims were verified in the code, not taken from the dot.

`CHECKER-USING`: bootstrap/cg/forth.fs `C-USING-CHECK-CALL` resolves the hook
with `LCHKUSING 13 done C-P2-FIND-CHECKER`, where 13 is the literal length of
`checker-using` — a bare dictionary lookup of that name, the same lookup that
finds the real global `: CHECKER-USING` at src/core/checker.f:5419 (checker.f is
itself an admitted global pre-hook surface). In a package the tail would sit in
that package's wordlist where a bare lookup cannot reach it: the mirror call
would find nothing, the fixture's `checker-using: BUS-A` line would vanish, and
the case would stop testing the mirror. This is an engine contract.

`BUS-SHADOW` and `BUS-CALLER`: tools/bootstrap.sh's `bootstrap_using_case` hands
each fixture to Gforth, which compiles it with the recovery emitter into a
standalone binary; the script runs the binary and compares the whole of stdout
and the first stderr line against exact expected text. bin/hb never loads these
files. `BUS-SHADOW` must be the name that already resolves before `using BUS-A`
opens, or "an import never shadows a name that already resolves" has nothing to
shadow. `BUS-CALLER` is compiled while the import is open from real top-level
position; in a package the subject would silently change to whether an outer
import survives into a newly opened package, a different and untested question.

So the treatment is a fourth principled category in
tools/package-diff-lint-core.f, `STAGE0-FIXTURE?`, built as its own row table
with one path-comparison site (`STAGE0-PATH=`) exactly like the existing
grammar-fixture table. It lists the two exact paths that actually need a
top-level word — not all five — so a global added to any other fixture still
reports. Admission also requires the plain lower-case `:` definer, and two
guards: the path must start with `test/` and end with `-src.f`, which is how
bootstrap.sh composes the path it builds. No new error code was needed;
malformed-table failures reuse E-PKGDIFF-ROWTAB. docs/forth.md § Packages now
documents the category beside the other exceptions.

### Falsification of the category (tools/package-diff-lint-test.f)

The first hostile battery was WRONG and the mutation run caught it: weakening
`STAGE0-PATH=` to a suffix comparison left the suite green, because no hostile
path ended with the whole listed path, and a fully case-varied hostile
(`test/Bootstrap-Using-Src.f`) was refused by the case-sensitive `-src.f` guard
for the wrong reason, hiding a case-folded comparison. Three hostiles were added
and one was corrected. Final battery, each mutation applied alone to the lint and
reverted byte-identical afterwards:

| mutation | result |
| --- | --- |
| baseline | exit 0 |
| `STAGE0-PATH=` -> `LINT-ENDS-WITH?` (suffix) | fails on `test/test/bootstrap-using-src.f` |
| `STAGE0-PATH=` -> `LINT-PREFIX?` (prefix) | fails on `test/bootstrap-using-src.f-more-src.f` |
| `STAGE0-PATH=` -> `LINT-STR=CI` (case-folded) | fails on `test/Bootstrap-Using-src.f` |
| plain-colon shape check -> `true` | 12 failures, starting at "only the plain colon word is admitted" |
| `STAGE0-FIXTURE?` unhooked from `GLOBAL-SURFACE?` | 4 failures, starting at "both listed stage0 fixtures admit" |
| an extra hostile row added to the table | 4 failures: the pinned row count and the unlisted-sibling hostile |
| basename-only rows plus suffix comparison | 5 failures |
| suffix comparison AND `test/` guard dropped | 4 failures |
| prefix comparison AND `-src.f` guard dropped | fails on `test/bootstrap-using-src.fs` |
| restored byte-identical | exit 0 |

The narrowing is pinned positively too: the five ordinary globals of
`TEST-ADD-ORDINARY-GLOBALS` placed in a LISTED fixture produce exactly four
findings, named `PDL-WRAPPER`, `PDL-STATE`, `PDL-LIMIT`, `PDL-CELL`, with
`PDL-HELPER` explicitly NOT named. A `CHECKED:` global and a `NEWTYPE`
declaration in a listed fixture both still report, so the two categories do not
leak into each other.

Honest limitation, written into the lint's own comment: dropping either narrowing
guard ALONE changes no verdict, because the exact whole-path comparison already
refuses every hostile. Those two guards constrain future row edits, and the table
above is what proves each of them bites once the comparison is weakened.

### Gates

  * package-diff-lint on the combined artifact: 40 findings, all on
    bootstrap/cg/forth.fs, 0 elsewhere (mirror findings, expected to clear at
    integration; this base predates the mirror category).
  * tools/bootstrap-codegen-test.f: exit 0.
  * tools/package-diff-lint-test.f: exit 0.
  * typed-local-diff-lint on the artifact: exit 0.
  * error-code-lint: exit 0, 1327 files, 844 claims, 39 reservations, 0 findings.
  * suite-coverage-lint: exit 0, 164 suites, 0 findings.
  * host-lint: exit 0.
  * The five bootstrap using-gate fixtures were built and run directly the way
    tools/bootstrap.sh does it (`HABU_TARGET=macos-aarch64`, `gforth -e "require
    test/nf.fs ... FORTH-EXE"`), comparing whole stdout and the first stderr line:
    bootstrap-using rc 0, -unknown rc 91, -ambiguous rc 94, -scope rc 70,
    -checker-hook rc 0. All five match. Full gate-stdlib was not run, per the
    contract.
