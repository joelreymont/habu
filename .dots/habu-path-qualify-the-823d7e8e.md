---
title: Path-qualify the audited hook allowlist
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-29T22:50:34.841001+02:00\""
---

Full context: UB-HOOK-ALLOWED? in tools/checked-boundary-lint-core.f:290 allowlists set-check hook installs by NAME ONLY (HOOK, USER-HOOK, CHECK-HOOK, CHK-CHECK-HOOK, LINT-CHECK-HOOK, ES-VERDICT-HOOK, PROP-CHECK-HOOK), so any file can define a word with a listed name and pass. This predates the snap-lib packaging (which only made one entry more generic: SNAP-CHECK-HOOK became CHECK-HOOK). The structural (path, name) authority already exists in tools/hook-sites.f. Drive the allowlist from that single table instead of a parallel name list: the lint knows the file it is scanning, so require (current-file, installed-name) to match a hook-sites row. Falsify by mutation: a hostile fixture defining CHECK-HOOK in a wrong file and installing it must become a finding; the seven legitimate sites stay green.

Claim: agent=hookpath workspace=.jj-ws/habu-path-qualify-the-823d7e8e (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED (agent=hookpath, workspace .jj-ws/habu-path-qualify-the-823d7e8e, base f4162fa3)

What changed. UB-HOOK-ALLOWED? and UB-TOP-HOOK-ALLOWED? in
tools/checked-boundary-lint-core.f no longer carry their own lists of accepted
hook names. Both now ask tools/hook-sites.f whether the file currently being
scanned and the name being installed are recorded together as one registry row,
through the existing public words HOOK-SITES:CHECK-MATCH? and
HOOK-SITES:TOP-MATCH?. The parallel name list is deleted, so the registry is the
only place a legitimate hook identity is written down. The top-row hook was
migrated rather than kept as a one-name special case: the registry already
carries top-row rows with their own kind, and keeping a separate single-name rule
would have been a second authority for the same question. A small helper
UB-FILE$ replaces the eight repetitions of "UB-FILE-A@ UB-FILE-U @".

Falsification matrix. Every row below runs through the real lint entry point
(CHECKED-BOUNDARY-LINT:FILE, the same word tools/checked-boundary-lint.f and
tools/check-core.f call); no validator was copied.

  case                                            before      after
  legitimate name at a temp-directory path        clean       UNAUDITED-HOOK
    (' CHECK-HOOK / ['] HOOK / ' LINT-CHECK-HOOK) (accepted)  (3 findings)
  TR-HOOK installed outside src/core/top-row.f    clean       UNAUDITED-TOP-HOOK
  install inside a TRUSTED: word at a wrong path  clean       UNAUDITED-HOOK
  all 11 committed registry sites, own paths      3 false     no hook finding
                                                  findings
  same 11 sites addressed as ./path               clean       unaudited (exact
                                                              path match, as the
                                                              census requires)
  install text inside a line comment, a paren     clean       clean
    comment, s" ... ", and s\" ... "
  made-up names (EVIL-HOOK, EVIL-TOP-HOOK)        finding     finding

The three false findings the old rule produced on real files were CPR-HOOK in
test/compile-preflight-recovery.f, CGR-HOOK in tools/codegen-role.f, and TRH-LOG
in test/top-row-hook-test.f: all three are recorded registry sites whose names
were simply missing from the hand-maintained list. Driving the check from the
registry fixes those at the same time as it closes the hole.

Mutation proof. Restoring the old name-only bodies (and nothing else) turns the
suite red with 12 assertion failures across TEST-NAMEONLY, TEST-TOPNAME,
TEST-STRICT-TRUSTED, TEST-SITES, and TEST-DOT-SITES. The tests therefore have
independent content and are not restating the implementation.

The hostile-text fixture was checked the same way, and the first version of it
failed that check: a string written as s" ' CHECK-HOOK set-check" is harmless
even to a scanner with no string handling at all, because the closing quote
sticks to the last word and the token reads set-check" rather than set-check.
The fixture now leaves a space before each closing delimiter, so the install
shape survives naive tokenization. Disabling line-comment skipping, paren-comment
skipping, or string skipping in turn now reds TEST-QUIET each time (two assertion
failures per mutation), which it did not do before.

Tree-wide scan. All 1391 tracked .f/.fs files linted through
tools/checked-boundary-lint.f: zero UNAUDITED-HOOK and zero UNAUDITED-TOP-HOOK
findings, and every non-hook finding is byte-identical to the parent tree's
output over the compared range.

Gates run on the exact tree.
  - tools/checked-boundary-lint-test.f through its owning load line: exit 0.
  - All eight gate-stdlib slices run one at a time - lint, lint-tools,
    lint-manifest, lint-artifacts, lint-libs, tool, check-cli, tail: exit 0 for
    every one. The tool slice carries this test's suite, tool-boundary-lints.
  - tools/trusted-inventory-test.f, the owning test for tools/hook-sites.f:
    exit 0.
  - tools/typed-local-diff-lint.f and tools/package-diff-lint.f on the
    jj diff --git artifact: exit 0 each.
  - tools/suite-coverage-lint.f: 164 suites, 0 findings, exit 0.
  - tools/host-lint.f: 0 findings, exit 0.
  - tools/dot-dep-lint.f: 0 findings, exit 0.
  No new error codes were introduced.

Note on the machine. The whole run happened with roughly twenty other agent
processes on the box and a one-minute load average between 18 and 29. One
combined gate-stdlib run (all slices at once) reported four red phases:
refine-lint and refine-lint-fixtures, which the gate itself labelled
TIMEOUT-UNDER-LOAD after hitting the 120-second cap with the pool saturated;
check-cli-boundary, whose child subprocess exceeded its ten-second budget with
throw code -2502 (E-PROC-TIMEOUT); and compiler-ir-id, whose three failing cases
are concurrent-allocator and task-reuse timing checks. Every one of them passes
when rerun on its own on this same tree, and none of them is one of the six
suites known to be red. The check-cli slice was also run against the parent
version of the lint core under the same load and passed, and the same child
fixture was timed on the parent tree at 10.0 to 11.2 seconds against its
ten-second budget, so that red belongs to the box, not to this change. None of
the six known-red suites appeared red in the combined run.

Known gap left open. UB-HOOK-NAME? in the same file is a second name-only rule:
while the checker is switched off it exempts any definition whose name ends in
CHECK-HOOK from the unchecked-definition finding, in any file. That is the same
class of weakness this dot closed for installs, but it is a different rule with
different callers, so it is reported for its own dot rather than folded in here.
Separately, tools/trusted-inventory.f re-implements registry pair matching inline
instead of calling HOOK-SITES:CHECK-MATCH?; that duplication is worth retiring.
