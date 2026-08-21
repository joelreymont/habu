---
title: Restore fail-closed 70 on patched checker
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T17:36:37.901104+02:00"
---

Full context: test/engine-error-package.f assert 5 (post-seal missing checker fails closed, line 143) patches the single embedded checker-package lookup token in the hb image (PATCH-IMAGE, line 118) and runs package source under the patched engine, expecting the designed fail-closed exit 70; on the proofs branch (identical on parent 960bf2d5 and the seal merge) the patched engine exits 67 (UNCAUGHT-RC, src/habu/layout.f:166) — the missing-checker path surfaces as an uncaught throw. Suspect: the CHECKER-AUTH-PACKAGE/CHECKER-PKG-CONTEXT plumbing throws 7136 before the post-seal bridge's mapped fail-closed exit. Root-cause which throw escapes (WHY-THREW or gdb catch on the exit), then make the missing-checker bridge map it to the deterministic fail-closed 70 on every load leg. Acceptance: test/engine-error-package.f all 5 asserts green through its exact gate path; a mutation restoring the uncaught throw reds assert 5.

Claim: agent=failclosed workspace=.jj-ws/habu-restore-fail-closed-4f1d6375 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED 2026-07-30 (agent failclosed, workspace .jj-ws/habu-restore-fail-closed-4f1d6375,
parent 01a544cd).

Static invariant: a definition the checker cannot place in a package must be
refused before it is compiled, and that refusal must reach the caller as the
engine's compile-reject status (70) with the offending state named. It must never
leave the checker as a throw code too wide to be a process exit status, because
such a code can only surface as the generic "nobody handled and nobody named"
exit 67.

Which leg was pinned, and how. The escaping throw is the one at
src/core/checker.f:634 (pre-change numbering) inside CHECKER-PKG-CONTEXT, and it
fires during the BOOT PREFIX RELOAD, not while reading user source. Proof by
mutation on the real load path: copy the tree to a private root, change only that
site from `E-PKG-CONTEXT throw` to `7911 throw`, and run the patched engine with
that root as its working directory (the engine re-reads its checker/core prefix
from disk at boot). Stderr became `hb: uncaught throw code 7911`, so none of the
eight other E-PKG-CONTEXT sites in the private verifier scope was involved. The
patched engine also failed with EMPTY stdin, which places the throw in the prefix
reload. A second probe at the same site printed `check@` and `get-current`:
`hook=0 cur=146`, i.e. the engine is inside a package word list while NO check
hook is installed. So the caller is a declaration front end asking for the
package context directly during the unchecked prefix reload, and the engine's
package bridge to the checker had already been skipped for that package because
the corrupted `checker-package` token could not be found.

Rejected first attempt (recorded because it is the tempting one). Close the
fail-open at the engine side instead: make src/habu/habu2.f C-FIND-CHECKER skip a
missing bridge word only while no check hook is installed (HOOK-CELL), mirroring
the compile-immediate path's existing demand for its preflight hook. Implemented,
rebuilt to fixpoint, and MEASURED to not fix assert 5: the `hook=0` evidence
above shows HOOK-CELL is still zero when the refusal happens, so the guard can
never fire first. Reverted. Any variant keyed on "the checker looks loaded"
(sibling-name consistency, a first-successful-resolution latch) rests on where
checker.f happens to define its five bridge words relative to the first `package`
keyword, which is ordering luck, not an invariant.

The fix. src/core/checker.f: CHECKER-PKG-CONTEXT's refusal now goes through one
new word, CHECKER-PKG-CONTEXT-REJECT, which writes
`hb: no authenticated package context for this definition` to fd 2 and throws
PKGCTX-REJECT-RC (70, the engine's RC-REJECT). Because 70 is inside the [1,255]
band the top-level reporter passes through untouched, an unhandled refusal exits
exactly 70 on every load leg, while an enclosing `catch` still receives a
catchable reject instead of a process exit. src/habu/xref.f retires the new
name with the rest of that cluster before the engine-prefix seal. Which programs
are refused is unchanged; only how the refusal surfaces. The eight verifier-scope
E-PKG-CONTEXT sites are untouched, so test/checker-verify-pkg-scope.f keeps its
7136 contract.

Measured legs, all with the rebuilt engine: patched engine + package source on
stdin -> named diagnostic, exit 70; patched engine + empty stdin -> same; plain
engine + `300 set-current : FOO ( -- n ) 1 ;` on stdin -> same; the same source
through `--load file` -> same; the same source inside `evaluate` under `catch` ->
the catch receives 70, the diagnostic is printed, and the session continues.
Plain engine + ordinary package source is still exit 0.

Falsification. (1) Restore the uncaught throw (`CHECKER-PKG-CONTEXT-REJECT` body
becomes `E-PKG-CONTEXT throw`) in a private root: assert 5 reds with exactly
`expected 70 got 67`. (2) Keep rc 70 but change the message to `hb: rejected`:
the new assert 6 reds. So neither the status nor the diagnostic is free.

Test added. test/engine-error-package.f now records the child's fd-2 byte count
and asserts the patched-engine run's stderr contains
`no authenticated package context` (assert 6). Without it the suite proved only
"some rc-70 reject happened", which an undefined word also satisfies.

Gates on the exact tree (artifact = `jj diff --git` against 01a544cd):
test/engine-error-package.f exit 0, six asserts; fixpoint refresh
`install --force` -> `compiler fixpoint`, self-check census
`0 uncheckable, 0 rejected, certified = 4237`; typed-local-diff-lint,
package-diff-lint, error-code-lint all exit 0 (error-code-lint: 0 finding(s)).
Affected phases run against a full copy of the parent tree for comparison:
engine-error-package 1 -> 0; aot-wid-suite unchanged (same three asserts fail,
their reported code moves 67 -> 70, which is dot
habu-model-bare-wordlists-9e7c3521's decision to make); gate-dictionary (full
run.f prefix), gate-engine runtime and repair slices, check-all-errors-test,
seal.f, seal-package.f, seal-absence.f, export-package.f, checker-verify-pkg-scope.f,
prop-test-core.f, compiler/ir-id.f - all green and byte-identical output in both
trees. No phase entered the red set.

Follow-up worth its own dot (not done here, outside this dot's boundary): the
engine may still SILENTLY skip a checker package-scope bridge word, and a missing
`checker-end-package` in particular would leave the checker's mirror inside a
package the engine has already left - a wrong authority rather than a refusal,
which no named exit can catch. Closing that needs a real handshake (the checker
latching "package bridge installed" in an engine cell at a defined point), which
is new engine layout surface this dot did not plan.
