---
title: Bind MATCH depth and LIN-CHECK vectors
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-29T20:36:18.199163+02:00\""
---

Full context: two checker guards remain unbound after the vacuity audit closed three others. (1) MATCH's own depth guard: mutating its #CFC bound from 30 to 10 in src/core/checker.f leaves the checker-model gate GREEN — needs a vector nesting 31 begins then a MATCH over a real SUMTYPE fixture in test/compiler/checker-model-cases.f, model side opens 31 ++ TMatch/TFamTok = VReject. (2) LIN-CHECK: making it a no-op leaves all three linear vectors answering the same verdicts (verified directly — the rejections come from the deferred-taint rule, not the per-step count). A vector for the case where the linear value sits on NEITHER row when LIN-CHECK runs — an ordinary word carrying a to-r effect — would bind it. Both are the audit's falsification residue: theorems exist, gates cannot see the guards.

Claim: agent=matchdepth workspace=.jj-ws/habu-bind-match-depth-a2694b03

## Measured report (agent=matchdepth)

Both guards are now bound. Four new shared vectors were added to
`test/compiler/checker-model-schema.f` (CMV15-CMV18), two new prelude items to
`test/compiler/checker-model-cases.f` (the sum family `cmres` and the ordinary
to-r word `TO-R-WORD`), and the model's own depth result in
`formal/Common/Control.v` was strengthened so that it, too, pins the number 30
rather than the mere existence of a guard. `src/core/checker.f` is not modified
by this change; every mutation below was applied to a scratch copy of the tree
and restored before the diff was taken.

### What the dot prescribed, and why the shipped vectors differ

The dot asked for one vector: 31 nested `begin`s and then a match, model trace
`opens 31 ++ [TMatch; TFamTok fmres]`, verdict `VReject`. That vector binds
nothing, and this was checked rather than assumed. The body is unbalanced, so
the checker refuses it whatever the guard's number is; lowering the bound from
30 to 10 leaves the verdict `VReject`, and deleting the guard outright also
leaves it `VReject`, because with only a family token in play nothing ever
pushes past the frame ceiling. A guard that only ever refuses cannot be bound by
a program it refuses.

What binds it is a PAIR of programs straddling the guard whose verdict changes
class. A match form opens two control frames of its own and the frame stack
holds 32, so 30 is exactly the deepest stack at which both still fit:

  - CMV15, at depth 30: `begin` x30, then `MATCH cmres cmok OF begin`. The match
    takes the last two slots, so the trailing opener is the 33rd frame and the
    definition stops being checkable — UNCHECKABLE.
  - CMV16, at depth 31: `begin` x31, then `MATCH cmres cmok OF`. The guard
    refuses first, as a hard reject, which outranks an uncheckable — REJECT.

Lower the bound and CMV15 becomes a refusal; raise or delete it and CMV16
becomes an uncheckable. One shallower, at depth 29, the same probe answers
REJECT because the extra opener still fits — so the pair pins the number 30
itself and not merely a range.

### LIN-CHECK is bindable; the vector exists

No model gap was needed. The deciding case is exactly the one
`formal/Common/Control.v` already described in prose: the conservation count is
taken over the data row and the return row together, and a call runs it inside
the data-row step, BEFORE the return rows move. So an ordinary word declared
`( a | -- | a )` has its argument on neither row at the moment the count is
taken, while `>r` — which is its own rule and snapshots the whole transfer —
never does. CMV17 is that word applied to a linear value (REJECT) and CMV18 is
the control: the same word and the same tokens with an `i64` in play (CERTIFY),
so what CMV17 records is the linear and not the transfer. The model already
published both halves in `linear_count_spans_both_rows`; nothing there changed.

### Falsification matrix

Each row: mutate `src/core/checker.f`, run
`bin/hb --load test/compiler/checker-model-proof.f`, restore, run again. Script
and captured output in the agent's scratch (`cycle.sh`, `matrix.txt`,
`m1-bound10.txt`, `m2-noguard.txt`, `m3-lincheck-noop.txt`,
`restored-green.txt`).

| mutation | gate | which case failed | observed |
| --- | --- | --- | --- |
| none (baseline, before the new vectors) | exit 0 | - | `test: ok` |
| `MATCH-FAM-TOK` bound `#CFC @ 30 >` -> `#CFC @ 10 >` | exit 1 | case 632, CMV15 | `the shipped checker answers VUncheckable for a_match_at_the_deepest_frame_that_fits_takes_both_frames` / `assert: expected 1 got 2` |
| that guard line deleted entirely | exit 1 | case 633, CMV16 | `the shipped checker answers VReject for a_match_one_frame_deeper_is_refused_before_the_overflow` / `assert: expected 2 got 1` |
| `LIN-CHECK` body replaced by `;` (no-op) | exit 1 | case 634, CMV17 | `the shipped checker answers VReject for a_linear_on_neither_row_when_the_step_is_checked` / `assert: expected 2 got 0` |
| restored after each | exit 0 | - | `test: ok` |

One more mutation, this time of the frozen row rather than of the checker, to
show the new rows really are two-sided obligations and not one-sided
assertions: flipping CMV17's verdict from `V-REJECT` to `V-CERT` fails the Habu
case AND makes Rocq refuse the generated file with
`Error: Unable to unify "VCert" with "VReject"`. Restored immediately.

Two things the matrix shows beyond the headline. Each mutation turns exactly ONE
case red, so the attribution is exact rather than a wall of noise. And under the
LIN-CHECK no-op the three older linear vectors (CMV7, CMV8, CMV9) stayed green,
which is the audit's original finding reproduced: they are decided by the
deferred-taint rule, not by the count.

Direct verdict measurements behind the rows, taken with
`CHECK-QUIET-CANDIDATE!` on the shipped checker (-1 certified / 1 unresolvable /
0 refused) and with `vm_compute` on the model:

| program | checker | model |
| --- | --- | --- |
| `begin` x29, `MATCH cmres cmok OF begin` | 0 | `VReject` |
| `begin` x30, `MATCH cmres cmok OF begin` | 1 | `VUncheckable` |
| `begin` x31, `MATCH cmres cmok OF` | 0 | `VReject` (reason `MD_DEPTH`) |
| `( cmltok -- cmltok ) TO-R-WORD r>` | 0 | `VReject` |
| `( i64 -- i64 ) TO-R-WORD r>` | -1 | `VCert` |

### Gates

  1. `bin/hb --load test/compiler/checker-model-proof.f` - exit 0, `test: ok`.
  2. `make -C formal` clean; no `Admitted`, `admit`, `Axiom` or `Parameter` line
     anywhere in the diff; the assumption manifest still claims nothing (the
     gate reads it with assumption rows forbidden and every result still
     reports closed under the global context).
  3. `tools/package-diff-lint.f` and `tools/typed-local-diff-lint.f` on the
     `jj diff --git` artifact - both exit 0.
  4. `bin/hb --load tools/suite-coverage-lint.f` - exit 0, 164 suites, 0
     findings.
  5. `bin/hb --load test/gate-stdlib.f`: see the note below. The owning suite,
     `checker-model-proof`, is green in all three runs (14s before the change,
     40s and 64s after it - the machine was busier and there are four more
     vectors and four more Rocq obligations).
  6. `jj diff` shows no change to `src/core/checker.f`.

### Honest note on gate-stdlib

The whole-gate red set is not stable on this machine right now, so "unchanged"
could not be established as an exact set, and this is reported rather than
smoothed over. Three runs, all with a load average between 20 and 25 because
roughly twenty other worker lanes are running on the same host:

  - before the change: engine-error-package, pre-trust-defer, aot-wid-restore,
    stdlib-process-fixtures, owner-wid-internal, build-fixpoint-fixtures,
    hb-build-fixtures;
  - after: trusted-inventory, compiler-ir-id, check-cli-boundary;
  - after, again: compiler-ir-id, check-cli-boundary, refine-lint,
    refine-lint-fixtures.

The three sets are disjoint, which is already a sign they are not about the
change. Every suite in them spawns child `hb` processes and asserts on the
child's exit outcome, and the failures are process timeouts and outcome-kind
mismatches (`check-cli-boundary` fails with `E-PROC-TIMEOUT`, and the two
in-tree ones report "expected 0 got 1" on `T-OUTCOME-EXITED=`). Run one at a
time on this exact tree, `test/compiler/ir-id.f`, `tools/trusted-inventory-test.f`,
`tools/refine-lint.f` and `tools/refine-lint-test.f` all exit 0. None of them
loads any file this change touches: the diff is
`test/compiler/checker-model-{schema,cases,proof}.f`,
`test/compiler/checker-model-axioms.txt`, `formal/Common/Control.v`,
`LESSONS.md` and this dot. Also, a failing phase stops the run, so neither
post-change run even reached the phase the earlier seven reds live in.

An orchestrator merging this should re-run gate-stdlib on a quiet host to get a
clean set; the reviewer should not read the sets above as a regression, and I am
not claiming they prove the absence of one.
