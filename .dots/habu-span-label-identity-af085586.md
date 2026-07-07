---
title: Span label identity beyond string equality
status: open
priority: 2
issue-type: task
created-at: "2026-07-02T00:18:23.161682+02:00"
---

MEDIUM from harness review: span dedupe (GS-CHILD-OWNED?) and subject attribution (GS-LABEL-SUBJ) key on raw label bytes. Two different subjects reusing one label text collide: the fork child suppresses a span the pool does not own, and attribution joins the wrong test row. Fix direction: qualify labels with the pool generation (GT-POOL-GEN$) or a phase id in both the test row and span records, or lint/reject duplicate labels at GT-POOL-START*/GS-TEST time. Add a negative fixture with two same-label entries proving current miscount first.

## Progress (attribution fixed + duplicate-label lint, in-territory)

Took the "lint duplicate labels" direction, self-contained in `test/gate-stats.f`
(the exclusive territory; GS-TEST/GS-SPAN/GS-CHILD-LABEL! signatures are frozen —
they are called from many gate-pool/run-lib files, so threading a generation
through them is a cross-territory change).

- `GS-LABEL-SUBJ` no longer first-match-wins: it resolves a label only when every
  matching test row agrees on the subject, returns `GS-SUBJ-AMBIG` (-2) when rows
  conflict, -1 when none match. `GS-SUBJ+` routes an ambiguous span to an
  unexpected stray instead of silently inflating the first subject's total. This
  fixes "attribution joins the wrong test row".
- `GS-CHECK-LABEL-DUPS` (run in `GS-SCAN` after indexing) counts test rows whose
  label collides with an earlier row of a different subject into `GS-LABEL-DUP`,
  reported in `GS-SUMMARY` as `label-dup=N`. This is the exact precondition that
  breaks BOTH `GS-LABEL-SUBJ` and `GS-CHILD-OWNED?`; nonzero surfaces the collision
  in RCA instead of a silent miscount. Live gate reports `label-dup=0`.
- Negative fixture `GST-TEST-LABEL-DUP` (test/gate-stats-test.f): two test rows
  share a label under different subjects + a span; asserts the span is NOT
  attributed to either colliding subject (fails under the old first-match code —
  proves the miscount), `label-dup=1`, and a unique-label control still attributes.
  Full gate `test/run.f` green; typed-local-diff-lint clean.

Remaining (cross-territory, keep OPEN): the lint makes a collision visible but does
not PREVENT the `GS-CHILD-OWNED?` over-suppression (the child drops the span before
it is written, so the scan cannot recover it). Full prevention needs the sanctioned
generation qualification at the label SOURCE — `gate-pool.f` building
`GT-POOL-LABEL$`/`GS-CHILD-LABEL!` with `GT-POOL-GEN$` so labels are unique per
generation — plus a hard gate assertion of `label-dup==0` in the gate runner. Both
live outside `test/gate-stats*.f`; do them as a coordinated gate-pool/run-lib change.

## Hard gate landed; qualification design tension (2026-07-07, from head d2456358)

LANDED: the hard gate assertion. `GS-LABEL-DUP-GUARD` (test/gate-stats.f)
dies rc 1 with a source-pointing diagnostic when the scan counted any
cross-subject label collision; `TR-COMPLETE` (test/run-lib.f) runs it after
GS-SUMMARY and the red check, so a collision now fails the whole gate
instead of surfacing only as a summary field. Fixture: GST-TEST-DUP-GUARD
(test/gate-stats-test.f) proves both legs via spawned children over tiny
fixture files (die exits the process, so in-process catch cannot assert
it): forced dup -> child rc 1 with the diagnostic; clean -> rc 0. Live
gate green (label-dup=0), so the guard adds no false positive.

STILL OPEN: generation qualification at the label source. Mapping the
emission paths surfaced a real design tension the earlier note glossed:
GS-CHILD-OWNED? suppression is exactly what dedupes the fork child's OWN
completion span (emitted by generic runner code, e.g. gate-stdlib-inline
GSI-RUN at :116, with the raw path label) against the parent pool's
authoritative span for the slot. Qualifying the parent-side records
(GT-POOL-LABEL$/GS-CHILD-LABEL! with GT-POOL-GEN$, per this dot) makes the
child key "g<gen>:<label>" while the child's inner completion span still
carries raw label bytes - the suppression then MISSES and every pool child
double-emits its span (parent qualified + child raw), which is a worse
miscount than the collision being prevented. Sound designs must qualify
BOTH sides consistently: either (a) thread the qualified identity into the
child's runner context (a GS-side "current span identity" cell the fork
child sets and GSI-RUN-class emitters consult when their label equals the
raw child label), or (b) move dedupe off byte-matching entirely: the fork
child suppresses exactly its OUTERMOST completion span (depth-scoped
one-shot), making label text irrelevant to ownership. (b) is smaller and
kills the string-identity fragility this dot is about; it needs the
GSI/GR emitters' nesting behavior pinned by fixtures first. Either way the
change spans gate-pool.f + gate-stats.f + gate-stdlib-inline-lib.f /
gate-runner-lib.f; with the hard guard in place a regression cannot land
silently, so the remaining work is safe to schedule as its own unit.

## Design (b) pinned and REJECTED as unsound (2026-07-07, from head c3fd3853)

Pinned the GSI/GR fork-child span-emission behavior before touching anything.
Result: design (b) - "suppress exactly the child's OUTERMOST completion span,
label-independent" - cannot be made sound. Two real fork-worker shapes emit
their completion spans at the SAME emitter nesting depth (the top level of the
fork-worker body), so no depth/order/one-shot rule can tell them apart; only
equality with the slot's pool label can. The label match is load-bearing, not
incidental fragility.

Evidence (both shapes are live in the gate today):

- SINGLE-MATCH shape - `GSI-FORK-INCLUDE` (gate-stdlib-inline-lib.f:170) sets
  the pool label to the file path, then the child runs `GSI-INCLUDE`, whose
  `GSI-LOAD-FINISH` -> `GSI-SPAN` -> `GS-SPAN` emits exactly ONE span whose
  label EQUALS the pool label. The parent's pass-hook (`TR-POOL-PASS-SPAN` ->
  `GS-SPAN`, run-lib.f:595) emits the authoritative copy, so the child's must be
  suppressed. `GS-CHILD-OWNED?` does exactly that by byte-matching the pool
  label.

- MULTI-NONE-MATCH shape - `GSI-LINT-TOOLS-STATUS` (gate-stdlib-lint-tools.f:34)
  is a fork worker whose pool label is "lint-tools/status", but its body emits
  FOUR sibling completion spans via `GSI-RUN`/`GSI-INCLUDE` - "repl-lint",
  "trust-lint", "stale-status-lint", "test/gate-stats-test.f" - NONE equal to
  the pool label. The parent records only "lint-tools/status" (pass-hook); the
  four sub-spans are legitimate drill-down strays that must all be counted.
  `GS-CHILD-OWNED?` suppresses none of them because none matches the slot label.

A label-free "suppress the first/outermost completion span" one-shot armed at
fork entry would swallow "repl-lint" (undercount) while the SINGLE-MATCH shape
needs its one span swallowed - and both spans are top-level siblings of the
fork-worker body, indistinguishable structurally. There is no
depth/first/last signal that keeps every sub-span in the MULTI shape yet drops
the self-completion in the SINGLE shape. Hence (b) is unsound.

Two further wrinkles reinforce this:
- Nested pools reuse the SAME `GS-SPAN` entry for a DIFFERENT role: a fork child
  that is itself a pool parent (e.g. the resident phase-9 child ->
  `TRWE-POST-CANDIDATE`) emits its grandchildren's authoritative spans through
  the pass-hook -> `GS-SPAN` -> `GS-CHILD-OWNED?`. A one-shot armed in that child
  would consume on the first grandchild span and suppress an authoritative span.
  So (b) would also have to split the pass-hook emission from the self-completion
  emission, which are indistinguishable at the `GS-SPAN` call site.
- `GSI-RUN` emits its span AFTER its quotation runs, so if a worker ever nested
  GSI scopes the outermost completion would be emitted LAST, not first - the
  "deterministically first/last" assumption also fails.

Pinning fixtures (pass on the current tree, guard the byte-match semantics a
future change must preserve or consciously replace):
- `GST-TEST-MULTI` (test/gate-stats-test.f) - unit: child label set, three
  sibling spans none matching, all three counted; falsifies "drop the first
  span".
- `GPT-SPAN-MULTI-CASE` (test/gate-pool-test.f) - real fork of the
  MULTI-NONE-MATCH shape: parent emits the slot label once, all three child
  sub-spans survive; sits beside `GPT-SPAN-CHILD` (the SINGLE-MATCH shape) so the
  two shapes are pinned side by side.

STILL OPEN - the sound fix is generation qualification of BOTH sides
consistently (the dot's original top-of-file direction, not (a)/(b)): thread
`GT-POOL-GEN$` into the pool label SOURCE (`GT-POOL-LABEL$`/`GS-CHILD-LABEL!`) AND
into every self-completion emitter (`GSI-SPAN` via `GSI-PATH$`, `GR-STATS` via
`GR-TOKEN$`, the `GSI-INCLUDE` path) so the byte-match still lines up but labels
are unique per generation and cannot collide across subjects. That keeps the
label match (which is doing real work) while removing the collision it can
mis-fire on. It is the cross-territory change the earlier note flagged (frozen
`GS-TEST`/`GS-SPAN`/`GS-CHILD-LABEL!` signatures, every emitter), NOT the smaller
label-free redesign (b) hoped for. The hard `GS-LABEL-DUP-GUARD` remains the
safety net so no collision regression can land silently while this is scheduled.
