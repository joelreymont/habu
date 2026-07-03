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
