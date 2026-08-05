---
title: Model the declared checker omissions
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-29T19:29:19.813769+02:00\""
---

Full context: formal/Common/Effects.v and Control.v each declare omissions in their own headers, and the new parity gate cannot see inside any of them — so these are checker rules that decide real programs with no model and therefore no gate: T-ATOM rigid host identities (region, extent, generation); VALUE-RECORD field cells with FIELD-PAIR? and FIELD-COERCE?; the whole-bundle TRANSPORT ops and the generated-accessor window; construct (CONM) and field projection; uniform<bool> block-uniform branches and the block-collective barrier; and MATCH's scrutinee pop. Split one leaf per omission rather than attempting them together. The gate's frozen tables already have a place to record each as modelled once its leaf lands.

Claim: agent=modeldecl workspace=.jj-ws/habu-model-the-declared-4a2eb3c9

SPLIT. Seven leaves, one per omission the code actually shows, each carrying the
exact header line it discharges, the checker path that decides real programs with
no model behind it today, the model file and section the rule belongs in, the
vector shape that would bind it, and the mutation that must go red:

- habu-model-t-atom-8110cc18 - T-ATOM rigid host identities (region, extent,
  generation).
- habu-model-value-record-f4eb2f10 - VALUE-RECORD field cells, FIELD-PAIR? and
  FIELD-COERCE?.
- habu-model-whole-bundle-9589e059 - the whole-bundle transport ops and the
  LAYOUT-XPORT register.
- habu-model-construct-row-bcdd5ef6 - construct (CONM) and its row surgery.
- habu-model-field-projection-d0c89b80 - the field-projection window and the
  generated-accessor arming.
- habu-model-block-uniform-6830ddce - uniform<bool> block-uniform branches and
  the block-collective barrier.
- habu-model-match-scrutinee-8d897108 - MATCH's scrutinee pop.

DISCREPANCY between this dot's summary and the code. This dot named six areas and
counted "construct (CONM) and field projection" as one. They are two different
rules with two different owners and two different obstacles, so they are split.
construct is a token machine in src/core/checker.f (CONM, checker.f:9098-9118)
whose step effect is applied inline; field projection is a single-shot armed
window (FIELD-PROJ!, checker.f:9130) that no ordinary candidate can reach at all -
only a named TRUSTED forwarder gets in, as test/field-proj-suite.f:43 does. Also
worth recording: the generated-accessor window is named in the Effects.v header
alongside the TRANSPORT ops, not alongside construct, so the transport leaf and
the field-projection leaf each say which of them takes it.

MEASURED. One of the seven leaves, habu-model-construct-row-bcdd5ef6, is
implemented in this workspace in the commit after the split; the other six are
open and unclaimed. construct is now modelled in formal/Common/Control.v with
five proved results and eight shared vectors, the construct clause is removed
from both model headers, and the parity gate is green. The full falsification
matrix, the honest gap list, and the long-term-versus-patch answer live in that
leaf's own dot. Two things learned there that the remaining leaves should read
before they start:

- The gate can only ask a question the checker's scope rules allow it to ask.
  construct resolves its family in the active package only, so
  test/compiler/checker-model-proof.f now runs the whole gate inside
  package CHECKER-MODEL-CASES. habu-model-field-projection-d0c89b80 and
  habu-model-block-uniform-6830ddce both face the same kind of problem (a sealed
  armed window, and two registered family ids) and should settle it before
  writing vectors.
- Mutating the shipped checker to falsify a row means rebuilding the fixpoint,
  because the behavioural vectors go through the checker baked into bin/hb and
  not through the source the structural walks read. That is
  bin/hb --load tools/build-fixpoint-refresh.f -- install --force, about fifteen
  seconds. A mutation that stops lib/ certifying cannot be used at all, because
  the fixpoint self-check refuses the build before the gate runs.
