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
