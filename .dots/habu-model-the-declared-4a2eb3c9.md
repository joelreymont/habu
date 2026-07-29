---
title: Model the declared checker omissions
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T19:29:19.813769+02:00"
---

Full context: formal/Common/Effects.v and Control.v each declare omissions in their own headers, and the new parity gate cannot see inside any of them — so these are checker rules that decide real programs with no model and therefore no gate: T-ATOM rigid host identities (region, extent, generation); VALUE-RECORD field cells with FIELD-PAIR? and FIELD-COERCE?; the whole-bundle TRANSPORT ops and the generated-accessor window; construct (CONM) and field projection; uniform<bool> block-uniform branches and the block-collective barrier; and MATCH's scrutinee pop. Split one leaf per omission rather than attempting them together. The gate's frozen tables already have a place to record each as modelled once its leaf lands.
