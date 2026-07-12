---
title: "maki: SKEY typed replay columns + evidence rows (unblocked tail)"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T18:47:47.739663+02:00"
---

Tail of the closed CAD ADT swap campaign (habu-cad-adt-swap-7bf0bb1f), now UNBLOCKED: the S2 wide-store capability and LAYOUT-BUFFER both landed via master's wide-ADT stack. (a) SKEY typed replay-table columns: upgrade the STR=-keyed replay table (maki/sched-key.f SK-FIND/SK-PUT/SK-GET) to typed skey product columns via LAYOUT-BUFFER, keeping the durable load path text-only and the injectivity pins (sched-key-test FNV golden + field-eq/text-eq never-diverge invariant). (b) Evidence rows: the typed evidence-row storage the swap plan deferred to LAYOUT-BUFFER S3. MANDATORY FIRST STEP: probe MODEL-CAD-V2-PLAN.md (design database §9, typestate/evidence R7) + the habu-epic-model-cad-70b629a9 dot tree for supersession - V2's immutable content-addressed artifact DB may replace the V1 replay/evidence design entirely; if superseded, close this dot pointing at the owning V2 slice instead of building V1 surface. Two disjoint slices -> two parallel workers per the stage-then-fan-out protocol if both survive the probe.
