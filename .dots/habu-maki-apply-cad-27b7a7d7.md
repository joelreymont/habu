---
title: "maki: apply CAD-KIND:region to fusion-planner region ids"
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-12T18:41:30.705265+02:00\""
---

R3 remaining scope (merge 2634198e panel note N2). CAD-KIND:region is declared (maki/cad-kinds.f) but the fusion path (maki/cad.f REGION-FAM/FP-RID + fusion-plan.f region ids) still uses raw n - fable's planner surface that master never typed. Type the region ids with CAD-KIND:region + an FP-owned private refinement per the R3 owner-module rule, negatives for region<->node-id and region<->plan-id swaps, FP-RID@ callers migrated. Depends on the R3 merge landing (dot habu-merge-policy-master-961bb2b7).

## SCOPE CORRECTION 2026-07-12 (worker STOP report — no edits made)

The region-id caller set is far larger than cad.f+fusion-plan.f: lower-ew/mm/
red/move/launch.f, sched-key.f (SK-KEY$/SK-REGION-REP/SK-REGION-CK take raw
region), traffic.f (TRF-RGN-READS/WRITES), checkpoint.f (CK-SEG stores raw
FP-RID@), + tests backward-test, lower-mv-test, sched-key-test,
store-replay-test, demo-ffn-test, precision-device-test, lower-model-device-
test. Migration must be ATOMIC across all of them (checker rejects partial).
FP-REGION-COUNT stays a count (count-only consumers unaffected).

READY DESIGN (mirrors MIR-NODE-ID, model-ir.f:155-235): fusion-plan.f private
TRUSTED: RAW>RGN / RGN>RAW + RGN-RAW-CK (bounds 0<=raw<FP-RN after FP-CK,
E-FP-IDX) + public validated FP-REGION-ID ( n -- CAD-KIND:region ); FP-RID@
becomes ( CAD-KIND:node-id -- CAD-KIND:region ) keeping FP-CK guard (pin with
executed throw test); FP-REGION-MEMBERS/CLASSMIX take CAD-KIND:region; typed
FP-RGN= replaces raw = at consumer sites; FP-RID table stays behind private
RGN>RAW. Negatives: region<->node-id, region<->plan-id probes + out-of-range
FP-REGION-ID throw.

SEQUENCING: blocked behind dot habu-maki-audit-raw-25d3bf5e landing (its site
table maps the consumer migration; its worker owns the consumer files now).
Redispatch with ownership = the full caller set above.
