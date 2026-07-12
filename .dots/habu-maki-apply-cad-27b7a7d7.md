---
title: "maki: apply CAD-KIND:region to fusion-planner region ids"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T18:41:30.705265+02:00"
---

R3 remaining scope (merge 2634198e panel note N2). CAD-KIND:region is declared (maki/cad-kinds.f) but the fusion path (maki/cad.f REGION-FAM/FP-RID + fusion-plan.f region ids) still uses raw n - fable's planner surface that master never typed. Type the region ids with CAD-KIND:region + an FP-owned private refinement per the R3 owner-module rule, negatives for region<->node-id and region<->plan-id swaps, FP-RID@ callers migrated. Depends on the R3 merge landing (dot habu-merge-policy-master-961bb2b7).
