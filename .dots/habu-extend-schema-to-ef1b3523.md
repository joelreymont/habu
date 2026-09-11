---
title: Extend schema to full type tree for construct
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T15:42:48.640887+02:00"
---

Full context: the arity-n widening models sch as a width schema only - SchParam i, SchApp f (no arguments), SchCell - which is what SCH-NODE-IWIDTH (src/core/type-family.f:990) reads. That is sufficient for the MATCH scrutinee and uniform<bool> but cannot PRODUCE a type, and TFC-PAY-ROW, TFC-SCH-TERM and TFAM-FIELD-PROJ all instantiate a schema into a term over the family's arguments. Required result, named by the modelling worker as mechanical rather than deep: make sch a full type tree (SchParam i, SchApp f with argument list, SchCon, SchPtr, SchQuot), add instantiate : list ty -> sch -> ty, and DERIVE the width function from it so the two cannot drift. This also makes MATCH payload refinement genuinely arg-aware - Control.v's fam_pay is currently a supplied list. Two further named consumers stay separate and depend on this: transport needs the group-permutation machinery (XP-DATA-SEQ, XG-READ/XG-REPLAY, XP-BUNDLE-IN-K?) plus LAYOUT-XPORT/LAYOUT-INTRO as real parameters of layout_blockb; field projection needs the committed-field table (TYPE-FIELD: field id to family, variant, byte offset, extent, flags, schema), a registry the model does not have at all. Acceptance for this leaf: the schema tree, instantiate, derived width, arg-aware fam_pay; examples measured against bin/hb; every existing example still holds; build green; no Admitted.
