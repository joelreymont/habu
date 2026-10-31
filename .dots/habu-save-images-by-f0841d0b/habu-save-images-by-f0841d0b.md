---
title: Save images by declared type
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T22:20:05.953454+02:00"
---

Joel 2026-10-31 (LESSONS 'typed data'): whether data can hold an address is decided by its declared type, not by how it was built or by its value; the scratch pointer forms (PTR-VARIABLE, PTR-U8-TABLE, PTR-U8-TABLE-VARIABLE) are written as null into a saved image and only the PERSISTED- forms carry an address; the old byte scan goes, CELL-BODY:DECLARE included. Plan: carl-dots/.out/carl-file/typed-data-plan.md (lanes T1-T6). Responsible layer: the engine's address-cell registry (src/habu/address-cells.f), used by capture and by FILE DATA-HASH. T6 (closing the ptr-field launder, prims.f:552, ~1,200 sites; dot habu-refuse-ptr-field-331a9731) needs its own design and Joel's word before it starts. Landing: one Fable feature review of the combined T1-T5 diff (capture format, save check); lanes merge into an integration workspace, not master. Claim: carl (lead).
