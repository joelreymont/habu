---
title: Save images by declared type
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T22:20:05.953454+02:00"
closed-at: "2026-11-10T21:56:50.086525+02:00"
close-reason: all five children are closed; aot-common.f SCAN-DATA-CELL, tools/hb-build-stripped-cells-test.f and snap-lib.f no longer exist
---

Joel 2026-10-31: whether data can hold an address is decided by its declared type, not by how it was built or by its value; the scratch pointer forms (PTR-VARIABLE, PTR-U8-TABLE, PTR-U8-TABLE-VARIABLE) are written as null into a saved image and only the PERSISTED- forms carry an address; the old byte scan goes, CELL-BODY:DECLARE included. Plan: carl-dots/.out/carl-file/typed-data-plan.md (lanes T1-T6). Responsible layer: the engine's address-cell registry (src/habu/address-cells.f), used by capture and by FILE DATA-HASH. T6 (closing the ptr-field launder, prims.f:552, ~1,200 sites; dot habu-refuse-ptr-field-331a9731) needs its own design and Joel's word before it starts. Landing: one Fable feature review of the combined T1-T5 diff (capture format, save check); lanes merge into an integration workspace, not master. Claim: carl (lead).

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.

T1b landed 2026-11-04 as 1183fb56 (typed storage pointer cells are DATA address rows; TFAM:HAS-ADDRESS?, ADDRESS-CELLS:REMOVE-SPAN). No test yet fails when STORAGE-MARK-TERM's T-PTR arm (src/core/layout-buffer.f:186) is removed: the save's value scan (SCAN-DATA-CELL, src/habu/aot-common.f:177) still refuses a mapped value in an undeclared cell, and an in-window value needs no rewrite (measured: the merged case passes on an engine without the arm). Acceptance case for the lane that removes the value scan: tools/hb-build-stripped-cells-test.f HBT-STRIPPED-MAPPED-TYPED must fail on an engine built without that arm.
