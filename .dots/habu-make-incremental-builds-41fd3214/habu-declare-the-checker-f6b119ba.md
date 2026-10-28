---
title: "Declare the checker's restored counters as an owner span"
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T20:27:17.117986+02:00"
blocks:
  - habu-print-every-refusal-337830b1
---

Roots lane L3: UEND, SYM-N, SYM-STR-U/BOOT, NORET-END, BWIN-EPOCH, RECORD-LINK-EFFECT/CONTROL-HI, RECORD-BUF-*, CK-USE-* must survive a file and are restored through the checker's unit export/import; they become a checker CELL-SPANS producer read by OWNERS-GATHER (tools/native-unit-object.f ~2888) so the earlier-write check drops them as owner state. Masks 44+ files. After lanes 3b/3c. Claim: unassigned.
