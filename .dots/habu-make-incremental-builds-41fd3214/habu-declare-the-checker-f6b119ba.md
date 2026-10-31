---
title: "Declare the checker's restored counters as an owner span"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T20:27:17.117986+02:00"
closed-at: "2026-10-31T20:52:55.429521+02:00"
close-reason: "Obsolete after lane 11: no owner spans and no earlier-write refusal remain in the cache tools."
blocks:
  - habu-print-every-refusal-337830b1
---

Roots lane L3: UEND, SYM-N, SYM-STR-U/BOOT, NORET-END, BWIN-EPOCH, RECORD-LINK-EFFECT/CONTROL-HI, RECORD-BUF-*, CK-USE-* must survive a file and are restored through the checker's unit export/import; they become a checker CELL-SPANS producer read by OWNERS-GATHER (tools/native-unit-object.f ~2888) so the earlier-write check drops them as owner state. Masks 44+ files. After lanes 3b/3c. Claim: unassigned.
