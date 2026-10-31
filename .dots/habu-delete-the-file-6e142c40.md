---
title: Delete the file-frame row store only the frame diagnostic reads
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T00:32:04.656812+02:00"
---

CEN review F4: BEGIN-UNIT's SCOPE file row and the whole file-frame row store (FBYTES, FSLOTS, FLIVE, FILE-ROW, PROBE, REINDEX, KIND-ROWS) are read only by FILE-REPORT's diagnostic line after CEN. Not done in CEN because removing a row shifts every frame rows= assert in test/native-file-frame-e2e.f. Do it after lane 11 lands: delete the store, its writers and the diagnostic's use of it, and update the rows= asserts once.
