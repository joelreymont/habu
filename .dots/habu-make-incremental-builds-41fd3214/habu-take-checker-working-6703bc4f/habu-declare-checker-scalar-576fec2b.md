---
title: Declare checker scalar scratch inside spans
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T15:50:44.483527+02:00"
blocks:
  - habu-move-the-remaining-95400204
---

Lanes 3a-3c, serial in checker.f: (a) NEW set + CHECKER-QUIESCE-END list + CAPTURE-SCRATCH-PREPARE; (b) LATE-CAPTURE-SCRATCH-PREPARE + TV-SNAP-RESET scalars + CFS; (c) HIDX/USX/UIX/CHX/NRX/CK-GRAPH/ASIG-GRAPH indexes, nonzero dead states moved to their ensure words. Each deletes the list it replaces; the seam, not the name, classifies a cell. Full suite after 3c. Claim: unassigned.
