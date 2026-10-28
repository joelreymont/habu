---
title: Declare checker scalar scratch inside spans
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T15:50:44.483527+02:00"
blocks:
  - habu-move-the-remaining-95400204
---

Lanes 3a-3c, serial in checker.f: (a) NEW set + CHECKER-QUIESCE-END list + CAPTURE-SCRATCH-PREPARE; (b) LATE-CAPTURE-SCRATCH-PREPARE + TV-SNAP-RESET scalars + CFS; (c) HIDX/USX/UIX/CHX/NRX/CK-GRAPH/ASIG-GRAPH indexes, nonzero dead states moved to their ensure words. Each deletes the list it replaces; the seam, not the name, classifies a cell. Full suite after 3c. Claim: unassigned.

3a landed a1b82898; 3b landed 673671f7 "Declare late checker scratch inside spans" (review fold-ins F1, F3; SPAY-ARMED split to habu-clear-spay-armed-0c4ce365). 3c next: brief .jj-ws.noindex/carl-scr4/.out/brief-3c.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-scr4.
