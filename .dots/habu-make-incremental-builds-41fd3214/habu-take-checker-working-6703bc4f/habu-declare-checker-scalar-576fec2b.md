---
title: Declare checker scalar scratch inside spans
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T15:50:44.483527+02:00"
closed-at: "2026-10-29T09:51:15.987933+02:00"
close-reason: "3c landed 518398d5 'Declare checker index scratch inside spans' (review ACCEPT; fold-ins: epoch-restart test, seam and UIX/CHX comments, dead USX-P store). 3c proof: engine and whitebox builds rc 0; engine-suite ok; frame and slots e2e ok; full suite 495/495 suites, no red; census unchanged at 184 refused / 8 reused. Lanes 3a-3c complete. PRM-FIRST site growth (9 -> 28 files) noted on the parent dot."
blocks:
  - habu-move-the-remaining-95400204
---

Lanes 3a-3c, serial in checker.f: (a) NEW set + CHECKER-QUIESCE-END list + CAPTURE-SCRATCH-PREPARE; (b) LATE-CAPTURE-SCRATCH-PREPARE + TV-SNAP-RESET scalars + CFS; (c) HIDX/USX/UIX/CHX/NRX/CK-GRAPH/ASIG-GRAPH indexes, nonzero dead states moved to their ensure words. Each deletes the list it replaces; the seam, not the name, classifies a cell. Full suite after 3c. Claim: unassigned.

3a landed a1b82898; 3b landed 673671f7 "Declare late checker scratch inside spans" (review fold-ins F1, F3; SPAY-ARMED split to habu-clear-spay-armed-0c4ce365). 3c next: brief .jj-ws.noindex/carl-scr4/.out/brief-3c.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-scr4.
