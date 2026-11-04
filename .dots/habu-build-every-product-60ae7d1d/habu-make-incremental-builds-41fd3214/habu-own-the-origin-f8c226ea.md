---
title: Own the origin journal and machine table cells
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T20:27:17.133692+02:00"
closed-at: "2026-10-31T20:52:55.649920+02:00"
close-reason: "Obsolete after lane 11: the earlier-write refusal is gone; MACH-T/MACH-N are plain variables and need no owner span for reuse."
blocks:
  - habu-print-every-refusal-337830b1
---

Roots lane L5: engine-origin-early.f/engine-origin.f SCR-U, FILE-SERIAL, SCR-HEAD (43/26 files masked) and src/compiler/native/machine.f:136-137 MACH-T/MACH-N (tier-1, minimum: an owner span) as storage-owner spans. Claim: unassigned.
