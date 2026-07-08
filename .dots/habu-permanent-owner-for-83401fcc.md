---
title: Permanent owner for trusted-inventory ratchet rows
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T09:22:08.939431+02:00"
---

TRUSTED.md classification rows cite an owning dot id, and tools/trusted-inventory.f -- strict fails when the owner file is missing from .dots/ (DOT-EXISTS?, :841 - file presence only). Completed capability dots that own rows (habu-police-set-check-850bc543 owns the 9 HOOK-INSTALL rows; habu-primitive-effect-axiom-1119f176, habu-seal-set-check-b3676b33, habu-audit-trusted-inventory-3a950436 similarly) therefore stay open forever: closing is survivable (file remains) but archiving removes the file and orphans the rows, turning strict red at archive time. Capability: a permanent-ownership convention - either trusted-inventory accepts a documented non-dot owner (e.g. a TRUSTED.md section anchor or docs/ anchor) for rows whose capability landed, or an explicit ownership-transfer step in the dot-close/archive workflow. Then close the completed owner dots.
