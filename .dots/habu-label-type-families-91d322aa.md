---
title: Label type-families.md and V2-PLAN grammar as planned
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T19:03:52.208660+02:00"
---

Found by the docsync lane 2026-07-15: docs/type-families.md and MODEL-CAD-V2-PLAN.md describe the MODEL-CAD-V2 unified STRUCTURE ... ;STRUCTURE grammar and an E-REMOVED-TYPE-SYNTAX tombstone mechanism as if shipped; neither exists in the engine (unknown openers fail plain E-UNDEFINED exit 70; the error code appears nowhere in src/). The shipped surface is TYPEFAMILY/SUMTYPE/PRODUCT/ENUM/VALUE-RECORD/BEGIN-STRUCTURE per the reconciled forth.md block. Fix: reconcile both docs - clearly label the unified grammar + tombstone design as PLANNED (with its owning design dot if one exists, else decide whether the plan is still current or dead), and correct any sentence presenting it as current behavior; keep forth.md the single source of truth for the live surface with these docs pointing at it. Doc-only. Verify: rg for STRUCTURE/;STRUCTURE/E-REMOVED-TYPE-SYNTAX across docs/ + plan files all labeled or removed; host-lint. Ownership: docs planned-vs-shipped hygiene.
