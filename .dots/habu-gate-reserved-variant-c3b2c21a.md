---
title: Gate reserved variant names in the event module
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T17:18:31.145740+02:00"
---

Found by the ENUM front-end lane (2026-07-21): decl-event.f's comment claims SUMV-ADD gates dup/canon/RESERVED variant names, but SUMV-ADD (src/core/type-family.f:592 era) enforces only canon + dup - reserved and single-character names are NOT rejected, so a compact variant named n or if is currently accepted (the legacy TDECL-REQUIRE-VARIANT-NAME rejected these). Variant-name gating is event-module-owned per the settled seam, so the fix belongs in SUMV-ADD or the DEV-VARIANT path in src/core/decl-event.f, NOT duplicated in front ends. Add the reserved/single-char gate with the same reject codes the legacy path used (E-TDECL-NAME 7110 family), red-first negatives through BOTH front ends (ENUM-DECL full+compact, STRUCTURE field names already covered) and the raw decl-event suite, and correct the decl-event.f comment to match enforced reality. Verify type-family/decl-event/enum-decl/structure-decl suites.
