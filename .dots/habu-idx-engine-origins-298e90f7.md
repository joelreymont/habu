---
title: Index engine origins
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T14:50:14.400789+03:00"
---

Maki’s 18 MB checked-source replay spends about 70 percent of a native sample in ENGINE-ORIGIN:FIND-ROW/CELL-AT. Replace repeated origin-list scans with a direct lookup while preserving latest record occurrence, missing locations, FILE serial/phase/eligibility, imports, relocation and owner lifetimes. Base f97d4550 has Mark’s frozen coordinate-only origin collector and 72-byte FILE rows. Keep this separate from that format change; qualify native source/FILE behavior and the measured Maki replay, with independent review before integration.
