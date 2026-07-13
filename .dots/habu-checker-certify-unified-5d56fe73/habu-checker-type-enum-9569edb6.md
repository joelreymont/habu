---
title: "Checker: type ENUM construct match"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:14:49.624384+02:00"
blocks:
  - habu-checker-type-structure-d996215b
---

Own construct/MATCH effects and field-aware diagnostics for unified ENUM. Instantiate named payload fields in declaration order, preserve generic and linear rules, require exhaustive variants, and report variant plus field name on mismatch. Add exact positive/negative checked fixtures for compact and payload enums.
