---
title: "Tests: migrate declaration suites"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:18:37.922243+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own type declaration, family, schema, and rollback test declarations. Replace legacy syntax with STRUCTURE/ENUM named fields while preserving each invariant; add hard negatives for positional payloads, mixed modes, missing delimiters, duplicates, reserved names, and atomic rollback.
