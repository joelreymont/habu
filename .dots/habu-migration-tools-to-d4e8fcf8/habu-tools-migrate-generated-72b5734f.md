---
title: "Tools: migrate generated declarations"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:18:29.414971+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own generated Forth declaration strings and tool fixtures outside check-core/public-signatures. Convert each live legacy declaration to STRUCTURE or ENUM with named fields, preserving fixture intent and exact negative coverage; update tool manifests/filemap rows without adding host-language logic.
