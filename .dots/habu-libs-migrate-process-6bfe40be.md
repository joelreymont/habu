---
title: "Libraries: migrate process OUTCOME"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:16:55.142532+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own lib/process.f declaration/constructor consumers and focused process tests. Replace OUTCOME SUMTYPE positional payloads with named ENUM fields, preserving tag order, package spelling, error/result roles, MATCH behavior, and public effects. Do not touch unrelated process runtime code.
