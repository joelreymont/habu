---
title: Add compiler capability record
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:56.925238+02:00"
blocks:
  - habu-inventory-compiler-emission-badfd25c
---

Full context: design Wave 0 and shadow mode require an explicit disabled new-compiler capability plus named unsupported coverage. Add the immutable capability record and production-readable disabled state without routing compilation through the new backend. Acceptance: default is disabled; unknown capability cannot silently enable; capability identity/version is deterministic; production artifact bytes are unchanged.
