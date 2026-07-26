---
title: Lower wide memory and linear values
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:39.054655+02:00"
blocks:
  - habu-lower-enums-and-9cc3fb6c
---

Full context: design Wave 6 adds multi-cell locals, fetch/store, call homes, and linear ownership under explicit layout/effect witnesses. Acceptance: width/alignment/address-space/linearity/drop/duplicate/use-after-move mutations reject; current wide memory and linear suites run through the new compiler.
