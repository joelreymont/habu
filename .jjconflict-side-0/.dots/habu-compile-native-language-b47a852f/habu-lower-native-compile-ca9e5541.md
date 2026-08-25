---
title: Lower native compile-time words
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:39.015040+02:00"
blocks:
  - habu-lower-native-defining-c35914de
---

Full context: design Wave 5 adds string literals, POSTPONE, and modeled immediate computation through sealed HIR-builder capabilities. Acceptance: registered front-end intrinsics and checked computations produce frozen HIR; unmodeled or AArch64-reaching immediates reject with named capabilities; shadow corpus passes.
