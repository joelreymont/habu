---
title: Lower native typed locals
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:20.027236+02:00"
blocks:
  - habu-compare-native-control-f8b5d801
---

Full context: design Wave 4 lowers typed locals into SSA aliases or explicit homes only when address/lifetime requires storage. Acceptance: immutable locals emit no traffic, mutable/escaping locals have typed homes, scope/type/stale-home mutations reject, and differential fixtures pass.
