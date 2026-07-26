---
title: Lower native control flow
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:20.000633+02:00"
blocks:
  - habu-build-native-loop-71d4a638
---

Full context: lower verified control-flow SIR through LIR/A64IR using symbolic branches, layout-independent labels, and validated fixups. Acceptance: branch ranges/layout/fallthrough/terminator/one-exit invariants validate before encoding; no semantic state is recovered from bytes.
