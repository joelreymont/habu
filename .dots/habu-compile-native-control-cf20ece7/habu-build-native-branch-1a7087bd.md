---
title: Build native branch SSA
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:19.971850+02:00"
blocks:
  - habu-record-native-slice-a01a8ad7
---

Full context: design Wave 3 adds IF/ELSE/THEN and EXIT as explicit SIR blocks, successor arguments, one exit block, and no hidden stack snapshot. Acceptance: joins validate exact typed arguments; missing/extra/wrong-type successor values and multiple implicit exits reject; differential zero/nonzero and nested branches pass.
