---
title: Lower native exceptions
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:20.066896+02:00"
blocks:
  - habu-lower-native-calls-fef1fed5
---

Full context: design Wave 4 adds throw/catch/evaluate edges to the IR and definition transaction. Acceptance: caught errors are testable in-process where safe; every failure releases modules/objects, restores dictionary/data/code marks, and publishes no half-definition; nested exception differentials pass.
