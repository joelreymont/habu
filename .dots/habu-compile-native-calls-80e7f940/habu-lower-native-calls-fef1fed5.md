---
title: Lower native calls
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:20.053554+02:00"
blocks:
  - habu-lower-native-return-92993f27
---

Full context: design Wave 4 adds direct calls and typed indirect execute under explicit calling convention, clobber sets, stack homes, and symbolic targets. Acceptance: live-across-call values obey allocation validation; mismatched signatures/targets/clobbers reject; only required homes materialize.
