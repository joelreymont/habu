---
title: Build staged native compiler
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:09.586192+02:00"
blocks:
  - habu-compile-native-straight-f5e32927
  - habu-compile-native-control-cf20ece7
  - habu-compile-native-calls-80e7f940
  - habu-compile-native-language-b47a852f
  - habu-compile-native-wide-79743b60
  - habu-cut-over-native-984a5241
  - habu-self-host-staged-520ab588
---

Full context: design sections 7 and 14 Waves 2-8 replace native direct emission with source tape through HIR, SIR, LIR, A64IR, allocation, layout, and HBOBJ. Required result: production-shaped vertical waves, isolated shadow execution, AOT/object cutover, self-host fixpoint, and old-path deletion. Acceptance: design section 21 native criteria pass.
