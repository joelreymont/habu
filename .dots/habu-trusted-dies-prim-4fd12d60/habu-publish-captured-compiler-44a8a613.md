---
title: Publish captured compiler paths at cold boot
status: closed
priority: 1
issue-type: task
created-at: "2026-09-14T12:49:00.087648+03:00"
closed-at: "2026-09-14T13:08:42.866200+03:00"
close-reason: Cold compiler paths now publish before include freeze. Fresh rebuilt partial engine passes no-op repeated compiler/IR requires and native-defer; full native product passes two application-image generations with source-free compiler requires. Partial recovery image is not the full application runtime.
---

Owner cedar. Fresh partial product after 9d0a44a3 runs native CASE, but require src/compiler/native/compiler.f reloads captured IR-ID and exits84. ENGINE-PROVIDES? is false for compiler.f and ir/id.f; PFX-CHAIN:ROWS has no caller, while TABLE still emits the 45 captured paths. Restore the rows before REQUIRE-BOOT-FREEZE on every cold source path. Acceptance: require compiler is a no-op, native defer suite, repeated load and snapshot preserve registry; full native product remains supported.
