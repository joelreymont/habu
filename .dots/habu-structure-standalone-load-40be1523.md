---
title: Structure standalone load results
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:49:20.810490+02:00"
---

Current master defect: test/stdlib-standalone-load.f:37-43 stores module paths as parallel PATH-OFF/PATH-LEN arrays and immediately explodes typed process outcome into independent RC/EXITED globals. PATH$ at 86-87 can interchange same-cell offset and length. STORE! at 107-113 reconstructs impossible bool+code combinations and erases the distinction between signaled and timeout before LOADS asserts only EXITED and RC. Define a checked STRUCTURE arena-span stored in one LAYOUT-BUFFER for collected paths. Retain and exhaustively MATCH the payload process outcome per child, or define a STRUCTURE load-result containing path, outcome, and capture lengths without bool/code reconstruction. Make collection transactional and validate every index. Preserve discovered module order/set, exact child argv/stdin/capture behavior, gate diagnostics, and runtime. Prove compile-negative offset/length and outcome/code swaps; exact-capacity/canaries and reset/reuse; exited/signaled/timeout with exact diagnostics and original codes/signals; malformed child/capture failures; current discovered-module parity. Measure source/JIT/DATA/CODELEN, path storage, child capture sizes, and gate runtime before and after. Coordinate habu-pkg-remaining-30-99dbf693, which consumes this gate but does not own its representation.
