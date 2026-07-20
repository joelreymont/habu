---
title: Wire device-LN backward + execution
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T23:45:52.296293+02:00"
---

Unowned gap surfaced by the inventory rebuild (03834011): LayerNorm's backward (LN-BWD/ROWSUM-BWD) is not device-lowered, and even forward device execution was blocked by the harness never setting the ptxas arch (the E-PTXTC-ARCH class). The arch half is now solved twice over: the ATGT probe idiom (rmsnorm/rope device tests) and the TC-GATE identity landing. Own the remainder: lower LN backward to device, execute forward+backward on the GB10 through TC-GATE, parity element-close vs host, gradcheck on-device path. Files: maki/lower/* LN legs + device test; no src/.
