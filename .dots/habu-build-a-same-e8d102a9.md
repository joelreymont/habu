---
title: Build a same-machine engine for another OS without refusing it
status: open
priority: 1
issue-type: task
created-at: "2026-10-30T11:09:35.214688+02:00"
---

tools/native-build.f -- <out> --target linux-aarch64 on a macOS arm64 engine stops with rc 74 'native-build: target process ABI or image format is not executable here' (OUTPUT-LAUNCH-CK, tools/native-build-core.f:904, from the Intel merge 4a5faaae; reached through WRITER-MACHINE-CK in FILE-PREPARE and in IMAGE-INSTALL when FOREIGN? is false, i.e. the same architecture). docs/architecture.md 'Cross-compilation' and docs/xcomp.md 'Build target' say any hb builds hb for every required host and that building for a platform never runs on it; a foreign-architecture product is already built without a launch. Master's design wins over the Intel guard: a product the host cannot launch is built and written, and only SMOKE is skipped for it (as it is for a foreign architecture); the refusal goes. Proof: the linux-aarch64 engine cross-built on the Mac from master runs on spark (echo bye → E-UNDEFINED rc 70, plus a test row on the copied tree); the Mac's own engine build still smokes. Route for the W^X spark rows (carl-wx) and any Linux engine while habu-restore-gforth-recovery-618de7d1 is open.
