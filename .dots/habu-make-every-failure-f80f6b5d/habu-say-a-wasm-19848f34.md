---
title: Say a Wasm build cannot load a native-only file
status: closed
priority: 2
issue-type: task
created-at: "2026-11-12T18:05:04.127101+02:00"
closed-at: "2026-11-12T20:30:40.112463+02:00"
---

Problem: a Wasm app build of a program that requires src/lib/fs.f stops at src/lib/ffi-abi.f with "'FFI' is sealed" (rc 84) after the boundary seal, and with E-UNDEFINED DLOPEN-SLOT (ffi-abi.f:200) before it; neither says that the file is native only. Acceptance: a file that builds for some targets only states them on its first code line, `HB-TARGET-MACOS? HB-TARGET-LINUX-KERNEL? or ?TARGET` (src/core/include.f, beside ?ENGINE-PROVIDES), and the files requiring it state nothing; on another target the load stops there with `E-TARGET habu: <path>:<line>: the file does not build for <target>` and throws 74, the target named by each seam's HB-TARGET$ (src/os/*/target.f; Joel named both). Depends: none. Claim: agent=jerry-worker workspace=.jj-ws.noindex/jerry-target.
