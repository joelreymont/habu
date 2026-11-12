---
title: Say a Wasm build cannot load a native-only file
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T18:05:04.127101+02:00"
---

Problem: a Wasm app build of a program that requires src/lib/fs.f stops at src/lib/ffi-abi.f with "'FFI' is sealed" (rc 84) after the boundary seal, and with E-UNDEFINED DLOPEN-SLOT (ffi-abi.f:200) before it; neither says that the file is native only. Acceptance: such a build is refused with a line naming the file and the target, before it compiles any of the file. Depends: none. Claim: unassigned.
