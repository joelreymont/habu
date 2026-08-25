---
title: Survey fork members for dlopen hazards
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T12:23:19.280350+02:00"
---

dyld is not fork-safe: a forked child that dlopens a not-yet-mapped image takes SIGBUS inside dyld (proven and fixed for the clang reference column in 48305f39; PROC-FORK:CHILD? and the E-CODEGEN-CLANG-FORK refusal shape now exist). Nothing surveys the other fork members for the same hazard: any pool member reaching lib/ffi-abi.f FFI:DLOPEN or any dlopen path on a library the gate root did not map will hit the same wall. Survey the members' require closures for dlopen reachability, and guard or pre-map each the way the reference column now is.
