---
title: macOS FFI runtime proof
status: open
priority: 2
issue-type: task
created-at: "2026-06-29T09:26:15.893051+02:00"
---

Gap: lib/ffi.f depends on DLOPEN-SLOT/DLSYM-SLOT, but src/os/macos/layout.f does not define them and src/os/macos/macho.f currently emits libSystem only as a load dylib with zero imported symbol slots. lib/ffi-test.f also hardcodes libc.so.6/libm.so.6, so local macOS cannot prove the AAPCS64 FFI runtime or macOS API calls. Fix: split target-independent ABI/marshalling JIT tests from dynamic-loader tests; add macOS dyld-backed dlopen/dlsym slots or an equivalent first-symbol bootstrap for libSystem; add macOS fixture using /usr/lib/libSystem.B.dylib or RTLD_DEFAULT-compatible APIs; keep CUDA Driver proof on zed/Linux.
