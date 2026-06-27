---
title: Add device-FFI smoke to maki gate (catch stale-bin/hb)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T15:12:57.968236+02:00"
---

Static invariant: the running bin/hb must provide every native FFI primitive the loaded lib/ffi.f needs (ffi-call, ffi-call-n, ffi-call-abi, ffi-call-abi-r) before any device tool runs; a missing primitive must fail closed early, not as a cryptic 'ffi-call-abi' at first call. CAUSE (found 2026-06-27): rebasing maki onto master's 'Support AAPCS64 FFI ABI' added the ffi-call-abi primitive (checker.f:848, used by lib/ffi.f:101); the running bin/hb predated it, so tools/ptx/bandwidth.f, maki/eval-device.f, maki/gpu.f all errored 'ffi-call-abi' while the maki gate (CPU/checker-only, no FFI) stayed GREEN and hid it. FIX: add a focused checked slice loaded by the maki gate that (a) asserts the FFI primitives lib/ffi.f requires are defined in the running engine and (b) runs a tiny cuInit/cuDeviceGet FFI smoke, failing closed with a clear 'refresh bin/hb (docs/bootstrap.md)' message. Files: new maki/ or tools/ test + maki/README.md gate list + docs/bootstrap.md note. VERIFY: pre-AAPCS64 bin/hb -> slice fails clearly; refreshed bin/hb -> passes. Deps: none.
