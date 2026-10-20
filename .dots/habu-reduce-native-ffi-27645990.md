---
title: Reduce native FFI binding overhead
status: active
priority: 2
issue-type: task
created-at: "2026-10-14T17:03:00.352548+03:00"
---

Otto’s blackboard report records26 FUNCTION bindings with2,604 bytes of28–172byte marshalling stubs. Determine redundant native binding work using actual FFI descriptor/ABI facts; preserve arguments/results, mixed float/int registers, spills, variadic contracts and runtime library resolution. Stub size alone is not proof of removable code. Acceptance: responsible compiler/FFI change only if a genuine shared or compact lowering wins on same-source measured artifact; focused real native FFI calls/refusals, native/convergence gates and Astra review.

The rebuilt5b93a9ee engine (7cc7f9be/map95f25745) reduces nine matched bindings from1104 to856 native code bytes: six lifetime bindings548 to336, and three mixed/fixed/variadic f32 bindings556 to520. Both engines pass the existing native lifetime calls and the same C-peer f32 probe, including rounding, negative zero and mixed registers. This qualifies those shapes; complete integrated FFI/native/convergence gates remain pending.

Helper-inclusive same-source ffi-app.f artifact comparison: retained baseline14400 to current21b4/573305bb13756 code bytes, DATA838 unchanged, signed executable49788 unchanged because padding absorbs644 saved code bytes. The included I32-RESULT helper is36 bytes including ret. Both images execute ffi-artifact:ok and pass signature verification; existing ABI/lifetime suites pass. Baseline donor identity/map are unavailable, so this does not isolate ea6a59a1 or establish signed-image savings.
