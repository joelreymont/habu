---
title: Reduce native FFI binding overhead
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T17:03:00.352548+03:00"
---

Otto’s blackboard report records26 FUNCTION bindings with2,604 bytes of28–172byte marshalling stubs. Determine redundant native binding work using actual FFI descriptor/ABI facts; preserve arguments/results, mixed float/int registers, spills, variadic contracts and runtime library resolution. Stub size alone is not proof of removable code. Acceptance: responsible compiler/FFI change only if a genuine shared or compact lowering wins on same-source measured artifact; focused real native FFI calls/refusals, native/convergence gates and Astra review.
