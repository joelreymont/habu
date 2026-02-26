---
title: Cut factor/ratsimp runtime gap
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-24T18:36:11.827340+01:00\\\"\""
closed-at: "2026-02-26T13:28:24.141914+01:00"
close-reason: Restored stable JIT wins for factor/ratsimp with compiled wrappers and hotspot refresh
---

tools/maxima-hotspots current baseline after f199ec46: RCA JIT-vs-interpreter gap for factor/ratsimp and land one measured generic optimization. Scope: src/interp/vm.zig hot call/resolve paths, src/jit/backend.zig lowering if profiling proves needed, tools/maxima-hotspots evidence refresh. Add focused regression if semantics-affecting. Depends on closed trials habu-speed-up-forwarding-edeeb1ad and habu-inline-threshold-for-1269a322.
