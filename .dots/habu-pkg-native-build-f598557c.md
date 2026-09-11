---
title: Package native build support
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:27:41.047443+02:00"
---

Current master has 633 active package-less definitions across native build/runtime support. The original frozen census covered 438: sha256.f 84, image-bytes.f 64, linux/elf.f 89, driver-io.f 35, debug-watch.f 18, aot-lib.f 74, snap-lib.f 42, and treeshake.f 32. A currentness pass adds profiler emitter 21, cold-prefix xref 88, AOT-seeded repl 43, stepper 20, and breakpoint debugger 23. These are cold bootstrap, fixpoint, AOT, snapshot, REPL/debugger, and build dependencies; mutable cursors, scratch buffers, trusted casts, scanners, and generic names remain globally findable. Runtime/AOT module prefixes consume persisted dictionary bytes without privacy; profiler-emitter helpers instead bloat only the build host and must be measured separately. This controller owns the complete production census and small module leaves. End state: explicit SHA256, IMAGE, ELF, DRIVER-IO, WATCH, AOT-LINK, SNAPSHOT, TREESHAKE, PROF-EMIT, XREF, DICT-LIFECYCLE, REPL, and BREAKPOINT packages; only documented entry APIs and intentional global product commands remain public; no compatibility globals; stage-0 bootstrap mirrors remain the documented package-less recovery boundary. Acceptance: old globals and qualified private helpers reject, public APIs resolve through cold bootstrap/Gforth recovery/fixpoint/AOT/snapshot/warm-snapshot/debugger paths, byte outputs remain exact, package records survive AOT/snapshot restoration, build-host versus shipped dictionary-name/JIT/DATA/CODELEN measurements are not conflated, and every focused plus full native gate passes. Do not dispatch until child dependency/write sets and bootstrap mirror effects are explicit.
