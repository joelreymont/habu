---
title: Package profiler emitter
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:49:40.093931+02:00"
---

Current master package census: src/habu/prof.f:5-123 exposes 21 unowned build-time emitter definitions, six longer than the 16-byte inline-name limit and 275 total definition-name bytes. These definitions live in the metacompiler host rather than the produced runtime dictionary; only the emitted prof-on and prof-report primitives are baked, so package work must not claim shipped-name shrink. Wrap the source in package PROF-EMIT. Export only PRIMS for the habu2.f primitive-publication site and RUNTIME for the dump/handler emission phase; keep labels, constants, signal-frame helpers, target-specific ucontext emitters, and primitive bodies private. The two runtime primitive names remain the documented global product surface. Preserve exact emitted machine bytes, signal ABI behavior, runtime size map, fixpoint, and stage-0 bootstrap/cg/prof.fs package-less recovery mirror. Add old-global/private rejection and qualified phase positives. Measure build-host dictionary-name/JIT/DATA cost, build latency, emitted CODELEN, and runtime behavior before/after without attributing host-only names to bin/hb. Serialize with habu-bound-profiler-counter-235c5f48, habu-idx-profiler-pc-45b4c841, and habu-make-profiler-sample-4df2965e. Verify profiler/signal/bootstrap/fixpoint/both-target/full native and package/host/dot gates.
