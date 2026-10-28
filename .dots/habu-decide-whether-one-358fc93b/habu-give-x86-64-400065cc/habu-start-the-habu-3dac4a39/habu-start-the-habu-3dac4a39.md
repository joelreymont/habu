---
title: Start the Habu tier 0 compiler
status: active
priority: 2
issue-type: task
created-at: "2026-10-27T22:53:56.832400+02:00"
---

Port lane L1 commit 1 (design jerry-aot/.out/notes/fable-t0-l1-design.md): src/habu/t0.f token walk over body text with a spill-at-boundaries virtual stack and src/habu/t0-x64.f emitter for straight-line code and structured control; scratch harness times T0 walk+emit per definition against the tier 0 JIT load. The timing decides the x86 plan. Claim: jerry, workspace jerry-t0.
