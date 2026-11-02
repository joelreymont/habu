---
title: Start the Habu tier 0 compiler
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T22:53:56.832400+02:00"
closed-at: "2026-11-02T11:34:09.222742+02:00"
close-reason: "Landed 5f4df01bd8 'Start the Habu tier 0 compiler' with its children (4c2b576bde, 007d6032a1, 9c0caa1132, 4419bc533d); the x86 emitter followed in 27e3cf87."
---

Port lane L1 commit 1 (design jerry-aot/.out/notes/fable-t0-l1-design.md): src/habu/t0.f token walk over body text with a spill-at-boundaries virtual stack and src/habu/t0-x64.f emitter for straight-line code and structured control; scratch harness times T0 walk+emit per definition against the tier 0 JIT load. The timing decides the x86 plan. Claim: jerry, workspace jerry-t0.
