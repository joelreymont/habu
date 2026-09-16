---
title: Fuse tier-1 data-stack moves in the prune pass
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T19:47:48.895777+03:00\""
---

Problem: after the word-frame lane (7e71385e) tier 1 still compiles a data-stack publish as `str x0,[x19]` + `add x19,x19,#8` (dstore + dpublish) and a pop as sub + ldr: 8,518 fusable pairs in the engine image, about 34 KB. The lane measured and designed it and stopped: the frame fusion could live in the emitter because a frame is a per-function fact, but a stack move is a per-pair fact and OP-INSNS sees one op with no neighbour; the right home is src/compiler/native/prune.f (the pass formerly named combine.f; the build-once lane renamed it when the fold moved into selection), which runs before allocation while the data-stack offsets are contract-fixed, with one new IR opcode and its tables. Acceptance: combine emits the post-indexed store and pre-indexed load for adjacent publish/store and pop/load pairs through a new A64IR opcode; the frame fusion moves from the emitter into the same pass (one owner, A64FRAME:FUSED? stays the bound); tools/tier-census.f (both spellings) shows the sp columns drop by the pair count and bytes accordingly; tools/tier-bench.f unchanged or better; byte fixpoint (two-generation); the native-* suites and test/run.f green. Files: src/compiler/native/prune.f, a64ir.f, select.f, emit.f, frame.f, test/compiler/. Verify: census before/after; tier-bench; tools/native-build.f fixpoint; test/run.f. Depends: none (the build-once lane landed 2026-09-16 as fccb4a02; the word frame landed as 0b023ec2). Ownership: tier-1 prune pass. Claim: agent=hazel-fuse workspace=.jj-ws/hazel-fuse
