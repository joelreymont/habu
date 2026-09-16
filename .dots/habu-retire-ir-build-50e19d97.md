---
title: Retire IR-BUILD rows from the context teardown
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T21:42:34.718635+03:00"
---

Problem: IR-BUILD's rows are not retired at context teardown: IR-ARENA owns the single IR-CTX:RETIRE-CHILDREN! vector and a second install is refused by name, so IR-BUILD keeps an owner probe and a sweep of its own (FIND-B: 1,105 calls and 2,688 scan steps per trivial definition, about 1 percent; scratch lane, 2026-09-16). Acceptance: IR-CTX offers retirement to more than one child registry (a small table of vectors, or one vector IR-ARENA chains to), IR-BUILD registers its own retirement and drops its owner probe and sweep the way arena.f did in 30df3a77, with the invariant tested at capture; measured per-definition instructions before and after; suites and fixpoint. Files: src/compiler/ir/context.f, build.f, arena.f, test/compiler/ir-build.f. Verify: perf stat as docs/compiler-measurements.md section 6; test/compiler suites; fixpoint. Depends: none. Ownership: IR context. Claim: unassigned.
