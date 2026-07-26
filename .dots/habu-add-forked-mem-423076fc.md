---
title: Add forked memory fault injector
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T22:16:01.349558+02:00"
---


Why: two accepted MAJOR reviews and the frozen fatal-release contract all require forcing memory allocation and release failures honestly; no injector exists (gpt2-alloc-test.f:96 admits it). The mechanism survey (2026-07-26) proved name interposition is refused fail-closed at three engine layers by design and MEM's OS route is two primitives with no seam, so the seam is created deliberately, using the tree's sanctioned execution-vector pattern (docs/forth.md: typed defer words; production precedent lib/process.f PROC-REAP-ARM).

Design: lib/memory.f declares defer MEM-MMAP ( n n n n n n -- n ) and defer MEM-MUNMAP ( ptr u8 n -- n ), installed once at load with the real primitives; MEM-MMAP-RC and RELEASE-BYTES route through them; nothing else in the file changes shape; cost one indirect branch per syscall. The unarmed default IS the real primitive, fixture-proven. FROZEN INTERFACE: two wrappers in package MEM-FAULT (lib/test/mem-fault.f) matching SUBJECT:RUN with nth prepended - WITH-ALLOC-FAULT ( nth src/u8 out/cap err/cap timeout -- out-len err-len outcome ) and WITH-RELEASE-FAULT with the identical shape. Each arms the defer, calls SUBJECT:RUN (lib/test/subject.f - forked child with cleared handler cells, the catch-bypass shape), RESTORES the real primitive on every parent exit path, and never returns an unowned stderr pointer; outcome asserts via lib/test/outcome.f. Arming lives ONLY in that boundary file, referenced by no production load list; arming from outside the boundary fails closed, fixture-proven. Forbidden: conditionals, env flags, or mode flags anywhere in production MEM words - the one unconditional typed execution vector pair IS the permitted design; value heuristics; a scoped primitive-interposition compiler capability (rejected - it reopens the spoof vector the engine defends at three layers).

Acceptance: forcing allocation failure observes a real caller error path through a production entry (WSTORE:BUFFER-NEW's RB-STEP leg, weight-store.f:513 - the branch the file's own comment admits no test can force); forcing release failure observes today's propagated throw, labeled to flip to the fatal exit when habu-make-owned-release-79de2b5c lands (the MEM-MUNMAP vector is where its fatal branch will live - keep the failure path structured so the flip is a small local change); arming-outside-boundary fixture fails closed; unarmed-default-is-primitive fixture; engine byte-identity proven by fixpoint rebuild sha compare; focused suites green; both diff lints; TRUSTED.md rows in the same commit. Claim: agent=meminj workspace=.jj-ws/habu-mem-injector
