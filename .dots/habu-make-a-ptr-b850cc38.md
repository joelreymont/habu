---
title: "Make a pointer definer's read as cheap as a bare cell"
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-17T07:05:08.943677+03:00\""
---

Problem: reading a PERSISTED-PTR-VARIABLE is a call into a create/does> body and then a call to ptr-field, and on the compiler's hot path that definer's does> body alone was 8.9 percent of a trivial tier-1 definition's samples (48 percent from RD@); the offset-handle lane (630dc6e5) had to keep the arena's region base in a bare `create` cell marked with ptr-cell-mark instead, a measured exception to docs/forth.md's rule, documented at the definition in src/compiler/ir/arena.f. Acceptance: a read of a PERSISTED-PTR-VARIABLE (and of the other create/does> pointer definers in lib/) compiles to the one load and ptr-field the bare cell costs, with no call frames, at tier 0 and tier 1 (an immediate fold or inlining of the does> body: the identity-fold dot 7318ac43 and the inline-prims work are the neighbours); the arena's exception reverts to the definer with no measured loss; tools/tier0-profile.f and the sampler show the does> body gone from the hot path. Files: src/habu/habu2.f, src/habu/jit.f, src/compiler/native/select.f, lib/ptr-variable definers, src/compiler/ir/arena.f, test/. Verify: perf stat per trivial definition as docs/compiler-measurements.md section 6; census; fixpoint. Depends: none. Ownership: definer lowering. Claim: agent=hazel-ptrread workspace=.jj-ws/hazel-ptrread
