---
title: Carry the tier 0 virtual stack across do loops
status: active
priority: 2
issue-type: task
created-at: "2026-10-27T19:48:51.150516+02:00"
---

Joel 2026-10-27 ("dot and implement then"): carry tier 1's register use into tier 0 before tier 1 is deleted. Problem: do, ?do and loop/+loop are CF-ENTRY rows (src/habu/habu2.f ~12424-12425) that spill the whole virtual stack at both ends, so values live across a counted loop go through memory every iteration; begin/while/until/repeat already snapshot the VS in registers (jit.f LVSNAP ~814-869) and reconcile at the back edge (LVRECON ~871-1078). Acceptance: do/?do snapshot the VS like begin, loop/+loop reconcile at the back edge like until, leave/unloop/exit and the loop frame (x29 LIDX cache, habu2.f ~2730-2750, 2826, 2835-2897) stay correct; nested loops, i/j, leave out of nested ifs, and a call inside the body behave as today. Tests: tier0-inline-prims harness shows no VS spill at do/loop for a register-resident value; runtime-regression rows for nested loops, leave, +loop with negative step, and a call in the body. Measure zz and tender run instructions before/after. Gate: test/compile-floor-gate.f. Files: habu2.f (do/?do/loop/+loop/leave rows), jit.f. Depends: none. Ownership: tier 0 counted-loop codegen. Claim: jerry, workspace jerry-loops.
