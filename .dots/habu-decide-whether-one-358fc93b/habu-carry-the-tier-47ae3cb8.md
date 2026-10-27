---
title: Carry the tier 0 virtual stack across do loops
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T19:48:51.150516+02:00"
closed-at: "2026-10-27T21:23:56.169852+02:00"
close-reason: "Landed: tier 0 carries the virtual stack through do/?do/loop/+loop bodies (LVRECONAT reconciles leave to the loop's frame); tier0-inline-prims TEST-LOOPS pins 1/1 push/pop where the base spilled 4/2. Neutral alone on zz/tender run instructions (within noise); integrated stack with csetm and constant division: zz 14.202G -> 13.926G, tender 310.16G -> 281.77G. Full suite 501/501 and build-fixpoint-test ok on the integrated engine."
---

Joel 2026-10-27 ("dot and implement then"): carry tier 1's register use into tier 0 before tier 1 is deleted. Problem: do, ?do and loop/+loop are CF-ENTRY rows (src/habu/habu2.f ~12424-12425) that spill the whole virtual stack at both ends, so values live across a counted loop go through memory every iteration; begin/while/until/repeat already snapshot the VS in registers (jit.f LVSNAP ~814-869) and reconcile at the back edge (LVRECON ~871-1078). Acceptance: do/?do snapshot the VS like begin, loop/+loop reconcile at the back edge like until, leave/unloop/exit and the loop frame (x29 LIDX cache, habu2.f ~2730-2750, 2826, 2835-2897) stay correct; nested loops, i/j, leave out of nested ifs, and a call inside the body behave as today. Tests: tier0-inline-prims harness shows no VS spill at do/loop for a register-resident value; runtime-regression rows for nested loops, leave, +loop with negative step, and a call in the body. Measure zz and tender run instructions before/after. Gate: test/compile-floor-gate.f. Files: habu2.f (do/?do/loop/+loop/leave rows), jit.f. Depends: none. Ownership: tier 0 counted-loop codegen. Claim: jerry, workspace jerry-loops.
