---
title: Compile a tier-1 call to a word whose effect is unrecorded
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T19:48:39.613674+02:00"
closed-at: "2026-11-04T11:48:49.843591+02:00"
close-reason: "Census 2026-11-04: goes with habu-delete-tier-1-f5c44fb4, T0 compiling every target, tier 1 and the JIT kept only for experiments. Tier-1 hir-word.f RESOLVE-SITE defect at 1 set-tier; T0 compiles it (e1016d24)."
---

Problem: at 1 set-tier, a TRUSTED: or checked caller of a word whose inferred effect the checker did not record (W-EFFECT-NOT-RECORDED: more than 23 type variables or an unmodeled type tag) dies E-NCOMP-BINDING -8575 uncaught, rc 67: src/compiler/native/hir-word.f RESOLVE-SITE (~:914) throws BINDING-RC when the callee row has no bound effect. T0 compiles the same call (lane wide: checker-owner UNRECORDED-IN/-OUT fields carry the callee's cell arity). Repro /tmp/jerry-big/gaps/unrecorded-callee-t1.f. Acceptance: tier 1 reads the same unrecorded arity fields T0 reads (one shared reader, no second rule) and runs the repro printing the 30 values as T0 does; a regression row beside test/diag-position-test.f's wide-caller row. Depends: T0 release (lane wide landed). Claim: unassigned.
