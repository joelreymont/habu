---
title: Compile a tier-1 call to a word whose effect is unrecorded
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T19:48:39.613674+02:00"
---

Problem: at 1 set-tier, a TRUSTED: or checked caller of a word whose inferred effect the checker did not record (W-EFFECT-NOT-RECORDED: more than 23 type variables or an unmodeled type tag) dies E-NCOMP-BINDING -8575 uncaught, rc 67: src/compiler/native/hir-word.f RESOLVE-SITE (~:914) throws BINDING-RC when the callee row has no bound effect. T0 compiles the same call (lane wide: checker-owner UNRECORDED-IN/-OUT fields carry the callee's cell arity). Repro /tmp/jerry-big/gaps/unrecorded-callee-t1.f. Acceptance: tier 1 reads the same unrecorded arity fields T0 reads (one shared reader, no second rule) and runs the repro printing the 30 values as T0 does; a regression row beside test/diag-position-test.f's wide-caller row. Depends: T0 release (lane wide landed). Claim: unassigned.
