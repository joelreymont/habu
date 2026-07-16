---
title: Reject fork with linear state
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-16T04:45:09.084233+02:00\""
---

Context: src/core/checker.f models fork as an ordinary zero-input primitive. Runtime fork duplicates the whole process image, so any live DEFLINEAR value in the checked data stack or checked locals is copied into parent and child, violating linear ownership; a process-pty-handle can then be consumed twice. Fix: give fork a checker/compiler rule that rejects when any reachable live stack or local term contains a linear type, before the syscall can execute. Preserve fork only at linear-empty boundaries. Add minimal checked negatives for a direct linear stack value, an inferred/explicit linear local, and a linear composite; add a positive linear-empty fork fixture; keep runtime handle owner-PID validation as defense in depth under the PTY dot. Acceptance: exact fail-closed diagnostics, checker/source/bootstrap parity, focused linear/process regressions, typed lint, native gates.

Expanded soundness scope: persist nonlinear type-variable and row-tail constraints in certified effects so wrappers cannot launder live data-stack state; persist and propagate a may-fork effect through wrappers, recursion, aliases/EXPORT, and nested/returned quotations so every actual application constrains caller-live return-row state and locals. Certification must preserve the exact verified constraints rather than reparse them away. Cover direct and two-level wrappers, open data/return tails, late-bound generic and layout arguments, concrete/late locals, direct/returned/generic quotation execution, alias/export, failed-overload rollback, owner-PID defense, and a positive wrapper that consumes its linear input before fork without over-rejecting it.

Claim: agent=checker_loader_fix workspace=.jj-ws/habu-checker-reject-loader-body.
