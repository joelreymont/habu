---
title: Specify V2 generated machine-state integrity
status: closed
priority: 1
issue-type: task
created-at: "2026-07-13T12:58:14.365191+02:00"
closed-at: "2026-07-13T13:00:22.083508+02:00"
close-reason: Specified required generated-machine-state types, emitted-CFG verification, proof layers, V2 phase mapping, and coordination dependencies; dot-dep, host, and filemap lints green.
---

Full context: MODEL-CAD-V2-PLAN.md type requirements stop at host-language stack and CAD effects, while tools/lint/clobber-lint.f shows that emitter stack effects do not type generated ARM64 GPR, SIMD, NZCV, SP, frame, label, or call state. Fix: add a required generated-machine-state section, dependency order, Kernel IR deliverables, dot mapping, and Definition of Done that connect the existing typed routine-contract, indexed operand, emitted-CFG, differential, and proof-carrying allocation dots. Also make overlapping owner/AOT dots depend on the active owner-persistence slice. Acceptance: the plan distinguishes host stack effects from generated machine effects; every prevention layer and dependency is named; no duplicate implementation dot is created; dot-dep-lint, host-lint, and filemap-lint pass.

Claim: agent=/root workspace=.jj-ws/clobber-types.
