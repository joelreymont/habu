---
title: tools run their CLI on ambient argv at file scope
status: open
priority: 2
issue-type: task
created-at: "2026-08-23T12:14:26.142802+02:00"
---

Problem: tools/imgdump.f, tools/imagedisasm.f and tools/ptx/perf-regress.f end in 'SCRIPT-ARGC 0 > if MAIN then' at file scope, so requiring any of them into an image that carries script argv runs the CLI against that argv (measured 2026-08-23: rc 74 'imgdump: stat failed', rc 64 usage, under both test/run.f -- --under bin/hb and gate-runner-entry -- <group>); a tool that is not a library cannot be scheduled into a resident list, which is why their five suites had to be spawned (SUITE-TAIL-PROCESS?, +5.8 s on the gate's critical path). Acceptance: each tool split into a library file with no load-time side effect and an explicit entry file (the tools/*-main.f or named-load-guard shape build-fixpoint.f:2080 uses); the suites return to resident lists; schedule-lint 0 findings; the three spawned registrations retired. Files: the three tools, their tests, test/gate-stdlib-*.f. Verify: gate-runner-entry -- lint-artifacts-fast green with --under; schedule-lint. Depends: habu-schedule-lint-counts-9eaac4d2 (landing). Ownership: tooling. Claim: unassigned.
