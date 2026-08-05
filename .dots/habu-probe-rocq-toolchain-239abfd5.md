---
title: Probe Rocq toolchain before parity gate
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T00:30:43.268110+02:00"
---

Full context: test/compiler/ir-id-proof.f shells out to the rocq proof assistant with no capability probe — ROCQ-RUN asserts exit 0 from /usr/bin/env rocq — so on a host without the toolchain the identity parity gate fails outright rather than skipping. Because of that it cannot be scheduled into test/run.f and currently sits in suite-coverage-lint's documented manual-gate table, running only in the standalone stdlib merge gate. Give it a capability probe with a recorded skip, matching the CUDA:OPEN? device-SKIP pattern already used for GPU tests. Acceptance: on a host without rocq the suite prints a recorded skip and stays green; on a host with rocq it runs and still fails when a theorem statement is trivialised; once probed, the file leaves the manual-gate table and is scheduled normally.
