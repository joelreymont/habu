---
title: V2 typed async DAG
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-11T12:14:25.182626+02:00\\\"\""
closed-at: "2026-07-13T17:19:53.645171+02:00"
close-reason: "Landed on master: typed async DAG schema (package ADAG nominal stream/event/node families, 6 audited prim-axiom mints; ENUM akind + LAYOUT-BUFFER typed columns; explicit-edge ordering + seal-time Kahn acyclicity; PIR-BUILD/PIR-RUN plan-IR lowering proven numerically identical to EX-RUN). Host scope; device overlap leg E1-gated under habu-v2-checked-async-8d460576. Orchestrator review: diff host-only (no engine), survives type-DSL cutover (maki green incl async-dag-test + plan-ir-test), byte-diff-lint 0, trust-lint 0, trusted-inventory strict baseline (re-owned 6 ADAG rows to resolvable epic 70b629a9), host/filemap 0"
---

Problem: MODEL-CAD-V2-PLAN.md:1419-1436 requires streams/events/dependencies as typed resources; execution is currently hidden driver sequencing. Fix: define the minimal immutable async DAG schema for kernel, copy, memset, event-record, and event-wait nodes with stream ownership and deterministic topological replay. Acceptance: use-before-ready, cross-stream missing wait, event double-destroy, and dependency cycle reject; two independent branches overlap on device. Files: maki/executor.f, maki/plan-ir.f, tools/ptx/cuda-launch.f, docs/model-cad.md. Verify: host DAG negatives, device ordering golden, maki/test.f.

Claim: agent=tasync workspace=.jj-ws/fable-tasync (host scope: maki/executor.f, maki/plan-ir.f, docs/model-cad.md; device leg E1-gated)
