---
title: V2 typed async DAG
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.182626+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:1419-1436 requires streams/events/dependencies as typed resources; execution is currently hidden driver sequencing. Fix: define the minimal immutable async DAG schema for kernel, copy, memset, event-record, and event-wait nodes with stream ownership and deterministic topological replay. Acceptance: use-before-ready, cross-stream missing wait, event double-destroy, and dependency cycle reject; two independent branches overlap on device. Files: maki/executor.f, maki/plan-ir.f, tools/ptx/cuda-launch.f, docs/model-cad.md. Verify: host DAG negatives, device ordering golden, maki/test.f.
