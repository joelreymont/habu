---
title: V2 quantized plan schema
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T12:14:25.480944+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:1471-1482 requires INT8/INT4 and weight-only plans with explicit semantics. Fix: add one INT8 plan schema carrying scale, zero-point, calibration digest, saturation, rounding, packing, accumulator domain, and dequant placement; no kernel optimization yet. Acceptance: missing calibration or mismatched packing rejects; every field participates in hash/serialization; FP and INT8 artifacts cannot collide. Files: maki/precision.f, maki/plan-ir.f, maki/artifact-store.f. Verify: codec/hash mutations and checker negatives.
