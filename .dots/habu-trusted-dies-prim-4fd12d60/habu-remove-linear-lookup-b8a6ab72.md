---
title: Remove linear lookup cost from native compilation
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-11T09:05:33.547563+03:00\\\"\""
closed-at: "2026-09-11T09:37:30.294125+03:00"
close-reason: Direct validated context/arena slot lookup is integrated. Focused lifetime, builder and application-image checks pass; the same isolated F64 workload improved from 65.87s to 16.97s. Combined full-suite verification remains tracked by the existing integration dot.
---

Owner: pointer_review investigates; assign bounded implementation after representation is confirmed; Cedar integrates. Ownership: src/compiler/ir/arena.f, context.f and only measured dependent lookup code, plus their focused regressions. F64 loading samples identify FIND-A and generation scans as the largest cost, with context/digest lookup secondary. Fix the responsible lookup representation while preserving handle identity, release/reuse, stale-handle rejection and image relocation. Acceptance: existing arena/context lifetime and native compiler regressions pass and the same isolated F64 load shows materially lower elapsed time; no timing ratchet or repeated compilation cache framework.
