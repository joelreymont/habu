---
title: Gathered-GEMM golden + K=511 stride-case regression
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T13:19:18.317371+02:00"
---

docs/case-tma-stride.md payoff. CPU golden for O[m,n]=sum_k A[ix[m],k]*B[n,k] (current idiom ok; migrate to extent-typed accessors when available); wire bf16 K=511 case into GOLDEN as permanent regression — exact under every lowering the planner chooses. Can start after 'Pointer-increment gather lowering'; extends to TMA lowerings as they land.
