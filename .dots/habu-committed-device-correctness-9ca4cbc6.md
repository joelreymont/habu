---
title: Committed device-correctness tests for GEMM/attention/fused kernels
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T16:53:45.385660+02:00"
---

GAP #4: the kernels in lib/ptx/cg-matmul.f (SGEMM), cg-attention.f (fused attention), the v4 fused chains, and the bandwidth bench are device-VERIFIED only via throwaway /tmp runners + python (run_matmul.py, run_attn.py, grade_habu*.sh) - NONE is reproducible from the committed tree or covered by a gate. So a regression in the emitted PTX would not be caught. FIX: a checked-Habu device-test harness (extend maki/eval-device.f's ED-RUN pattern) that, for each kernel, emits -> ptxas -> launches on the Orin -> compares a committed CPU golden (SGEMM small known A*B; attention softmax(QK^T)V; fused relu(a*x+y) with a relu-exercising input). Add to a device gate slice (Orin-only, like tools/ptx/ptxas-smoke.f). Habu-native, no /tmp python. VERIFY: the suite re-runs from the tree and fails if any kernel's PTX regresses. Deps: relates to habu-commit-checked-habu (grader) + habu-add-device-ffi (device gate plumbing).
