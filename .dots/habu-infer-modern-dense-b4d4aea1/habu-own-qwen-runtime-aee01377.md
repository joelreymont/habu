---
title: Plan Qwen runtime storage
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:48.558758+02:00"
blocks:
  - habu-add-qwen-model-bf23d2ff
  - habu-validate-qwen-tensor-0fba9ad6
---

Why: Qwen construction needs exact weight, activation, descriptor, logit, and workspace extents before allocation. Interface: package-private DEVRT:PLAN-QWEN takes validated qwen2 MDLCFG and QWENTENSOR layout authority and returns planned(qplan) or refused(plan-error); qplan contains only checked host/device extents and buffer offsets. Owner: Qwen extent derivation only. Production red: no checked Qwen allocation plan exists. Acceptance: every weight and working extent and offset matches the validated geometry; zero, one-over, sum, product, and alignment overflow reject before allocation. Forbidden: allocation, core, qbuild, BEGIN-QWEN, DROP-QWEN, footprint owner, kernel module, public plan, fixed cap, or alternate geometry. Smallest owning check: bin/hb --load maki/infer/qwen-runtime-plan-test.f.
