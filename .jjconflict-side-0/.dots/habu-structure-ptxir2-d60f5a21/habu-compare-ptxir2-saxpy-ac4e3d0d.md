---
title: Compare PTXIR2 SAXPY shadows
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:47.977112+02:00"
blocks:
  - habu-ptx-register-pressure-ed521b40
---

Full context: GPU Wave A routes a bounded checked SAXPY subset through PTXIR2 and compares old/new PTX assembly and device behavior under explicit policy. Acceptance: ptxas, launch, sentinel, output, resources, performance, coverage, renderer, and external-parser roundtrip pass; old path remains publisher and unsupported ops are named.
