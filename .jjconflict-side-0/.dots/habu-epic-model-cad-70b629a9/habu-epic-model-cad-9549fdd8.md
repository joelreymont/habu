---
title: "EPIC: Model CAD V2 competitive execution"
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:13:16.657993+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:1354-1558 defines the execution program required to move from checked-compiler architecture to measured Triton/PyTorch substitution. Fix: coordinate compute backend, async runtime, end-to-end fusion, dynamic multiversioning, numeric domains, training, deployment, compile UX, and the standing comparison matrix without duplicating existing owners. Acceptance: every section has a bounded implementation dot or explicit dependency on an existing owner; claims are exact-keyed and independently reproducible; delivery follows section 22.11. Files: MODEL-CAD-V2-PLAN.md:1354-1558, docs/model-cad.md, docs/eval-triton.md, maki/, lib/ptx/, tools/ptx/. Verify: dot tree, dot ready, tools/dot-dep-lint.f.
