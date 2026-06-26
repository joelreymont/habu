---
title: "M2f: self-host fixpoint rebuild plus M2 negatives"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:03:30.851848+02:00"
blocks:
  - habu-m2e-kernel-defining-ff32435f
---

Part of PTX M2. Refresh native bin/hb so parametric tokens are accepted (new tokens need a checker-only bootstrap stage, docs/forth.md), reach byte-for-byte fixpoint, add the M2 negative suite. Rebind HOOK across checker.f/render.f/check-hook.f together (LESSONS.md). Closing M2f closes the M2 epic; M4 (habu-ptx-m4-tile) is the downstream consumer.
- Files: tools/build-fixpoint*.f path; new tools/ptx-check-test.f.
- Verify: fixpoint byte-for-byte; native gate passes; M2-class negatives reject.
- Dep: M2a-M2e.
