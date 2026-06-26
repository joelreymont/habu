---
title: "PTX M3: toolchain spike (no checker)"
status: active
priority: 1
issue-type: task
created-at: "\"2026-06-25T13:43:16.909930+02:00\""
blocks:
  - habu-ptx-m1-c-1df1d6e7
---

docs/ptx-sketch.md M3. Minimal PTX encoder under src/arch/ptx/; hand-built IR -> header-complete saxpy.ptx -> ptxas -arch=sm_87 -> run on Orin via the M1 harness -> CPU golden. New ISA encoder (shares none of src/arch/arm64/).
