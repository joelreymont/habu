---
title: "M4e: SAXPY from checked source via M3-emit"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:11:55.489892+02:00"
---

Decomposes M4. Write KERNEL: SAXPY ( span span uniform -- ) from ptx-sketch.md (x GRID-CTX; x g LOAD a SCALE; y g LOAD +.; y g STORE) and prove it CHECKS against the declared parametric effect, then lowers via the M3-emit encoder to header-complete sm_87 PTX (replacing the hardcoded string printer in src/arch/ptx/emit.f with type-driven emission for this kernel).
- Files: a tools/ptx/ or lib/ kernel + emit wiring.
- Verify: SAXPY checks clean; emitted PTX matches the saxpy-test.f contract; ptxas-smoke assembles (Orin).
- Dep: M4d + M3-emit (minimal encoder).
