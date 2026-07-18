---
title: "Scheduled DDC gate: prove bootstrap chain converges"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:15:04.487589+02:00"
---

Depth review (src/habu fork): tools/ddc-verify.f is env-gated (HABU_ALLOW_BOOTSTRAP=1, line 132) and manual; codegen chain exists twice by design (native ~17K vs bootstrap/cg ~12.8K, icode pair ~fully diverged) and nothing proves the gforth recovery chain still converges except recovery day. Add scheduled DDC run (weekly or on any src/habu|src/arch|bootstrap/cg touch). Duplication itself is the trust argument — do NOT deduplicate.
