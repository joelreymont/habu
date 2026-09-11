---
title: Lower integer LIR
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:57:03.054981+02:00"
blocks:
  - habu-optimize-straight-line-19a6e5d9
---

Full context: design section 7.5 and Wave 2 require target-neutral integer LIR for literals, add/sub/mul/bitwise, resolved calls allowed by the slice, and return. Preserve symbolic values, stack homes, effects, and target contracts. Acceptance: LIR verifier rejects illegal widths/effects/target operations; canonical fixtures bind to optimized SIR. Dependency: straight-line SIR optimization.
