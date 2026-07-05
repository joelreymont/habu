---
title: "AD: SOFTMAX-ROWS-BWD checked + gradchecked"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:00:43.576213+02:00"
---

Capstone of ad-reverse. Build SOFTMAX-ROWS-BWD (autograd.md Worked-example) deriving dx = y*(dy - sum(dy*y)) via reverse pass + simplifier; it must pass BOTH the checker AND the device gradcheck (not merely type-check). Gradient-buffer spans MUST share the primal extent token (mint via MK-SPAN=) so len(dx)=len(y) is proven, not re-asserted at the trusted boundary (autograd.md caveat).
- Files: a maki/ or tools/ptx/ kernel + test.
- Verify: checker certifies the body against its declared parametric effect; gradcheck passes vs CPU golden; an extent-mismatch variant is rejected.
- Dep: reverse pass + VJP table + BROADCAST (landed) + simplifier (IR layer) + gradcheck harness.
