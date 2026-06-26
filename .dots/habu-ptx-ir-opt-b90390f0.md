---
title: PTX IR + opt layer (fold/DCE/CSE/peephole)
status: open
priority: 2
issue-type: task
created-at: "2026-06-26T23:59:58.998843+02:00"
blocks:
  - habu-ptx-m2-parametric-a854a419
---

EPIC (new work, untracked before review). ptx.md section 3: the self-hosted bin/hb emits machine words directly; the only optimizer is the gforth-bootstrap peephole (bootstrap/cg/opt.fs), no CSE, no strength-reduction. A general PTX IR with constant-fold/DCE/CSE/peephole is built fresh. Needed by the autograd algebraic-simplify step (autograd.md What-is-new-work: this is a PREREQUISITE of the simplifier, not part of it) so derived backwards reach closed form rather than literal reversal. Alternative: scope AD-v0 to literal reversal and make the closed-form simplifier a follow-on dot.
- Files: new src/arch/ptx/ir.f (IR node + builder), src/arch/ptx/opt.f (passes); consumes the M3-emit encoder as the lowering target.
- Verify: fold/DCE/CSE/peephole each have value fixtures with known before/after IR (docs/forth.md Encoder-factoring-needs-value-fixtures); a softmax-bwd literal reversal simplifies to the closed form dx = y*(dy - sum(dy*y)).
- Dep: M2; decompose into per-pass sub-dots when picked up. ad-reverse (habu-ptx-ad-reverse-26aebee3) simplifier blocked-by this.
