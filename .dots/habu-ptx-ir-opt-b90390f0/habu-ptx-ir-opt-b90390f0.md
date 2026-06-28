---
title: PTX IR + opt layer (fold/DCE/CSE/peephole)
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-26T23:59:58.998843+02:00\""
---

EPIC (new work, untracked before review). ptx.md section 3: the self-hosted bin/hb emits machine words directly; the only optimizer is the gforth-bootstrap peephole (bootstrap/cg/opt.fs), no CSE, no strength-reduction. A general PTX IR with constant-fold/DCE/CSE/peephole is built fresh. Needed by the autograd algebraic-simplify step (autograd.md What-is-new-work: this is a PREREQUISITE of the simplifier, not part of it) so derived backwards reach closed form rather than literal reversal. Alternative: scope AD-v0 to literal reversal and make the closed-form simplifier a follow-on dot.
- Files: new src/arch/ptx/ir.f (IR node + builder), src/arch/ptx/opt.f (passes); consumes the M3-emit encoder as the lowering target.
- Verify: fold/DCE/CSE/peephole each have value fixtures with known before/after IR (docs/forth.md Encoder-factoring-needs-value-fixtures); a softmax-bwd literal reversal simplifies to the closed form dx = y*(dy - sum(dy*y)).
- Dep: M2 is landed; decompose into per-pass sub-dots when picked up. ad-reverse (habu-ptx-ad-reverse-26aebee3) simplifier blocked-by this.

2026-06-30 local checkpoint: first child slice landed the checked value layer in `lib/ptx/ir.f` (library path, because the optimizer is consumed by PTX/AD libraries rather than target text emission directly). It now has structure-record nodes, value numbering, constant fold, peephole canonicalization, CSE, and DCE live marking with static fixtures in `lib/ptx/ir-test.f`. Remaining parent work: lowering/rewrite integration with the AD simplifier and the softmax closed-form proof; no zed/device work was attempted.

2026-06-30 local checkpoint: child `habu-ptx-ir-softmax-2d981327` added distinct input symbols plus block-algebra nodes (`PTXIR-BSUM`, `PTXIR-BSUB`) and a value fixture for `dx = y * (dy - sum(dy*y))`. Remaining parent work: connect the AD emitter/rewrite pass to this IR and lower/render optimized kernels; no zed/device work was attempted.

2026-07-01 local checkpoint: `lib/ptx/ad-ir.f` now maps the canonical package-qualified softmax forward body (`DUP BLOCK-MAX PTX:B- EXP. DUP BLOCK-SUM PTX:B/`) through `AD-TOKENIZE` into the ADIR op list and renders the closed-form backward IR. `lib/ptx/ir-test.f` proves the body path and fail-closed rejection of stale bare `B-`/`B/`. Remaining parent work: lower/render optimized kernels and integrate the rewrite into the broader AD simplifier path.
