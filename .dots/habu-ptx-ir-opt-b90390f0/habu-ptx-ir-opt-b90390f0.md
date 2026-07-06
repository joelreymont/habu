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

2026-07-06 local checkpoint: added a SECOND, complementary optimizer layer that works on EMITTED PTX TEXT (distinct from the expression IR above). `lib/ptx/opt-ir.f` parses a captured module (src/arch/ptx/emit.f PTX-CAPTURE$) into a typed line-oriented instruction table (pure op / dtype / dst / srcs, or opaque passthrough; fail-closed: any unmodelled or memory-touching line is opaque and rendered byte-identically). `lib/ptx/opt.f` runs SOUND bit-exact passes over it: copy-propagation, constant-fold (duplicate immediate mov), CSE, redundant-store elimination for reused registers, DCE, and a self-move peephole. All passes are region-scoped (barriers/atomics/labels/branches/predication fence value numbering) and never reorder, recompute, or change a rounding mode. `OPT-PTX ( ptr u8 n -- ptr u8 n )` optimizes entry-by-entry; integration is OPT-IN via `PTX-MAYBE-OPT`, OFF by default everywhere (no proven kernel changes until a consumer flips it with device evidence). Tests: `lib/ptx/opt-ir-test.f` (classification / round-trip / fail-closed) and `lib/ptx/opt-test.f` (per-pass before/after fixtures, idempotence opt(opt(x))=opt(x), fma-refusal, and a safety layer that optimizes real saxpy/gelu/cg-mma text and asserts semantics survive). Wired into the ptx-stdlib gate (test/gate-stdlib-cases.f + gate-stdlib-inline-lib.f) and FILEMAP.md; std.manifest has no lib/ptx rows so no manifest change. Gates green: typed-local-diff-lint, filemap-lint, host-lint, maki/test.f, full test/run.f.

  Host-measured instruction-count deltas (mnemonic count, directives/labels/braces excluded):
  - SAXPY (proven kernel): 21 -> 21, delta 0 (no redundancy; byte-preserved).
  - GELU forward (single, tanh-approx): 17 -> 17, delta 0 (all subexpressions/constants distinct; correctly preserved).
  - GELU recomputed twice on the same input: delta 7 (CSE eliminates the redundant recompute).
  - cg-mma TF32 tile (MMA-LMODE 0, cvt baseline): 364 -> 361, delta 3 (loop-invariant address recompute `add.u32 %r41,%r16,%r30` across the 4 unrolled MMA-K substeps removed; mma.sync / cvt.rna.tf32.f32 / st.global anchors preserved).

  PENDING-ZED (device): zed is offline, so no device run was attempted. Device perf/correctness of any OPTIMIZED kernel is UNVALIDATED. Before flipping `PTX-OPT-ON` for any consumer: ptxas-assemble + device-golden the optimized saxpy/gelu/cg-mma on Orin sm_87 and confirm bit/tolerance parity vs the un-optimized kernel; only then opt-in per-consumer.

  Follow-up capability (beyond the sound conservative scope): cross-block / loop-invariant code motion with a real CFG+liveness would let the address-ALU CSE hoist out of the MMA-K loop entirely (rung-1 ALU-overhead lever) rather than only collapsing within one unrolled body. Mint a dot when picked up. The mul.rn+add.rn -> fma.rn fusion is intentionally NOT implemented: it folds two roundings into one and is never sound for our `.rn`/default-rounding emitters (opt-test.f asserts the pair survives); a dot should track a rounding-safe fma path only if an emitter ever produces a genuine single-rounding fma expressed as mul+add.
