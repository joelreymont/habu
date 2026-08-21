---
title: Vectorize the byte loops with NEON
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.405181+02:00"
---

The counted byte/cell scan loops (BYTE-SUM, BYTE-FIND, COUNT-CHAR shapes) are memchr-class: 8-16x is normal with 16-byte NEON strips plus a scalar tail. Scope strictly to the loop shapes the elaborator already recognizes (single induction, single array, pure body of the vectorizable ops); each NEON encoder goes through asm.f + the Rocq model per the Csel precedent (ld1, and/orr/add vector forms, addv/uminv reductions, cmeq+shrn movemask idiom); answers pinned including the alignment head/tail edges and the empty and single-byte inputs. Acceptance: measured against the clang column (clang auto-vectorizes these — this is where the biggest gaps will sit); the sweep tool gains a vector-loop leaf so placement effects on the strip loop are known.

Blocked by: habu-epic-hard-cut-a684f24d phases 1-6. Re-scoped: only recognized induction/access/reduction shapes with complete scalar head/tail handling and exact empty/short/aligned/unaligned goldens; start with byte scan/find/count workloads where clang shows a large measured gap.

Claim: agent=neon workspace=.jj-ws/habu-vectorize-the-byte-a0da35a7 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED 2026-08-06 (agent neon). Stage 1 landed as `codegen: model the byte-strip SIMD forms` on bookmark `neon`: Ld1v, Uaddlv and Umovh in `formal/Common/Insn.v` with the full row discipline, `ENC-LD1V`/`ENC-UADDLV`/`ENC-UMOVH` in `src/arch/arm64/asm.f`, schema rows, and eleven mutations falsified. No emitter, so no emitted byte moved and codegen-compare reports 0 findings.

THE LEAF'S STOP QUESTION, ANSWERED: the Rocq model needs NO V-register structural surgery. `Fcsel` was already the precedent for a form whose registers are not X registers, and the V file IS the D file (v3 and d3 are one register), so the shipped encoders bound vector operands with the same `DR2` the FP forms use. What the new rows add that Fcsel could not say: `Umov` spans BOTH files, so `xregs` holds its general-register destination and not its vector source, and `every_x_register_is_checked` has to agree.

TWO PREMISES OF THIS LEAF ARE FALSE, BY MEASUREMENT.

1. BYTE-FIND is not a vectorization gap. clang does NOT auto-vectorize it - the early exit blocks it, and `hc1_byte_find` at -O2 is eleven scalar instructions, 48 bytes. It does not appear among the ten largest chain-vs-clang TIME gaps at all; its 52-byte gap is a scalar codegen gap. NEON cannot close it. Only BYTE-SUM is a genuine vectorization gap: chain 12.433 ns vs clang 3.983 ns, and its twin is vectorized with 8 accumulators over 32 bytes an iteration.

2. The blocker is not registers. The allocator ALREADY has two register files, per-file pools, per-file pressure and per-file call-clobber (`C-GPR`/`C-FPR`, `FILE-OF`, `regalloc.f:226-251`), and `abi.f:99` hands a compiled routine the whole 32-register floating file. The real blocker is that NO LOOP SHAPE SURVIVES INTO THE IR: `?do`/`loop` lower in `elaborate.f:2248-2303` to `sub`/`brz`/`add`/`lt`/`brz`, the trip count and induction variable live in the elaborator's compile-time `CS-IDX`/`CS-LIM`/`CS-HEAD` and are discarded, and `ir/fun.f:233-242` gives blocks no attribute window at all.

AND ONE SOUNDNESS DEFECT FOUND, which must be repaired BEFORE any vector class exists: `regalloc-verify.f:600-608` keys interference on value CLASS, not on register FILE. That is exact today only because class and file are in bijection. A vector class must map to `F-FPR` (the V registers alias the D registers), and the moment one does, the validator silently stops checking a vector value against a double. Dotted as `habu-key-a64rav-interference-151111d3`, priority 1.

REMAINING WORK, dotted and ordered: `habu-key-a64rav-interference-151111d3` (soundness, blocks the rest) -> `habu-give-the-ir-bba00e2a` (K-VEC type kind + the a64 dialect's V class, at the seam `a64ir.f:323-329` names) and `habu-decide-and-build-4ae94353` (how loop shape reaches a pass - the open design decision) -> `habu-vectorize-byte-sum-f5ac861d` (the pass, modelled on `combine.f`, which already solves the "no untouched row changes bytes" problem by counting first and rebuilding only on a hit) -> `habu-pin-the-vector-58faaf02` (edge goldens and clang twins).

UNBLOCKED (2026-08-05, user order): the optimization program proceeds NOW on the chain as it stands — the hard cut continues in parallel and is no longer a prerequisite. Standing acceptance for every optimization lane: name the corpus rows expected to improve BEFORE implementing; show the emitted instruction delta on them; every oracle answer preserved bit-for-bit incl. NaN; report BOTH gaps per touched row (chain-vs-clang closed, chain-vs-own-baseline gained) from tools/codegen-compare.f; re-pin the chain baseline with --update-chain only after the report is read; no regression on untouched rows outside a stated multi-objective trade. New instruction forms (madd/msub, ldp/stp, bitfields, ccmp, NEON) require Rocq rows in formal/Common/Insn.v with enc/wf/roundtrip before the emitter uses them — the CG-02 discipline, applied per-lane not deferred.
