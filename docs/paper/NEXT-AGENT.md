# Instructions for completing the paper (docs/paper/habu.tex)

Author: Joel Reymont <joelr1@gmail.com>. Build: `tectonic habu.tex` (brew
install tectonic). The current PDF compiles; every red [TODO] marks work.
Ground rules: EARNED CLAIMS ONLY - every quantitative statement names its
running in-tree gate/test; NEVER name the private application project
(NVIDIA model workloads are citable); keep the Moore/OKAD lineage framing
consistent with README.md + docs/positioning.md.

## Blocked on CAD-PLAN 8.1 results (do NOT write before measuring)
1. Section 7 (Performance, compute-bound): run the 8.1 sequence on zed -
   roofline microbenches, GEMM-vs-Triton baseline (extend the
   docs/eval-triton.md protocol + reproduction scripts), cp.async stages,
   tensor-core MMA, cad-6 tune, end-to-end model latency vs torch.compile.
   Insert tables, not prose. Update abstract + contribution (5).
2. Section 8 (Ablations): implement the effectiveness matrix from dot
   habu-paper-habu-checked-1c035895 (one committed experiment per
   invention; rows that exist already - wrong-adjoint fixture, convergence
   gate, error battery - just cite paths). Report as one table with
   per-row gate paths.

10. Host rows: DONE 2026-07-12. Landed from the tree (every number verified
   against its owning suite): ONNX external golden (onnx 1.22.0 byte-identical
   fixture + onnxruntime 1.27.0 parity, measured 1.24e-7 max abs —
   maki/onnx/ort-ref-test.f) in Section 5 Import; Adam trainer + torch 2.13.0
   per-step parity (worst rel-L2 1.4e-8, loss 7e-10, tol 2e-6 —
   maki/adam-torch-ref-test.f), second-order gelu pilot (backward-test.f
   2nd-BW-BUILD wiring + gelu-test.f gelu'' gradcheck), gradient checkpointing
   (docs/maki/train.md spec, bit-identical maki/checkpoint-test.f), and suite
   growth 77->88 in Section 6; live 2026-07-12 pass@k round
   (checker-as-judge transcript harness, saxpy 4/5+1-repair, softmax 5/5,
   fused-relu 5/5 — docs/eval-triton.md, maki/eval-live-test.f) in Section 7;
   Section 4.1 adoption paragraph refreshed to the landed family-typed CAD
   internals (descriptor trio + opkind + SKEY product, field swaps =
   checker rejects — .dots/habu-cad-adt-swap-7bf0bb1f.md). Device/GB/s rows
   untouched; no forward-looking performance claims added.

9. Section 4.1 (Type Families): DONE 2026-07-10. Written from the merged
   implementation (TFAM 9/10/12/14/15) and its negative regressions: 228
   reject fixtures across type-{decl,ctor,match,linear}-suite.f, the
   GE-MATCH-EXEC/GE-CONSTRUCT-EXEC engine batteries (forged-tag die, rc 85
   E-BAD-TAG, "hb: bad gemt tag"), and the mirror/AOT-persistence proofs.
   Adoption honestly deferred: the cad-adt-swap audit
   (.dots/habu-cad-adt-swap-7bf0bb1f.md) proves the swap is blocked on
   stack-only layout (WALL-1/2/3), gated on three capability dots; the paper
   claims the landed capability, not CAD adoption. Not an ablation row (no
   measured delta) — do not add it to the 16-row matrix.

## Writable now (source material exists; expand the TODO sections)
3. Section 4 (Checked Kernel Target): docs/forth.md (type system),
   docs/eval-triton.md (worked fused kernel listing K ... ;), lib/ptx
   module convention (one .version, N entries - tools/ptx/saxpy-test.f
   PTXT-COUNT regression), checker-miss RCA discipline (CLAUDE.md).
4. Section 5 (Model CAD): CAD-PLAN.md + docs/model-cad.md; the FFN demo
   numbers (maki/demo-ffn-test.f: regions 3, bytes 3040->2272, splits);
   named refs/true skip (maki/cad-ref-test.f, FFN-SKIP node.3.in "n2 i0");
   the four lowering slices (dot habu-maki-lower-tensor SLICE 1-4 entries);
   gate set + promote + store rows (maki/cad-test.f, store.f).
   Include a generated REGION_0 PTX excerpt (emit via maki/lower-ew.f
   capture sink; see maki/lower-ew-test.f).
5. Section 6 (Verified gradients): maki/backward.f headers, from-scratch
   flagship numbers (maki/from-scratch-test.f: NLL 0.130 -> -0.647, 60
   steps, bit-deterministic, GC-RUN V-PASS pre-training).
6. Section 3 (Related work): finish Inductor/torch.compile comparison;
   docs/triton.md has the 2019-paper taxonomy + modern-Triton deltas
   (verify [modern] claims against triton-lang.org before citing).
7. Bibliography: real BibTeX-quality entries (Triton MAPL'19 doi
   10.1145/3315508.3329973; find a citable OKAD/colorForth source; Halide
   PLDI'13; TVM OSDI'18; TC arXiv 1802.04730; XLA; Williams et al.
   roofline CACM'09; PyTorch 2/Inductor ASPLOS'24).
8. Figures: (a) pipeline diagram (capture->fuse->memory->schedule->lower->
   gates->promote); (b) roofline plot with measured points (mirrors
   eval-triton Figure-1 style); (c) fusion byte-accounting example.
   Generate via checked Habu tools where practical (report reducers exist);
   static SVG/TikZ acceptable for (a).

## Venue + mechanics
Workshop register first (MAPL/ARRAY class, ~10 pages); switch documentclass
to the venue's (acmart) only at submission - keep the plain-article draft
building in-repo. PDF is committed alongside the source; rebuild on every
edit (tectonic is deterministic enough). Keep this file updated: strike
items as they land, date-stamp completions.
