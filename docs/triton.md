# Triton — how it works (paper notes + modern system + habu implications)

Source of record: Tillet, Kung, Cox, *Triton: An Intermediate Language and
Compiler for Tiled Neural Network Computations*, MAPL '19
(doi 10.1145/3315508.3329973). Read 2026-07-04 from the extracted text of the
Harvard PDF. Sections marked **[modern]** describe today's Triton (Python DSL,
MLIR era) from general knowledge, not the paper — verify before citing.
Measured habu-vs-Triton numbers live in `docs/eval-triton.md`; strategy in
`CAD-PLAN.md` §8.1. This doc is the mechanism reference.

## 1. Thesis (paper, 2019)

Vendor libraries (cuBLAS/cuDNN) cover a restricted op set; novel primitives
need expert hand-tuning. Prior DSLs split three ways (their related-work
taxonomy):

- **Tensor-level IRs** — XLA, Glow: pattern-match tensor programs into
  *predefined* LLVM-IR/CUDA templates. Inflexible.
- **Polyhedral** — Tensor Comprehensions, Diesel: parameterize/automate
  compilation of affine loop nests. Cannot express non-affine indices
  (structured sparsity, lookup-table addressing).
- **Loop synthesizers** — Halide, TVM: separate algorithm from *schedule*;
  the schedule is user-specified (manual, though parametric).

Triton's contrast: add **tile-level operations and optimizations to a
traditional (LLVM) compilation pipeline**. Claimed wins: more flexible than
XLA/Glow, non-affine indices unlike TC/Diesel, and **automatic inference of
the execution schedule** that Halide/TVM require manually. Stated cost:
"increased programming efforts" — the kernel author writes tile-level C.

## 2. Triton-C (the 2019 language)

- C/CUDA-like syntax. **Tiles = statically shaped multi-dim sub-arrays**
  (`int tile[16,16]`, distinct from C's nested arrays). Shapes are constexpr
  and may be **`tunable`** with a candidate set: `const tunable int TM =
  {16,32,64,128}` — the autotuner picks.
- Ranges by ellipsis (`int rk[TK] = 0 ... TK`); NumPy **broadcasting** with
  `newaxis`/slicing (pad-left-with-1s, then replicate; error if impossible);
  slicing a tile down to scalars is forbidden.
- Built-ins: `get_global_range(axis)`, `dot`, `trans`. **Predication**:
  `@mask statement` for tile-level control flow. Bounds handling is masked
  loads/stores (`float A[TM,TK] = check_a ? *pa : 0;` / `@checkc *pc = C;`).
- **Programming model**: SPMD like CUDA, but each kernel instance is
  **single-threaded** (and auto-parallelized) over its global ranges. No
  user-visible threads, no shared memory, no barriers — the compiler owns
  intra-tile concurrency entirely. (Contrast CUDA: user owns the thread
  block.) GEMM in ~30 lines: pointer tiles, masked loads, `C += dot(A,
  trans(B))`, pointer bumps.

## 3. Triton-IR (2019)

LLVM-based IR with tile types/ops added; built from Triton-C at parse time.
Modules → functions → typical LLVM structure with attributes. Extensions:

- **Retiling**: `reshape` (incl. pad-with-1 dims), `broadcast` (replicate
  along size-1 dims), `splat` (scalar → tile).
- Scalar instructions (`add`, `icmp`, `getelementptr`, `load`, ...) lifted
  element-wise to tiles; masks are boolean tiles (`icmp` on range tiles),
  loads/stores take predicates.
- Specialized arithmetic: `dot`, `trans`.
- Tile shapes in the IR are **concrete** (instantiated per autotuner choice);
  parametricity lives above the IR.

## 4. Triton-JIT passes (2019) — the heart of the paper

Machine-independent:
- **5.1.1 Pre-fetching**: detect tile loads in loops; hoist iteration-0 load
  to the preheader and issue next-iteration loads inside the loop (phi'd),
  hiding memory latency where independent instructions are scarce.
- **5.1.2 Tile-level peephole**: algebraic tile identities, e.g.
  `(X^T)^T = X`.

Machine-dependent (machine model: tile → **micro-tile** per SIMD unit →
**nano-tile** per ALU/LDST lane; Figure 5):
- **5.2.1 Hierarchical tiling**: decompose tiles to micro/nano tiles to fit
  the machine hierarchy (blocks/warps/lanes). The tiling parameters are the
  autotuner's search space.
- **5.2.2 Memory coalescing**: because the program is single-threaded, the
  backend **orders threads within micro-tiles** so adjacent threads hit
  adjacent addresses (coalesced DRAM transactions).
- **5.2.3 Shared memory allocation**: stage high-arithmetic-intensity
  operands (`dot`) in shared memory; allocation via **liveness analysis**
  over tile values (paper Figure 7 shows interval-style packing of SMEM).
- **5.2.4 Shared memory synchronization**: barrier insertion by **forward
  dataflow analysis** detecting RAW/WAR hazards (the paper gives the
  fixed-point equations over `ins/outs` sets; a hazard resets the set and
  emits a barrier).

**Autotuner (5.3)**: no hand-written templates — search space extracted from
the IR by concatenating pass meta-parameters. In the paper: hierarchical
tiling only, ≤3 params/dim/tile, **exhaustive** powers-of-two (tiles 32–128,
micro 8–32, nano 1–4). "Better auto-tuning methods could be used."

## 5. Results claimed (GTX 1070, cuBLAS 10 / cuDNN 7)

- GEMM: ≥90% of peak, on par with cuBLAS across DeepSpeech2/Transformer
  shapes; **cuBLAS wins on shallow transformers** via its split-K "3D"
  algorithm (more parallelism when M,N small — a real gap they name).
  Other DSLs 2–3× slower (TVM <2× when shapes are multiples of 32).
- Convolution: re-implements cuDNN IMPLICIT_GEMM (lookup tables of pointer
  increments for the non-affine indexing); beats cuDNN on ResNet tasks,
  parity on DeepSpeech2 — they attribute their ResNet win to cuDNN's
  engineering attention going to Winograd instead.
- Shift-conv: first **fused** shift+conv kernel (shift folded into the
  im2col-style pointer table) — fusion as address arithmetic, near the
  roofline.

## 6. [modern] What changed since the paper

- Frontend is a **Python-embedded DSL** (`@triton.jit`, `tl.*`): explicit
  `tl.program_id`, `tl.arange`, `tl.load/store(mask=)`, `tl.dot`;
  `tl.constexpr` meta-params; `@triton.autotune` decorator with explicit
  config lists (Triton-C's `tunable` sets became user-supplied configs —
  the paper's "extract space from IR" idea regressed to user templates).
- Compiler rebuilt on **MLIR** (Triton dialect → TritonGPU dialect →
  LLVM/PTX): layouts/encodings on tensor types replace the paper's
  hierarchical-tiling pass; `num_warps`, `num_stages` (software pipelining
  with `cp.async`) are the headline meta-params; tensor-core `mma` paths;
  AMD + CPU backends. Kernel launch/caching per-process at JIT time.
- The 2019 predication/`@` syntax is gone; masks are explicit load/store
  arguments. Single-threaded-per-instance tile model is unchanged — that is
  Triton's durable idea.

## 7. Implications for habu (ties to CAD-PLAN §8.1, measured record in eval-triton.md)

- **Same durable insight, different trust story.** Triton and habu both let
  a scalar-looking program own a tile while the compiler owns concurrency.
  Habu's checker types the composition (stack effects, roles) and the
  GOLDEN/gradcheck gates prove the numerics; Triton has no equivalent —
  author bugs surface at runtime (measured in eval-triton.md's error-catch
  battery).
- **Their four machine-dependent passes map onto our plan**: hierarchical
  tiling ≈ our schedule families (bm/bn/bk/warps/stages); coalescing ≈
  cad-3 memory planning + v4 vectorization (already at parity on bandwidth);
  SMEM allocation + barrier dataflow = what our tile-smem/collective
  emitters do explicitly today and what the MMA/cp.async work
  (habu-tensor-core-mma, §8.1) must generalize. Their liveness-packed SMEM
  and RAW/WAR barrier equations are directly reusable designs if we automate
  SMEM staging.
- **Autotuning**: the paper's ideal (space extracted from the program, not
  hand templates) matches our schedule machinery better than modern
  Triton's config lists; our §7.4 content-keyed store + cad-6 makes tuning
  persistent instead of per-process JIT — a real differentiator on embedded.
- **Their named weakness is our opening**: split-K for small-M/N reductions
  (their cuBLAS loss) is a schedule-family candidate we can add; and
  everything cross-kernel (fusion depth across regions, weight layout at
  PROMOTE, launch amortization) is outside Triton's per-kernel horizon by
  construction.
- **Non-affine indexing via pointer-increment lookup tables** (their conv
  trick) is the pattern for our gather/scatter and future embedding kernels.

## 8. Pointers

- Paper text extracted at /tmp (regenerate: fetch the PDF, `pdftotext`).
- Related systems named: XLA, Glow, TC, Diesel, Halide, TVM, PlaidML;
  micro-kernel prior art; roofline methodology for Figure 1.
- Modern docs: triton-lang.org; MLIR dialect sources in the openai/triton
  repo. **[modern]** claims here should be re-verified against those before
  external use.
