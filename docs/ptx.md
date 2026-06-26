# Habu → PTX: a checked GPU kernel DSL

**Thesis:** a statically-checked, concatenative GPU DSL whose type system makes a
defined class of GPU bugs *unrepresentable* (address space, extent-relative
bounds, collective protocol). Working hypothesis — to be measured, not asserted
(see the experiment in [`ptx-sketch.md`](ptx-sketch.md)) — that this is a better
target for LLM-generated kernels. Not "beat Triton on FLOPS" (unwinnable
near-term); compete where the incumbents are strong-but-unsafe and LLMs struggle.

## Runtime / hardware

- Target GPU: **NVIDIA Jetson Orin** (`zed`, a Stereolabs ZED Box Orin; Ampere,
  CUDA/PTX-capable, aarch64 Linux). Reachable over SSH (Tailscale).
- Application: a 4-camera edge-perception / sensor-fusion workload (4× ZED X One
  GS cameras on the Orin, currently Zig + CUDA/ZED SDK). Real, latency-critical
  image kernels: demosaic, undistort/rectify, stereo/disparity, preprocessing.
- Strategic alignment: the just-completed **Linux-aarch64 port** is what puts
  `bin/hb` on the Orin. PTX is the natural third codegen target after
  macos-arm64 and linux-aarch64.
- Target is **sm_87** specifically (not "and up"): features are arch-gated —
  bf16/f16 yes, **no fp8** (Hopper sm_90+), async-copy/vector-op availability is
  checked per target. Other arches are future target-feature gates.

## Why this fits Habu better than CPU Forth did

The measured CPU benchmark verdict: Habu *loses* on array tasks — Python/Rust are
trained-on and ~20–66× cheaper per LLM token. **GPU inverts this.** The
"mainstream" is CUDA/Triton, where humans and LLMs both struggle and the bugs
(races, wrong memory space, divergence, out-of-bounds, uncoalesced access) are
the nastiest to debug. The playing field is level, and the checker's value is
highest exactly where the incumbent is weakest and unsafe.

The LLM-kernel field (2025) is a gold rush — TritonBench, TritonRL, AutoTriton,
Meta KernelEvolve — overwhelmingly "train/RL/agent the model harder." The
mainstream kernel languages (CUDA, Triton, Python eDSLs) do **not** enforce this
contract set (address space, extent-relative indexing, lane-mask/uniformity
discipline) at the source level — that is the gap Habu's checker targets, a niche
the dynamically-typed incumbents structurally do not occupy.

## Landscape (2025)

- Triton pipeline: Python → Triton IR (MLIR: `Triton`/`TritonGPU`/`TritonNVIDIAGPU`)
  → LLVM IR → **PTX** → `ptxas` → cubin. NVIDIA added a CUDA Tile-IR backend.
- Crowded: Gluon (OpenAI, lower-level), CuTe-DSL (NVIDIA), TLX (Meta), Pallas
  (Google/JAX), TileLang, Warp, ThunderKittens, Helion, Mojo. Axis:
  declarative/researcher-friendly ↔ imperative/hardware-friendly. Almost all are
  still Python eDSLs; only Mojo/TileLang break away.
- Python-eDSL critique (Modular/Lattner): "looks like Python but isn't"; dynamic
  typing for low-level code; embedding forbids new syntax; blind debugging; ~20%
  off hand-CUDA on H100; governance/portability fragmentation.

## Why PTX is a friendly target

PTX is a *virtual* ISA: emit unlimited virtual registers (SSA-ish); `ptxas` does
the real register allocation, scheduling, and SASS codegen. A from-scratch
backend therefore does **not** need a world-class allocator. The self-hosted
`bin/hb` already has an arm64 assembler/encoder (`src/arch/arm64/`), a
stack→register compiler (`src/habu/jit.f`/`regalloc.f`), and AOT (`src/habu/aot.f`)
— but note two honest limits the plan review surfaced: (1) the existing
"multi-target" axis is **OS/object-format** (macos↔linux over the *same* arm64
ISA); PTX is a **new ISA** target sharing none of `src/arch/arm64/`, so it is a
new encoder, not a trivial port. (2) The self-hosted codegen emits machine words
directly; the constant-fold/DCE/CSE/peephole passes live only in the gforth
bootstrap (`bootstrap/cg/opt.fs`, peephole-only), so a PTX IR + opt layer is new
work too. Memory spaces are explicit in PTX
(`global/shared/local/const/param/reg`), good for the type system.

## The type system as the moat

Intended v0 static guarantees, enforced as stack-effect contracts — exact types
in [`ptx-sketch.md`](ptx-sketch.md):

- **Typed address spaces:** a span carries its space `S`; a global load on a
  shared span is untypable. Eliminates memory-space confusion.
- **Extent-relative bounds:** a load needs a tile context *derived from the span
  it reads*, so its mask is computed against that span's declared extent `N`.
  Out-of-bounds **relative to the declared span** is unrepresentable.
  Constructing a span asserts its extent — the trusted boundary, like Rust's
  `slice::from_raw_parts`. This is not universal memory safety; it is relational
  consistency between a span's declared length and every access to it.
- **Typed collectives:** block/warp reductions carry a mask identity (max→−∞,
  sum→0) and reject under divergent control flow.
- **Shape tokens:** `tile<T,B,M>` / `matrix<S,T,R,C>` travel in the effect, so
  composition shape-checks by unification.

This *combination*, enforced as a first-class source-level stack-effect
contract, is not what Triton or CUDA check at the language level — both verify
far less statically. (Not a claim that no checker anywhere expresses any of it.)

## Why tiles + concatenative is elegant

The tile dataflow, the program text, and its optimized (fused) form are the
**same artifact**:

1. A tile kernel is literally a pipeline of whole-tile transforms
   `LOAD → f → g → STORE` — exactly a concatenative pipeline; the tile shape
   carries the iteration, so there are no index variables or loop boilerplate.
2. The stack effect *is* the tile dataflow contract:
   `RELU ( tile<f32,B,M> -- tile<f32,B,M> )`. Habu's row-polymorphism over tiles
   gives shape-checking for free; Triton threads named SSA temporaries instead.
3. Habu's combinator algebra and the GPU tile-collective algebra are the **same
   algebra**: `MAP` = per-lane op, `FOLD`/`REDUCE` = warp/block reduction,
   `SCAN` = prefix sum. `[: + ;] TILE-REDUCE` is a block reduction.
4. **Point-free is fusion-friendly.** Concatenation hands the compiler a linear
   pipeline instead of a DAG to recover from named temporaries, so register-
   resident fusion (the dominant bandwidth optimization) is the *default shape* —
   subject to legality (barriers, effects, aliasing, register pressure,
   occupancy), which the checker/IR must still verify before fusing.
5. Small factored words → a composable, checked tile-primitive library
   (`TILE-LOAD-MASKED`, `BROADCAST`, `WARP-REDUCE-SUM`, `TRANSPOSE-TILE`) — the
   CuTe/Triton "composable layout abstraction", but verified.

## How Habu codegen can optimize PTX

`ptxas` owns the machine-level last mile (register allocation, instruction
scheduling, SASS selection) — do not fight it there. (Bank-conflict and
coalescing freedom are *source-level* layout/access-pattern properties, not
something ptxas fixes.) Habu optimizes *above* ptxas, where source-level
knowledge is required:

1. **Fusion (the big win):** concatenative pipelines are structurally fused —
   emit one register-resident kernel instead of N kernels round-tripping global
   memory (the bandwidth-bound common case). Structural, not an analysis pass.
2. **Type-driven memory shaping:** for layout/ctx/element-size patterns the
   checker can prove (contiguous span + flat ctx), Habu emits coalesced loads, and
   — only under a typed alignment+width proof — a vectorized load (`ld.global.v4`)
   with a masked scalar tail, from the type, not an optimizer guess. Shared-memory staging (`cp.async`) and `bar.sync` are
   *deferred* to the shared-memory/barrier milestone (v0 does not model shared
   aliasing), not claimed as checked yet.
3. **A PTX IR + opt layer is new work, not reuse.** The self-hosted `bin/hb`
   emits machine words directly; the only IR optimizer (`bootstrap/cg/opt.fs`,
   gforth-bootstrap-only) is **peephole** (no CSE, no strength-reduction; constant
   folding is JIT-time in `jit.fs`, not `opt.fs`). So a PTX IR with fold/DCE/peephole
   would be built fresh — plus the stack→register pass, which does carry over.
4. **stack→register is what PTX wants:** Habu already lowers concatenative stack
   ops to register dataflow for ARM64; PTX is register-based SSA-ish, so the same
   pass maps ~1:1 to PTX virtual registers (`dup` → reuse a vreg; stack → small
   live vreg set). Natural fit, not impedance mismatch.
5. **Compile-time specialization:** Forth defining words + checked
   metaprogramming generate specialized kernels (unroll by tile size, specialize
   on dtype, build the reduction tree) — Triton's `constexpr`/autotuning, but each
   specialization is checked.
6. **Autotuning (later):** to approach the perf ceiling, search tile size /
   num-warps / pipeline depth and time on the Orin. The "speed-of-light" stage.

## Honest hard parts

- **Perf is the whole game on GPU.** Habu's codegen is a simple JIT, not an
  autotuning/pipelining optimizer. MVP pitch must be safety + LLM-friendliness,
  not peak FLOPS; "fast" is a separate, large, later effort.
- **SIMT vs stack model.** Per-thread Forth-on-registers is easy but slow (no
  coalescing). Commit to **tile-level from day one**: the stack holds tiles, not
  scalars.
- **Zero ecosystem** vs PyTorch-integrated incumbents; the wedge must be
  safety/LLM, not generality.

## Foundational prerequisites (surfaced by plan review)

The plan review found two capabilities the milestones assume but the runtime/
checker do not yet have — both are prerequisite *build* work, not "another
backend":

1. **A real C-ABI FFI (the AAPCS64 calling convention).** Habu reaches the OS
   only via raw `svc` syscalls and has no userspace function-call ABI; `C-CALL` is
   internal codegen, no `dlopen`/`dlsym`. Process-spawn exists (so `ptxas` is
   invokable), but calling `cuMemAlloc`/`cuModuleLoad`/`cuLaunchKernel` requires
   the whole AAPCS64 standard — int args x0–x7, **FP args v0–v7**, x8
   indirect-result, stack spill, callee-saved discipline — plus out-param/`void**`
   struct marshalling and Tegra-path libcuda discovery. Sized as a milestone
   (see `ptx-sketch.md` M1); it is not "dlopen + marshalling."
2. **A parametric-type extension to a self-hosting checker.** M2 core support is
   a checker feature, not a syntax note: `<`/`>`/`,` signature tokens, explicit
   atom namespaces (`space-*`, `extent-*`, `mask-*`, `block-*`, `align-*`),
   side-table encoded `T-PARAM` terms such as
   `span<space-global,f32,extent-n>`, field-by-field unify, render/record
   round-trip, and a self-host fixpoint rebuild. The remaining M2 surface is the
   PTX defining vocabulary (`KERNEL:`, `%block`, `grid:`, `where`).

The linux-aarch64 `bin/hb` itself already exists (it lives on the Orin, `zed`);
the macOS checkout simply doesn't contain it. The Orin must also have a CUDA
toolkit (`ptxas`) installed.

## Decisions (locked)

- **Execution model:** tile-level (the stack holds tiles, not scalars).
- **Backend:** PTX-direct → `ptxas` (own the codegen; ptxas does register
  allocation, scheduling, SASS).
- **Type system v0:** typed address spaces + extent-relative bounds + typed
  collectives (exact types in [`ptx-sketch.md`](ptx-sketch.md)).
- **Flagship:** a real camera kernel (demosaic) proves the pipeline; fused
  softmax→attention (fp16/bf16; Orin is sm_87, no fp8) is the role-facing headline.
- **LLM claim:** deferred to the experiment in [`ptx-sketch.md`](ptx-sketch.md) —
  no "better LLM target" claim until the matrix exists.

## Sources (accessed 2026-06-25)

- Triton kernel compilation stages — https://pytorch.org/blog/triton-kernel-compilation-stages/
- CUDA Tile-IR backend for Triton — https://developer.nvidia.com/blog/advancing-gpu-programming-with-the-cuda-tile-ir-backend-for-openai-triton/
- Gluon (lower-level Triton) — https://biggo.com/news/202509190133_Gluon_GPU_Programming_Language
- Modular, "Triton and Python eDSLs" — https://www.modular.com/blog/democratizing-ai-compute-part-7-what-about-triton-and-python-edsls
- Helion / evolving GPU programming model (ianbarber.blog, Oct 2025) — https://ianbarber.blog
- TritonBench — https://arxiv.org/pdf/2502.14752 · AutoTriton — https://arxiv.org/pdf/2507.05687
- PTX ISA (sm_87 directives, `ld.global.v4`, `shfl.sync`, `ex2.approx`) — https://docs.nvidia.com/cuda/parallel-thread-execution/
