# Habu → PTX: a checked GPU kernel DSL

**Thesis:** a statically-checked, concatenative GPU DSL whose type system shifts a
defined class of GPU bugs left to author time: address-space mismatches,
extent-relative access discipline, and collective protocol errors. Fresh
per-call extent/mask identity is expressed as checker constructor templates
(`fresh-extent-*`, `fresh-mask-*`); full collective semantics are still active
work (`habu-fix-ptx-collective-997cfcce`). The measured
claim is not "beat Triton on FLOPS"; it is that checked source gives earlier,
more located failures for the stack-discipline class where LLMs struggle.

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
  Current v0 proves agreement for shared tokens and rejects independently minted
  fresh extent/mask identities when constructors use `fresh-extent-*` /
  `fresh-mask-*`. Constructing a span asserts its extent — the trusted boundary,
  like Rust's `slice::from_raw_parts`.
  This is not universal memory safety; it is relational consistency between a
  span's declared length and every access to it.
- **Typed collectives:** block/warp reductions are meant to carry per-collective
  mask identities (max→−∞, sum→0) and reject under divergent control flow. The
  current softmax path is device-proven, but generic collective mask/block
  hardening is still dotted before this is a full device-proof claim.
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
   checker can prove (contiguous span + flat ctx), Habu emits coalesced loads. The
   current v4 path emits `ld.global.v4` / `st.global.v4` behind explicit `*-V4`
   trusted primitives and an `N % 4 == 0` precondition; typed alignment proofs and
   masked scalar tails are future work, not current guarantees. Shared-memory staging (`cp.async`) and `bar.sync` are
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

## Landed foundation and remaining gaps

The plan review originally surfaced FFI and parametric stack types as
prerequisites. Both are now landed on the Linux/aarch64 Orin path:

1. **AAPCS64 FFI + dynamic ELF:** `lib/ffi.f` calls CUDA Driver functions through
   loader-resolved `dlopen`/`dlsym` slots, with integer/pointer calls, stack-spill
   support, and float-return helpers. Current device tools still need stronger
   fail-closed rc wrappers and cleanup (dot `habu-make-ptx-device-c0eb12a3`).
2. **Parametric checker terms:** signatures such as
   `span<space-global,f32,extent-n>` and `tile<f32,block-256,mask-live>` parse,
   render, unify field-by-field, and gate `KERNEL:` bodies. Fresh per-call rigid
   extent/mask minting is available to trusted constructors through
   `fresh-extent-*` and `fresh-mask-*`.

The remaining PTX foundation work is semantic, not bootstrap: correct generic
collective mask/block lowering (dot `habu-fix-ptx-collective-997cfcce`), typed
v4 alignment/tail proofs beyond the current `N % 4 == 0` path, and durable
device proof/gate hardening listed above.

## Decisions (locked)

- **Execution model:** tile-level (the stack holds tiles, not scalars).
- **Backend:** PTX-direct → `ptxas` (own the codegen; ptxas does register
  allocation, scheduling, SASS).
- **M3 local encoder:** `src/arch/ptx/emit.f` emits the checked sm_87 SAXPY PTX
  toolchain kernel; `tools/ptx/saxpy.f` is the CLI entrypoint and
  `tools/ptx/saxpy-test.f` pins the header/instruction contract.
  `tools/ptx/ptxas-smoke.f` is the Orin-only checked smoke for the `ptxas`
  step; cubin launch waits for the Habu CUDA Driver harness.
- **Type system v0:** typed address spaces + extent-relative bounds + typed
  collectives (exact types in [`ptx-sketch.md`](ptx-sketch.md)).
- **Flagship:** a real camera kernel (demosaic) proves the pipeline; fused
  softmax→attention (fp16/bf16; Orin is sm_87, no fp8) is the role-facing headline.
- **LLM claim:** the current Orin matrix earns the stack-discipline-left-shift
  claim and SAXPY v4 bandwidth parity only; broader "faster than Triton" and
  semantic-error static-checking claims remain unearned.

## Sources (accessed 2026-06-25)

- Triton kernel compilation stages — https://pytorch.org/blog/triton-kernel-compilation-stages/
- CUDA Tile-IR backend for Triton — https://developer.nvidia.com/blog/advancing-gpu-programming-with-the-cuda-tile-ir-backend-for-openai-triton/
- Gluon (lower-level Triton) — https://biggo.com/news/202509190133_Gluon_GPU_Programming_Language
- Modular, "Triton and Python eDSLs" — https://www.modular.com/blog/democratizing-ai-compute-part-7-what-about-triton-and-python-edsls
- Helion / evolving GPU programming model (ianbarber.blog, Oct 2025) — https://ianbarber.blog
- TritonBench — https://arxiv.org/pdf/2502.14752 · AutoTriton — https://arxiv.org/pdf/2507.05687
- PTX ISA (sm_87 directives, `ld.global.v4`, `shfl.sync`, `ex2.approx`) — https://docs.nvidia.com/cuda/parallel-thread-execution/
