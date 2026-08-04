# Habu → PTX: a checked GPU kernel DSL

**Thesis:** a statically-checked, concatenative GPU DSL whose type system shifts a
defined class of GPU bugs left to author time: address-space mismatches,
extent-relative access discipline, and collective protocol errors. Fresh
per-call extent/mask identity is expressed as checker constructor templates
(`fresh-extent-*`, `fresh-mask-*`). Collective lowering now applies the
inactive-lane identity at each reducer and derives shared-memory/fold bounds
from `%BLOCK`; divergent-control rejection remains M5 uniformity work. The measured
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
stack→register compiler (`src/habu/jit.f`, with `regalloc.f` a thin slice of
it), and AOT (`src/habu/aot-capture.f`/`aot-lib.f`/`aot-closure.f`; `aot.f` is
only the maker entry)
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
- **Typed collectives:** block/warp reductions apply per-collective inactive-lane
  identities (max -> -inf, sum -> 0), derive their fold bound from `%BLOCK`, and
  reject `WHERE ... block-N` mismatches. Divergent-control rejection remains M5
  uniformity work.
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
   current v4 path emits `ld.global.v4` / `st.global.v4` for full vectors and
   predicated scalar lanes for residual vectors behind explicit `*-V4` trusted
   primitives; typed alignment proofs remain future work, not current guarantees.
   Shared-memory staging (`cp.async`) and `bar.sync` are
   *deferred* to the shared-memory/barrier milestone (v0 does not model shared
   aliasing), not claimed as checked yet.
3. **A PTX IR + opt layer is new work, not reuse.** The self-hosted `bin/hb`
   emits machine words directly; the only IR optimizer (`bootstrap/cg/opt.fs`,
   gforth-bootstrap-only) is **peephole** (no CSE, no strength-reduction; constant
   folding is JIT-time in `jit.fs`, not `opt.fs`). The first PTX IR is now
   Habu-native (`lib/ptx/ir.f`) with constant fold, value-numbering/CSE, DCE, and
   a softmax-backward AD bridge (`lib/ptx/ad-ir.f`); general rewrite selection
   remains open.
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

## Generated-state integrity

A checked Habu emitter and the PTX program it emits execute on different
machines. The host stack effect proves that the emitter composes correctly; it
does not prove that PTX virtual registers, predicates, control flow, address
spaces, resources, or barriers are legal. A successful `ptxas` process proves
still less about semantics. Promotable device code therefore follows five
separate, content-bound verification layers.

### Target-indexed PTX state

Package `PTX-INSTRUCTION` owns typed instruction and operand values. Package
`PTX-STATE` independently verifies the resulting instruction graph before
package `PTX-RENDER` may produce text. The state is indexed by the exact target
and includes:

- nominal virtual-register identities, one-definition state, register class,
  scalar/vector value type, definition site, and dominance;
- predicate definition and its valid control region;
- labels, CFG edges, path joins, reconvergence, and live state;
- pointer address space, value type, access width, alignment fact, and extent
  or mask authority;
- declared and used parameter, register, shared, local, and constant resources;
- collective identity, uniformity/mask fact, and barrier phase;
- canonical instruction, CFG, target, and verifier-version digests.

`PTX-STATE:VERIFY` rejects undefined use, duplicate definition, wrong register
class, use-before-definition predicates, predicate/control mismatches,
incompatible branch joins, address-space/type mismatches, incomplete or
contradictory resource declarations, and M5-invalid barrier reachability. A
rendered PTX string is output, never verification authority. Text-only legacy
paths cannot produce promotable evidence.

M5 remains the sole owner of uniformity and divergent-barrier semantics. The
phantom-preservation dot remains the sole owner of carrying kernel type-family
parameters through checked emitters. The PTX-state verifier consumes both; it
does not reimplement them.

### Proprietary `ptxas` boundary

NVIDIA `ptxas` owns physical-register allocation, scheduling, and SASS
selection. Habu cannot honestly emit an independent allocation certificate for
those proprietary decisions. `PTXAS-ATTEST:ASSEMBLE` instead records an
opaque-backend attestation containing:

- the verified PTX instruction/CFG digest and canonical PTX byte digest;
- immutable target, feature, toolchain, version, configuration, and invocation
  policy identities;
- process result and content-bound diagnostics;
- typed register, shared, local, constant, stack, and spill report facts;
- the exact cubin payload digest and attestation/verifier version.

That attestation proves provenance, successful assembly, report well-formedness,
and observable resource facts. It does not claim that Habu proved allocation,
scheduling, or SASS semantics. `habu-v2-resource-model-985a0b0e` consumes the
attested resource row for prediction calibration; it remains the sole owner of
occupancy and prediction-error policy.

Unknown toolchains, version/config drift, malformed or duplicate required
fields, resource-report drift, failed assembly, missing output, input/output
mutation, and unsupported required verifier policies fail closed. A backend may
produce a typed diagnostic artifact when policy allows it, but it cannot become
promotable by silently omitting evidence.

### Cubin/SASS identity through promotion

Package `CUBIN-INTEGRITY` binds the `ptxas` output to target, toolchain,
verified PTX, attestation, cubin payload, kernel symbol, ABI schema, launch
configuration, environment, and verifier version. Cubin bytes are hashed at
registration and immediately before module load. Launch, golden, profile,
replay, and promotion evidence all name the same immutable subject.

Optional SASS/disassembly evidence is content-bound and policy-indexed. A policy
that requires independent disassembly rejects if the verifier is unavailable;
it does not downgrade to a path or verdict tag. A cubin byte mutation, region
swap, wrong symbol/ABI, target/toolchain mismatch, changed grid/block/shared
configuration, stale environment, wrong-subject device result, or promotion
with unbound verdict tags rejects before launch or promotion.

The generic canonical-artifact and proof-obligation owners define the envelope
and evidence policy. The device-proof owners define CUDA failure handling and
golden execution. The proof-carrying allocation owner applies only to
allocations Habu can independently verify. `CUBIN-INTEGRITY` preserves those
facts through the PTX-specific chain without duplicating them.

### Fixed call graph and implementation ownership

The reviewed call graph is fixed to:

~~~text
maki/lower/{ew,red,mm,move}.f
  -> lib/ptx/{ir,cg,header,collective}.f
     + lib/ptx/cg-collective.f
     + src/arch/ptx/emit.f
  -> lib/ptx/toolchain.f + tools/ptx/ptxas-smoke.f
  -> maki/lower/launch.f
  -> maki/lower/golden.f + maki/lower/model-device-test.f
  -> maki/report.f + maki/cad.f + maki/store.f
~~~

The three missing implementation owners are deliberately disjoint:

1. `habu-verify-ptx-virtual-50281017`: instruction/state ADTs, actual-CFG
   verification, diagnostics, and the render gate.
2. `habu-attest-proprietary-ptxas-6ce9fda2`: exact assembler provenance,
   resource-report parsing, opaque-backend attestation, and replay.
3. `habu-bind-cubin-and-c1103e74`: cubin/SASS identity through typed launch,
   device evidence, persistence, and promotion.

Any newly discovered emitter, assembler, launch, evidence, or promotion bypass
outside this census requires review and explicit ownership before it may emit a
promotable PTX artifact.

## Landed foundation and remaining gaps

The plan review originally surfaced FFI and parametric stack types as
prerequisites. The portable FFI ABI is landed on macOS/aarch64 and Linux/aarch64;
CUDA device proof remains Linux/Orin-specific:

1. **AAPCS64 FFI + target loader slots:** `lib/ffi-abi.f` provides target-independent
   integer/pointer calls, stack-spill support, float-return helpers, out-param
   readback, `void**` kernelParams packing, and loader-resolved `dlopen`/`dlsym`
   slots on Linux ELF and macOS Mach-O. Current
   device tools still need stronger fail-closed rc wrappers and cleanup (dot
   `habu-make-ptx-device-c0eb12a3`).
2. **Parametric checker terms:** signatures such as
   `span<space-global,f32,extent-n>` and `tile<f32,block-256,mask-live>` parse,
   render, unify field-by-field, and gate `KERNEL:` bodies. Fresh per-call rigid
   extent/mask minting is available to trusted constructors through
   `fresh-extent-*` and `fresh-mask-*`.

The remaining PTX foundation work is semantic, not bootstrap: correct generic
collective uniformity/barrier lowering, typed v4 alignment proofs, int-vs-float
arithmetic capability constraints, PTX virtual-state verification, exact
`ptxas`/cubin integrity evidence, and durable device proof/gate hardening listed
above.

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
