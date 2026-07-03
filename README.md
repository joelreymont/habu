# Habu — Model CAD for GPUs

**Change the model; Habu re-fuses, re-tiles, and re-tunes the GPU path.**

Habu keeps the optimization loop at the model level: one live REPL where
fusion, memory coalescing, tiling, validation, profiling, and tuning stay in
sync with model edits — the way EDA keeps placement, routing, design-rule
checks, and timing in sync with a schematic.

Whether a change is typed by a person or proposed by an LLM, it passes the
same gates before promotion: author-time type check, golden test against a
reference, gradcheck for generated backward code, and device profile.

## What "Model CAD" means

Electronic CAD does not merely let you draw gates. It places, routes, checks
rules, simulates timing, extracts artifacts, and gives feedback when the design
is wrong.

Habu applies that idea to model implementation:

| CAD concept | Habu equivalent |
| --- | --- |
| Schematic | Model composition |
| Placement | Layout and memory plan |
| Routing | Tensor movement and coalescing plan |
| Timing analysis | Profile and roofline report |
| Design-rule checking | Automatic legality checks |
| Simulation | CPU/device golden tests |
| Extraction | Generated GPU implementation |
| Process target | GPU architecture, shape, dtype, layout |

The model is the design artifact. Kernels are generated implementation
artifacts. The point is not that users become hardware experts; the point is
that the system handles more of the hard implementation work automatically.

## Lineage

Chuck Moore is the inspiration for this project. He designed fabricated
silicon — ShBoom, MuP21, F21, and the 144-core GreenArrays GA144 — with OKAD,
a full-custom VLSI CAD system of a few thousand lines of Forth he wrote
himself, at a time when chip design was assumed to require commercial EDA
seats. One live environment did the whole job — layout, design rules,
simulation, extraction — and it stayed small because Moore intended to
understand every line of it.

Habu makes the same bet against the ML performance stack. PyTorch and Triton
are this era's industrial suites: capable, enormous, and opaque end to end.
Habu is the personal CAD system: the engine, checker, ARM64 assembler and
JIT, PTX backend, and CUDA driver interface are self-hosted checked Forth,
and `bin/hb` — the native binary that type-checks, compiles, and rebuilds
itself to a byte-for-byte fixpoint — is under 128 KB. No Python, no LLVM; the
external surface is `ptxas`, `libcuda`, and the OS. The system is small
enough to be read whole — by a person, or by an agent in one context window.

Habu diverges from Moore in one deliberate way. Moore trusted one careful
human author and had no use for type systems. Habu's highest-volume author is
an LLM, so the discipline Moore kept in his head is enforced by a checker:
stack effects, address spaces, extents, masks, and uniformity are verified
before a generated kernel ever runs.

## Why Habu?

Modern ML systems can run models on GPUs. That is not enough.

The hard part is what happens after the model changes:

- Which operations should fuse?
- Which intermediate tensors should never hit global memory?
- Which layout makes the next three operations coalesced?
- Which reductions need shared memory?
- Which shapes should use tensor cores?
- Which schedule is best for this batch size and GPU?
- Which backward path should save intermediates, and which should recompute?
- Which generated implementation is actually correct?
- Which performance claim is real, and which is just benchmark noise?

Today, answering them means a cascade of manual GPU work after every model
edit:

```text
change the model
  → adjust tensor layouts
  → rewrite or regenerate kernels
  → maintain backward paths
  → fix masks and edge cases
  → benchmark again
  → inspect profiler output
  → retune tile sizes
  → package the chosen implementation
```

Habu treats those questions as part of the model-design environment, not as
separate scripts, notebooks, and profiler archaeology: more model iterations,
less hand-written kernel work, and measured performance from the same live
environment.

## What exists today

Habu consists of three layers:

```text
Habu core
  self-hosted checked Forth: row-polymorphic stack-effect checking, native
  ARM64 engine (macOS + Linux), JIT, AOT with tree shaking, byte-for-byte
  fixpoint rebuild, native test gate, explicit trust manifest (TRUSTED.md)

Habu-PTX
  checked GPU kernel DSL (lib/ptx/): typed tile/span/matrix vocabulary,
  checked KERNEL: definitions that emit PTX, assemble with ptxas for sm_87,
  and run golden-vs-CPU on an NVIDIA Orin through a CUDA Driver API FFI

Maki
  model framework (maki/): tensor shape/dtype and tensor-scale arrays,
  autograd orchestration with numeric gradcheck, SGD-family + Adam
  optimizers, losses, a training loop that converges at tensor scale (host
  and a GPU SGD demo), fail-closed ONNX op import, and the eval harness that
  uses the checker as the correctness judge (pass@k)
```

Measured on the Orin (sm_87), documented in [`docs/eval-triton.md`](docs/eval-triton.md):

- Checked vectorized SAXPY at **bandwidth parity with Triton** (~63 GB/s v4).
- Numerically stable SOFTMAX-ROWS within **1 ULP** of the CPU golden.
- A reverse-mode AD transform (`lib/ptx/ad.f`) whose auto-derived
  SOFTMAX-ROWS-BWD **passes a device finite-difference gradcheck**.
- The stack-discipline bug class (missing store, wrong arity) rejected at
  **author time with zero GPU**, where Triton catches it at runtime — the
  property that makes agent-generated kernels safe to accept at scale.

The honest current claim is narrow:

```text
Habu can already demonstrate the shape of the model-CAD loop on small GPU
kernels.
```

The next claim to earn is stronger:

```text
Habu automatically fuses, coalesces, tiles, validates, profiles, and tunes
useful model blocks from one REPL.
```

## The target loop

The REPL is host-resident; the GPU sees generated kernels. The REPL
orchestrates the whole loop: model definition, lowering, fusion planning,
layout planning, tiling, PTX emission, assembly and driver loading, launch,
golden checks, gradchecks, profiling, autotuning, artifact caching.

A target interaction should feel like:

```forth
maki> MODEL: FFN ( x w1 b1 w2 b2 -- y )
        LINEAR GELU LINEAR ;

maki> OPTIMIZE FFN SHAPE batch=1 seq=128 dim=4096 TARGET sm_87
fusion:       LINEAR+BIAS+GELU fused; residual+norm fused
memory:       6 global passes removed
coalescing:   all hot loads coalesced; one masked tail path
schedule:     selected 64x128x32, 4 warps, 2 stages
numerics:     golden pass
backward:     generated; gradcheck pass
profile:      memory-bound region at 92% measured roof
cache:        implementation saved for shape/dtype/device

maki> TRY FFN WITH GELU-APPROX=poly3
numerics: max error within tolerance
profile:  1.08x faster on sm_87
cache:    promoted candidate
```

This is the differentiator: the model, implementation, validation,
measurement, and tuning loop are live together.

### Automatic mega-fusion

For memory-bound workloads, the fastest kernel is often the one you never
launch, and the fastest tensor is the one you never materialize. Habu should
fuse legal regions (`matmul → bias → activation`, `residual → norm → scale`,
`scale → mask → softmax`, `dequantize → compute → requantize`) — but bounded
and measured, splitting on register pressure, occupancy, barriers, numerics,
or a measured regression. Every fusion report explains its splits:

```text
fusion: 9 ops → 3 kernels                        (target report)
removed: 4 intermediate global writes, 4 intermediate global reads
split reason: reduction barrier before softmax normalization
expected traffic: 3.1x lower than unfused baseline
```

### Automatic memory coalescing

Habu should track shape, layout, alignment, vector width, address space,
masks, and lane mapping, so it can generate coalesced access patterns or
explain why it cannot:

```text
memory plan:                                     (target report)
  x: coalesced v4 global load, 16-byte aligned
  w: staged through shared memory, bank-conflict padding applied
  y: coalesced v4 global store
  edge: masked tail path for N mod 4
```

The user should not have to inspect lane arithmetic to discover that a model
edit turned a clean load into scattered memory traffic.

### Automatic tiling and scheduling

Tiling should be a generated, inspectable, tunable design artifact — block
size and vector width for elementwise kernels; row/block mapping, shared
memory, and stable accumulation for reductions; tensor-core fragments, tile
shapes, staging depth, warp layout, and epilogues for GEMM and attention:

```text
schedule: FFN.block1.matmul                      (target report)
  target: sm_87
  dtype: f32 input, tf32 tensor-core math, f32 accumulation
  tile: BM=64 BN=128 BK=32   warps: 4   stages: 2
  epilogue: bias + GELU fused
  status: golden pass, selected by device timing
```

The schedule is not a hidden pile of host-language glue. It is inspectable
from the same environment that defines the model.

## Habu versus kernel-first tools

Kernel-first tools are powerful when you already know the kernel you want.
Habu should be better when you are still changing the model and want the
system to keep up.

```text
Kernel-first workflow:                Habu workflow:
  user designs model                    user changes model
  user notices bottleneck               system updates fusion/layout/schedule
  user writes custom kernel             system validates and profiles the
  user maintains backward path            generated implementation
  user benchmarks and retunes           user sees whether the idea is worth
  user repeats after model changes        keeping
```

The intended effect is more useful model iterations per day, without a custom
GPU-kernel side quest for every promising idea.

## Habu versus framework-first tools

Framework-first tools make models easy to express and run. Habu aims to make
model implementation plans easy to inspect, specialize, and tune:

```text
Why did this model get slower?
Which fusion was applied?
Which intermediate tensors remain?
Are the hot loads coalesced?
Which tile shape won?
Did the generated backward pass pass gradcheck?
Is this memory-bound or compute-bound?
What changed between the previous artifact and this one?
```

Habu should answer those questions directly from the REPL.

## What needs to be built next

The roadmap is not "add more syntax." It is to build the optimization loop.
The reviewed plan with phases, acceptance criteria, and dependencies is
[`docs/model-cad.md`](docs/model-cad.md):

1. **Mega-fusion planner** — regions, legality, materialization points, split
   reasons, traffic estimates, measured results.
2. **Coalescing and layout planner** — layout/alignment/vector/lane facts and
   memory-plan reports.
3. **Schedule vocabulary and autotuner** — schedules as tunable, cached,
   replayable design artifacts.
4. **Tensor-core backend** — fragments, shared-memory staging, TF32/f16/bf16
   MMA, epilogue fusion, shape-keyed search.
5. **One-REPL command loop** — `MODEL:` `LOWER` `FUSE` `MEMORY` `TILE`
   `CERTIFY` `GOLDEN` `GRADCHECK` `PROFILE` `TUNE` `PROMOTE` `EXPLAIN`.
6. **Agent loop** — agents propose; legality, golden, gradcheck, and profile
   gates decide; only winners are promoted.

## The machinery: checked Forth

The reason Habu can automate hard GPU work safely is the substrate: a
self-hosted, checked Forth. `bin/hb` is the small native engine: it
type-checks Forth stack effects, JIT-compiles words to ARM64 code, rebuilds
itself to a byte-for-byte fixpoint, and can AOT-build standalone binaries.

```forth
: SQUARE ( i64 -- i64 ) dup * ;   \ accepted
: BAD    ( i64 -- i64 ) dup ;     \ rejected: leaves an extra i64
```

Checked definitions use ordinary Forth plus a typed stack comment. The checker
supports concrete types, nominal roles, `ptr a`, row variables, quotations,
return-stack effects, recursion, loops, control flow, locals, and
`CREATE ... DOES>`. Parametric types and algebraic data types (sum/enum/product
families with checked `MATCH`) are specified in
[`docs/type-families.md`](docs/type-families.md).

The same discipline extends to the GPU: Habu-PTX kernels are checked words
whose types carry address-space, extent, mask, and uniformity facts — that is
what makes automatic fusion, coalescing, and tiling decisions safe to
generate, and safe for agents to propose.

Words that cross compiler/runtime boundaries are explicit `TRUSTED:` or
`TRUST` sites tracked in [`TRUSTED.md`](TRUSTED.md). New Forth is checked
unless the boundary is deliberately documented and tested.

These internals matter because they make automatic optimization safe. They are
not the headline.

## Quick Start

Use the repo skills for current commands:

- [`skills/habu-bootstrap/SKILL.md`](skills/habu-bootstrap/SKILL.md) — recover
  missing `bin/hb` with Gforth 0.7.9+, refresh the self-hosted engine, and
  port bootstrap work to Linux/aarch64.
- [`skills/habu-gate/SKILL.md`](skills/habu-gate/SKILL.md) — run focused and
  full native gates with explicit pool and budget arguments.
- [`skills/habu-host-profiles/SKILL.md`](skills/habu-host-profiles/SKILL.md) —
  run host-class macOS and Jetson/Orin timing profiles.
- [`skills/habu-build/SKILL.md`](skills/habu-build/SKILL.md) — build AOT
  binaries and REPL images.

After `bin/hb` exists, normal work is Habu-native: the checked REPL, source
loading, self-refresh, AOT builds, and gates all run through `bin/hb`. Run
from the repo root, or from a tree where `src/`, `lib/`, `tools/`, and `test/`
are available. Generated images are regenerable build artifacts.

## Repository layout

```text
src/core/         checker, renderer, roles, combinators, hashes
src/arch/arm64/   ARM64 assembler, encoders, disassembler, mnemonics
src/arch/ptx/     PTX emitter/backend pieces
src/habu/         engine builder, JIT, AOT, tree shaker, profiler, debugger
src/os/           Linux ELF and macOS Mach-O target seams
lib/              checked Habu libraries
lib/ptx/          GPU kernel vocabulary, PTX codegen, autograd transform
maki/             model/tensor/autograd/training/eval framework over Habu-PTX
tools/            checked Habu automation, build, lint, PTX launch tools
test/             native Habu gate and focused suites
bench/            benchmarks
docs/             design docs, backend strategy, evals, methodology
skills/           operational recipes for agents and humans
```

## Docs

- [`CAD-PLAN.md`](CAD-PLAN.md) — the Model CAD design: planners, cost model,
  schedules, gates.
- [`docs/model-cad.md`](docs/model-cad.md) — the Model CAD campaign:
  phases, dots, milestones.
- [`docs/positioning.md`](docs/positioning.md) — product positioning and copy.
- [`docs/eval-triton.md`](docs/eval-triton.md) — measured Habu-PTX vs Triton
  evaluation on the Orin.
- [`docs/type-families.md`](docs/type-families.md) — algebraic data types and
  type families design.
- [`docs/bootstrap.md`](docs/bootstrap.md) — bootstrap, refresh, and porting.
- [`docs/forth.md`](docs/forth.md) — mandatory Forth style and checker rules.
- [`docs/debugging.md`](docs/debugging.md) — stepper, debugger, breakpoints,
  watchpoints, image dumpers, JIT dumpers, and native fallback boundaries.
- [`maki/README.md`](maki/README.md) — the Maki framework layer.
- [`PLAN.md`](PLAN.md) — the Maki + Habu-PTX build plan.
- [`STATUS.md`](STATUS.md) — current gate status.
- [`LESSONS.md`](LESSONS.md) — concise project memory.
- `.dots/` — active implementation tasks.

## Agent protocol

Agents should read [`LLM.md`](LLM.md) before editing.

For model-CAD work, agents should always report:

```text
user-visible benefit
fusion effect
memory/coalescing effect
tiling/schedule effect
correctness gates run
profile result
artifact/cache result
known limitations
```

A change is not complete merely because it emits code. A change is complete
when Habu can explain what it optimized, prove the result against references,
measure it on device, and tell the user what changed.
