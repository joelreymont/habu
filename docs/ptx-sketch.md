# Habu → PTX: v0 language spec

A tile-level, statically-checked GPU kernel DSL in Habu that emits PTX directly,
assembled by `ptxas` for sm_87 (Orin Ampere). The checker enforces a *defined*
set of source-level contracts as stack effects: address space, extent-relative
indexing, lane-mask discipline, uniformity, and tile/matrix shape. Strategy:
[`ptx.md`](ptx.md). This is a v0 surface for review, not a frozen grammar.

## Types (one spelling)

Checked signatures use explicit atom tokens, not single-letter metavariables:
`space-global`, `space-shared`, `extent-n`, `extent-r`, `extent-c`,
`mask-live`, `block-256`, and `align-16`. The prose below still uses `S/T/N/M`
as metavariables; executable checked code must use the explicit tokens.

| Type | Meaning |
| ---- | ------- |
| `f32 f16 bf16 u32 i32` | scalar element types (Orin sm_87: bf16/f16 yes, no fp8) |
| `ptr<space-global,f32>` | raw pointer in one space (no extent) — only at trusted boundaries |
| `span<space-global,f32,extent-n>` | base + **extent token** in one space |
| `span<space-global,f32,extent-n,align-16>` | … with alignment refinement |
| `matrix<space-global,f32,extent-r,extent-c>` | `R`×`C`, row-major, row stride `C` |
| `gridctx<block-256,extent-n,mask-live>` | flat grid-strided context over `N`; lane index = global `i` |
| `rowctx<block-256,extent-n,mask-live>` | one-block-per-row context over `N`; lane index = local `tid` |
| `tile<f32,block-256,mask-live>` | `B`-lane tile of `T` with **active-lane mask** `M`; inactive lanes are poison |
| `uniform<T>` | a `T` provably identical across all lanes of the block |
| `rowidx<extent-r>` | row index proven `< R` (sound only under the launch ABI, below) |

`%block B` requires `B` ∈ a legal CUDA block size: a **multiple of 32, `1 ≤ B ≤
1024`**. Warp count is `⌈B/32⌉`; a partial final warp uses an active-lane mask.
(Vector loads — `vec4<T>` / `LOAD.V4` — are **not in v0**; they need a dedicated
vector-lane + tail effect and land with the vectorization milestone.)

`S` maps to `space-global`, `space-shared`, `space-const`, or `space-local`.
**v0 implements `space-global` only;** the others are reserved. `N/R/C` map to
`extent-*` tokens: same token ⇒ the checker proves agreement; it does not know
the runtime value.

## Trusted boundaries (where extents/strides are asserted)

Raw pointers carry no extent; spans/matrices are minted at trusted constructors
that assert the runtime length — like Rust `slice::from_raw_parts`. Wrong length
*here* is unchecked; everything downstream is checked relative to it.

```
TRUSTED: MK-SPAN   ( ptr<S,T> u32 -- span<S,T,N> )          \ fresh extent token N
TRUSTED: MK-SPAN=  ( ptr<S,T> ptr<S,U> u32 -- span<S,T,N> span<S,U,N> )  \ SHARED N: asserts equal length
TRUSTED: MK-MATRIX ( ptr<S,T> u32 u32 -- matrix<S,T,R,C> )  \ rows cols; v0 matrix is DENSE row-major (stride = C);
                                                            \ asserts R*C <= 2^32-1 elements and R*C*sizeof(T) fits u64.
                                                            \ A pitched/strided matrix<S,T,R,C,P> is a later milestone.
```

A lone `MK-SPAN` yields a fresh `N` that unifies with nothing else — so two
independent spans are *not* assumed equal length. Kernels needing equal extents
(e.g. saxpy) take spans built by `MK-SPAN=` (or the launch ABI checks lengths).

## Launch ABI (makes grid/block facts sound)

A kernel declares its grid/block shape; the host launch glue (trusted) verifies
before launch: `blockDim == B`; `gridDim.x == R` (so `ROW : rowidx<R>` is sound);
and `gridDim.x * blockDim.x ≤ 2³²−1` (so the flat global index fits `u32`). A
mismatch is a **launch error, not UB**, and is a host-side check — not a
compile-time checker rejection.

**Scalar parameter uniformity (trusted rule):** a value loaded from a `.param`
scalar is `uniform<T>`; any value derived from `%tid`/`%laneid`/a `tile` is
lane-varying. Kernel scalar args (e.g. `a:uniform<f32>`) are uniform by this rule.

## Words (exact signatures)

```
GRID-CTX  ( span<S,T,N> -- gridctx<B,N> )      \ flat: lane i = ctaid*ntid+tid; mask = i < N
ROW-CTX   ( span<S,T,N> -- rowctx<B,N> )       \ row-local: lane = tid; mask = tid < N

\ The span carries the base address; the ctx carries lane index + mask only.
LOAD   ( span<G,T,N> gridctx<B,N> -- tile<T,B,M> )    \ masked coalesced load; M = ctx mask
LOAD   ( span<G,T,N> rowctx<B,N>  -- tile<T,B,M> )    \ (overload on ctx kind)
STORE  ( tile<T,B,M> span<G,T,N> gridctx<B,N> -- )    \ writes active lanes only
STORE  ( tile<T,B,M> span<G,T,N> rowctx<B,N>  -- )    \ row form needs a ROW-SPAN'd span<G,T,C>

SCALE        ( tile<T,B,M> uniform<T> -- tile<T,B,M> )      \ tile*scalar; lowers mul.rn (no contraction)
+. -. *. /.  ( tile<T,B,M> tile<T,B,M> -- tile<T,B,M> )     \ elementwise; mask M must match
B- B/        ( tile<T,B,M> uniform<T> -- tile<T,B,M> )      \ tile (op) uniform scalar broadcast
FMA.         ( uniform<T> tile<T,B,M> tile<T,B,M> -- tile<T,B,M> )  \ a*x+y, single rounding (fma.rn)
EXP.         ( tile<f32,B,M> -- tile<f32,B,M> )             \ ex2.approx.ftz(x*log2e); tolerance acceptance-gated

BLOCK-MAX BLOCK-SUM ( tile<f32,B,M> -- uniform<f32> )       \ mask-aware all-block reduction (see lowering).
                                                            \ Inactive lanes seed identity (-inf / 0). Requires
                                                            \ block-uniform reachability; rejects under any
                                                            \ lane-varying predicate.

ROW       ( -- rowidx<R> )                                   \ blockIdx.x; sound via launch ABI gridDim.x==R
ROW-SPAN  ( matrix<S,T,R,C> rowidx<R> -- span<S,T,C> )       \ base = r*C (checked), extent C
```

Vectorized load (`LOAD.V4`) is **deferred** to the vectorization milestone: it
needs a `vec4` lane type, a per-lane 16-byte alignment proof, and an explicit
scalar-residual (tail) effect — none of which v0 expresses.

Elementwise/collective words require matching mask token `M`; an op mixing two
different masks rejects. `tile<T,B,M>` never yields a defined scalar for an
inactive lane (poison), so "load returns 0" is gone — masks, not magic zeros.

## Kernel: vector add `y = a*x + y`

Equal length is *proven*: both spans share extent `N` (built by `MK-SPAN=`). No
collective ⇒ masked lanes may simply not store (a branch is legal here).

```forth
%block 256
KERNEL: SAXPY  ( span<G,f32,N>  span<G,f32,N>  uniform<f32> -- )  grid: ceil(N/256)
   {: x:span<G,f32,N>  y:span<G,f32,N>  a:uniform<f32> :}
   x GRID-CTX {: g :}             \ gridctx<256,N>
   x g LOAD  a SCALE              \ tile<f32,256,M> = a*x   (mul.rn)
   y g LOAD  +.                   \ + y                     (add.rn)
   y g STORE ;
```

## Kernel: numerically-stable softmax over each row

`where C <= B` is a checked constraint (a `C>B` instantiation rejects). Grid is
`R` blocks; `ROW` is bound by the launch ABI. Row-local ctx ⇒ lane = `tid` (not a
global index). Collectives ⇒ **bounds are predicated, never branched**, and the
reduction is mask-aware. Requires `C > 0`.

```forth
%block 1024
KERNEL: SOFTMAX-ROWS ( matrix<G,f32,R,C>  matrix<G,f32,R,C> -- )  grid: R   where C <= 1024
   {: in:matrix<G,f32,R,C>  out:matrix<G,f32,R,C> :}
   ROW {: r :}                    \ rowidx<R>
   in r ROW-SPAN {: xs :}         \ span<G,f32,C>
   xs ROW-CTX {: c :}             \ rowctx<1024,C> ; mask = tid < C
   xs c LOAD {: x :}              \ tile<f32,1024,M>
   x BLOCK-MAX {: m :}            \ uniform<f32>  (inactive lanes seed -inf)
   x m B- EXP. {: e :}            \ tile = exp(x - m)
   e BLOCK-SUM {: s :}            \ uniform<f32>  (inactive lanes seed 0)
   e s B/  out r ROW-SPAN c STORE ;
```

## Lowering rules

- **Module header is mandatory:** every emitted module begins
  `.version 8.3` / `.target sm_87` / `.address_size 64`.
- **Index math is unsigned/wide:** `mul.wide.u32` for byte offsets. The flat
  global index `ctaid*ntid+tid` cannot overflow `u32` because the launch ABI
  bounds `gridDim.x*blockDim.x ≤ 2³²−1`; element counts beyond that are rejected.
- **No silent contraction:** `SCALE` then `+.` → `mul.rn.f32` + `add.rn.f32`
  (two roundings). `FMA.` → `fma.rn.f32` (one). `EXP.` → `mul.f32 x,log2e` +
  `ex2.approx.ftz.f32` (tolerance acceptance-gated, measured on sm_87;
  `EXP.PRECISE` → libdevice).
- **Collectives use predication, not branches:** in a kernel with collectives,
  the bounds mask is carried as a predicate; *no thread leaves via `bra`* before a
  `shfl.sync`/`bar.sync`. Inactive lanes execute the collective seeded with its
  identity. A non-collective kernel (saxpy) may branch on the mask.
- **Collective lowering (`BLOCK-*`):** intra-warp `shfl.sync.down.b32` with the
  **full-warp membermask** (membermask is reachability — all executing lanes must
  be named — *not* data validity; inactive lanes are seeded with the identity
  separately, before the shuffle) → each warp lane 0 writes its partial to a `⌈B/32⌉`-slot shared
  array → `bar.sync` → warp 0 reduces the `⌈B/32⌉` partials (a partial final warp
  contributes the identity) → broadcast the `uniform` result via shared + `bar.sync`.
  Sound only because `B` is a multiple of 32, `≤ 1024`, and the barrier is reached
  block-uniformly (next section). This lowering depends on the verified barrier
  model (milestone 5).

```ptx
.version 8.3
.target sm_87
.address_size 64

.visible .entry SAXPY(.param .u64 p_x, .param .u64 p_y, .param .f32 p_a, .param .u32 p_n) {
    .reg .pred %p<2>;  .reg .f32 %f<5>;  .reg .b32 %r<6>;  .reg .b64 %rd<6>;
    ld.param.u64 %rd1,[p_x];  ld.param.u64 %rd2,[p_y];
    ld.param.f32 %f1,[p_a];   ld.param.u32 %r1,[p_n];
    mov.u32 %r2,%ctaid.x;  mov.u32 %r3,%ntid.x;  mov.u32 %r4,%tid.x;
    mad.lo.u32 %r5,%r2,%r3,%r4;        // i = ctaid*ntid + tid (unsigned)
    setp.ge.u32 %p1,%r5,%r1;  @%p1 bra DONE;        // no collective here: branch is legal
    mul.wide.u32 %rd3,%r5,4;
    cvta.to.global.u64 %rd4,%rd1;  add.u64 %rd4,%rd4,%rd3;  ld.global.f32 %f2,[%rd4];
    cvta.to.global.u64 %rd5,%rd2;  add.u64 %rd5,%rd5,%rd3;  ld.global.f32 %f3,[%rd5];
    mul.rn.f32 %f4,%f1,%f2;  add.rn.f32 %f4,%f4,%f3;        // a*x + y, two roundings
    st.global.f32 [%rd5],%f4;
DONE: ret;
}
```

## Uniformity and collectives (the rule)

`uniform<T>` is identical across all lanes; `tile<…>` is lane-varying. A
collective consumes a `tile` and produces a `uniform`. The checker tracks a
uniform/lane-varying effect on control flow and requires, for a collective or
`bar.sync`, **block-uniform reachability**: *every* thread of the block reaches
the same barrier the same number of times. A collective reached under any
lane-varying predicate — or under a uniform-but-not-block-wide branch that some
threads skip — **rejects** (it would deadlock or read inactive lanes). A uniform
*value* is necessary but not sufficient; block-uniform *reachability* is the
condition. Bounds are never a branch around a collective — they are the mask `M`.

## What is and isn't guaranteed

**Rejected at compile time:** a global op on a non-global span; load/store without
a ctx; a ctx whose extent ≠ the span's; mixing two mask tokens; a collective under
lane-varying control flow; `LOAD.V4` on an under-aligned
span; mismatched tile/matrix shapes; raw pointer arithmetic on a `span`.

**Not guaranteed (honest):** the runtime extent/stride asserted at `MK-SPAN*`/
`MK-MATRIX` (trusted); that the host passed matching `gridDim`/`blockDim` (the
launch ABI checks this at launch, not at compile time); shared-memory race/bank
behavior (deferred to the shared-memory milestone); occupancy/performance;
numerical accuracy beyond each op's stated contract. The bounds property is
*relational* — "indexing outside the declared span's extent is unrepresentable" —
not universal memory safety.

## Acceptance criteria (by milestone)

Which criterion lands with which milestone: 1–3 (emit, assemble, device run) →
M3 spike. Criterion 4 splits by what each negative needs: wrong-space / missing
ctx / extent mismatch / mixed masks / raw-ptr-arith → M4; collective-under-
lane-varying / non-block-uniform-reachability → M5; `C>B` / row-local-ctx-as-grid
→ M6 (they need `matrix`/`rowctx`/`where`). Criterion 5 (host launch tests) → M1
harness. Criterion 6: `SCALE`+`+.` two-rounding → M4; `EXP.` tolerance → M6.

1. `bin/hb` emits a header-complete `saxpy.ptx` from the checked source above.
2. `ptxas -arch=sm_87 saxpy.ptx` assembles with no warnings.
3. Runs on device via the CUDA Driver API; matches a CPU golden within tol.
4. **Negative checker tests reject** (each a minimal program, at compile time):
   wrong-space load; missing ctx; extent mismatch; mixed masks; row-local ctx
   used as grid ctx; collective under a lane-varying predicate; raw pointer
   arithmetic on a span; non-block-uniform reachability of a collective.
5. **Host launch tests** (runtime, not the checker): `blockDim != B`,
   `gridDim.x != R`, `C > B`, and `gridDim.x*blockDim.x > 2³²−1` each fail the
   launch.
6. Numerics: `SCALE`+`+.` is two-rounding (not fma) unless `FMA.`; `EXP.` within
   the tolerance measured and pinned on sm_87.

## LLM experiment (claim deferred until measured)

No "better LLM target" claim until this validates: kernels {vector-add,
row-reduce, argmax, softmax-row}; arms {Habu-PTX DSL, raw Triton} with the same
model + checker/compiler-in-loop repair; metrics {pass@k vs CPU golden, repair
rounds, output tokens to green, runtime correctness, achieved GB/s (% of memory
speed-of-light)}. Bar: the checked arm wins on repair rounds or tokens-to-green at
equal correctness, or there is no claim.

## Milestones

1. **Host enablement — a real C-ABI FFI + CUDA Driver harness (prerequisite,
   large).** Habu reaches the OS only via raw `svc` syscalls; it has **no
   userspace function-call ABI** (`C-CALL` is internal codegen; no `dlopen`/
   `dlsym`). This is an **AAPCS64 call trampoline** — only the subset the CUDA
   Driver calls use (scalar int/ptr/float args + out-pointers; no HFA/HVA/
   variadics), not the whole standard: x0–x7 int args, **v0–v7 FP args**, x8
   indirect-result, stack spill for args ≥9, callee-saved x19–x28 + 16-byte SP;
   pointer/out-param and `void**`-`kernelParams` struct marshalling with
   by-pointer readback; a **first-symbol bootstrap** (how `dlopen`/`dlsym` are
   resolved before any FFI exists — a build-linked libdl entry table or an ELF
   link-map resolver); `dlopen`/`dlsym` resolving libcuda at the **Tegra path**
   (`/usr/lib/aarch64-linux-gnu/tegra/libcuda.so`, not the toolkit stub); then the
   Driver harness (`cuInit`/`cuCtxCreate`/`cuMemAlloc`/`cuMemcpy` H2D+D2H/
   `cuModuleLoad`/`cuModuleGetFunction`/`cuLaunchKernel`) + the launch-ABI check.
   The linux `bin/hb` runs on `zed`; ensure a CUDA toolkit (`ptxas`) is installed.
   Blocks every on-device step.
2. **Checker parametric-type extension (prerequisite, large).** Core M2 support
   is the checker term machinery: `<`/`>`/`,` signature tokens, atom tokens for
   space/extent/mask/block/alignment, `T-PARAM` side tables for terms such as
   `span<space-global,f32,extent-n>`, field-by-field unify, render/record
   round-trip, and a self-host fixpoint rebuild. The remaining M2 surface is the
   PTX defining vocabulary: `KERNEL:`, `%block`, `grid:`, and `where`.
3. **Toolchain spike (no checker):** a minimal PTX encoder under `src/arch/ptx/`
   (a new ISA target sharing none of `src/arch/arm64/`) emits header-complete
   `saxpy.ptx` from hand-built IR → `ptxas -arch=sm_87` → run on the Orin via the
   M1 harness → CPU golden.
4. **Tile DSL v0 + negatives:** `span`/`gridctx`/`tile<T,B,M>`, `MK-SPAN=`,
   `GRID-CTX`, `LOAD/STORE`, elementwise; saxpy from checked source; the
   non-collective negative cases.
5. **Mask + uniformity + barrier model:** `uniform<T>`, the uniform/lane-varying
   effect, predicated bounds, a verified `bar.sync` phase; collective negatives.
6. **Collectives + softmax-rows:** `BLOCK-MAX/SUM`, `rowctx`, `matrix`,
   `ROW`/`ROW-SPAN`, `where C<=B`; `SOFTMAX-ROWS`.
7. **2D image layout:** pitched 2D `matrix`/tiles, borders/halos, shared tiling.
8. **Camera demosaic** (Bayer→RGB) on real frames vs reference.
9. **Bench + autotuner + precision:** GB/s & % speed-of-light, autotune, perf
   gate; f16/bf16.
10. **Vectorization:** `LOAD.V4` (alignment proof + tail).
11. **Attention + experiment:** multi-tile rows, a matmul/attention tile IR,
    shared staging + accumulator policy → fused softmax→flash-attention; then run
    the LLM matrix.

## Resolved M1/M2 Decisions

1. **Shape constraints are compile- vs launch-time.** Extent tokens (`N,R,C`) are
   runtime values, **equality-checked** at compile time. Inequalities/arithmetic —
   `where C<=B`, `C>0`, `ROW-CTX` `N<=B`, flat-grid coverage `grid*block ≥ N`,
   `grid: ceil(N/B)` — are **launch-time host checks**, not compile-time
   rejections (unless a shape becomes a const-generic). So the `C>B` and coverage
   acceptance tests are **host launch tests**, not checker-negatives.
2. **Mask token belongs on the ctx:** `gridctx<B,N,M>`/`rowctx<B,N,M>` so
   `STORE`/elementwise can require the same `M` as the tile.
3. **Kernel ABI:** the concrete `span`→`(base,len[,align])`, `matrix`→
   `(base,rows,cols[,stride])`, `uniform`→scalar lowering to `.param` + the
   `void**` `kernelParams` packing/lifetimes + equal-token runtime dedup.
4. **No overload resolution yet:** v0 should use **distinct** grid/row words
   (e.g. `LOAD`/`ROW-LOAD`) rather than overloading `LOAD` on ctx kind.
5. **Staged self-host bootstrap (M2):** the term/unify machinery is encoded in
   old syntax first, then the refreshed checker accepts parametric syntax. The
   real token syntax is prefixed atoms (`space-*`, `extent-*`, `mask-*`,
   `block-*`, `align-*`); single letters remain type/row variables.
6. **Numeric capability on arithmetic:** `+.`/`SCALE`/… are `T`-generic but lower
   to float ops (`mul.rn.f32`); add an int-vs-float capability constraint.
7. **Context interop:** use `cuDevicePrimaryCtxRetain` (the camera pipeline
   already owns a CUDA context), not `cuCtxCreate`.
