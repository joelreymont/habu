# Habu → PTX: autograd

**Thesis:** reverse-mode automatic differentiation is a *syntactic reversal* of a
concatenative program, so it fits Habu without a runtime tape. A kernel is a
composition `w₁ w₂ … wₙ`; its gradient is the reversed pipeline of adjoints
`wₙ′ … w₂′ w₁′`. AD is a compile-time pass over the IR, and — the payoff — the
backward it emits is checked by the same type system as the forward, so a mask /
extent / address-space mistake in a gradient is a compile error, not a corrupted
training run. This is the forward-kernel thesis of [`ptx.md`](ptx.md) applied to
the part an LLM is *most* likely to get subtly wrong. Surface conventions follow
[`inference.md`](inference.md); the type system is [`ptx-sketch.md`](ptx-sketch.md).

## What "verified" covers — and what it does not (review 2026-06-26)

The checker proves **types**: address space, extent-relative bounds, mask, and
uniformity. It does **not** prove that a VJP entry or an algebraic rewrite is the
correct *derivative*. A wrong rule — a dropped term, a sign flip, a fan-out treated
as a permutation (the `OVER` bug this review found) — has identical tile in/out
*types* and so **type-checks and ships a silently wrong gradient**, the exact bug
this doc claims to eliminate, merely relocated from a hand-written PyTorch backward
into this VJP table + the simplifier. So "verified gradient" is scoped to the
mask/extent/space class. The derivative-rule class is verified a different way and
it is **mandatory, not optional**: every `VJP:` entry and every generated backward
must pass a device-run **finite-difference gradcheck** (central differences vs the
analytic VJP, per-element relative tol, randomized inputs) as a hard gate, and the
algebraic simplifier carries a numeric-equivalence test per rewrite rule. No
derivative-class "verified" claim until gradcheck is the gate.

Status: **v0 implemented, verification still scoped.** `lib/ptx/ad.f` implements
the VJP table/reverse pass for the checked PTX words covered by
`lib/ptx/ad-test.f`, and the checked backward fixtures exercise the stack/type
surface. Remaining work is device finite-difference gradcheck as the hard gate,
DAG validation hardening, algebraic simplification, save-vs-recompute policy,
and a public typed zero-seeded row-load or per-collective identity for the current
backward emitter path.

## Why concatenative is the right substrate

PyTorch records a **runtime tape** because Python is not compositional: it logs
ops as they execute, then walks the log backward, and a custom kernel must ship a
hand-written `backward` registered as an `autograd.Function`. A wrong one corrupts
training silently. Habu has no tape to build — the program *is* the composition,
and the adjoint is the reversed composition of adjoints. Reverse-mode AD becomes a
list reversal plus a per-word substitution, performed and **checked** at compile
time.

## Adjoints are type-duals

Every primitive carries a pullback word whose type is the *transpose* of the
forward effect. The `tile`/`uniform` split makes the classic AD dualities explicit
in the types rather than implicit in an engine:

| forward | type | adjoint | adjoint type |
| --- | --- | --- | --- |
| `DUP` (fan-out) | `( t -- t t )` | `+.` (sum) | `( t t -- t )` |
| `+.` | `( t t -- t )` | `DUP` | `( t -- t t )` |
| `BLOCK-SUM` (reduce) | `( tile<f32,B,M> -- uniform<f32> )` | `BROADCAST` (fill) | `( uniform<f32> -- tile<f32,B,M> )` |
| `BROADCAST` | `( uniform -- tile )` | `BLOCK-SUM` | `( tile -- uniform )` |
| `LOAD` (gather) | `( span ctx -- tile )` | `STORE` / scatter-add | `( tile span ctx -- )` |
| `SCALE` (a·x) | `( tile uniform -- tile )` | `( dz a x -- dx da )` | dx=a·dz, da=Σ(dz⊙x) |
| `B-` (x − s) | `( tile uniform -- tile )` | `( dz -- dt ds )` | dt=dz, ds=−Σdz |
| `EXP.` | `( tile -- tile )` | `( dz y -- dz⊙y )` | saves output `y` |
| `*.` | `( tile tile -- tile )` | `( dz x y -- dz⊙y  dz⊙x )` | saves primals |
| `BLOCK-MAX` | `( tile -- uniform )` | scatter to argmax lane | subgradient |

`DUP ↔ +.`, `BLOCK-SUM ↔ BROADCAST`, and `LOAD ↔ STORE` are **mutual adjoints**
with transposed types. `BROADCAST` is the named form of the implicit broadcast
already inside `B-`/`B/`.

Linear primitives have data-free adjoints; nonlinear ones (`*.`, `EXP.`,
`BLOCK-MAX`) consume *saved primals or outputs*. That saved set is the tape's
replacement, and it is finite and known at compile time.

## VJP registration

Adjoints are paired words. The AD pass looks each forward word up by name:

```forth
VJP: +.         ( dz       -- dx dy )       DUP ;                  \ cotangent copies to both addends
VJP: DUP        ( dz1 dz2  -- dz )          +. ;                   \ fan-out's adjoint is a sum
VJP: BLOCK-SUM  ( ds       -- dtile )       BROADCAST ;            \ reduce's adjoint is a fill
VJP: B-         ( dz       -- dt dscalar )  DUP BLOCK-SUM NEG ;    \ tile gets dz; scalar gets −Σdz
VJP: EXP.       ( dz y     -- dx )          y *. ;                 \ dx = dz ⊙ y  (y is the saved output)
VJP: SCALE      ( dz a x   -- dx da )       OVER SCALE  -ROT *. BLOCK-SUM ;  \ dx=a·dz, da=Σ(dz⊙x)
VJP: *.         ( dz x y   -- dx dy )       ... dz⊙y , dz⊙x ;
```

(`OVER`/`-ROT` are stack moves the checker erases to register renames — see
[`inference.md`](inference.md): stack juggling has cognitive, not runtime, cost,
so VJP bodies still prefer names at fan-out joints.)

## Full VJP table (the M6 primitives)

Every M6 forward primitive and its adjoint. `Σ` is `BLOCK-SUM`; ⊙/⊘ are
elementwise mul/div; "saves" lists the forward values the backward needs — the
tape replacement, supplied by save or recompute (below). Linear ops save nothing.

**Elementwise** `( tile tile -- tile )`:

| forward | z = | adjoint | saves |
| --- | --- | --- | --- |
| `+.` | x + y | `dx=dz, dy=dz`  →  `DUP` | — |
| `-.` | x − y | `dx=dz, dy=−dz`  →  `DUP NEG` | — |
| `*.` | x ⊙ y | `dx=dz⊙y, dy=dz⊙x` | x, y |
| `/.` | x ⊘ y | `dx=dz⊘y, dy=−dz⊙z⊘y` | y, z |

**Broadcast** `( tile uniform -- tile )` **and** `FMA.`:

| forward | z = | adjoint | saves |
| --- | --- | --- | --- |
| `SCALE` | a · x | `dx=a·dz, da=Σ(dz⊙x)` | a, x |
| `B-` | x − s | `dx=dz, ds=−Σdz`  →  `DUP BLOCK-SUM NEG` | — |
| `B/` | x ⊘ s | `dx=dz⊘s, ds=−Σ(dz⊙z)⊘s` | s, z |
| `FMA.` | a·x + y | `da=Σ(dz⊙x), dx=a·dz, dy=dz` | a, x |

(`B+`/`B*`, if added, are symmetric: `B+` → `dx=dz, ds=Σdz`; `B*` → `dx=s·dz, ds=Σ(dz⊙x)`.)

**Unary and collectives:**

| forward | z = | adjoint | saves |
| --- | --- | --- | --- |
| `EXP.` | exp(x) | `dx=dz⊙y`  →  `( dz y -- dx ) y *.` | output y |
| `BLOCK-SUM` | Σx | `dx=BROADCAST(ds)` (masked) | — |
| `BLOCK-MAX` | max x | `dx = ds` at the arg-max lane, 0 elsewhere (sub-gradient) | x, m |

**Tie-break (required, not optional).** When two lanes equal the max the arg-max
lane is not unique and the sub-gradient is ill-defined. The forward `BLOCK-MAX`
reduction computes only the max *value* (currently through the shared-memory
thread-0 fold) and does not by itself pin an index. So the forward contract fixes
a **deterministic** winner — **lowest global lane index** — and lowers the adjoint
selector to match it; the backward routes the **entire** `ds` to that single lane.
A tie-input gradcheck fixture (two lanes equal max) must assert `Σ(dx)=ds` and that
the chosen lane equals the forward's. (`docs/stdlib.md`'s "smallest index" rule is
for the CPU `A-ARGMAX` and does NOT carry to the GPU `BLOCK-MAX`.)

**Memory** (the adjoint reverses direction):

| forward | adjoint | note |
| --- | --- | --- |
| `LOAD ( span ctx -- tile )` | `STORE` of `dt` into the input's gradient span | scatter-**add** (`red.global.add`, arch-gated) if the input is read >1× across the grid; plain store if read once |
| `STORE ( tile span ctx -- )` | `LOAD` of `dt` from the output's gradient span | plain load |

**Stack and structural:**

| forward | adjoint | note |
| --- | --- | --- |
| `DUP ( t -- t t )` | sum the two cotangents | type-directed: `+.` for a `tile`, scalar add for a `uniform` |
| `DROP ( t -- )` | push a zero of the dropped value's **exact** type | `tile<…,M>` or `uniform<T>` — not an untyped "zero tile" |
| `SWAP` / `ROT` | inverse permutation (self for `SWAP`) | genuine permutation; reorders cotangents, no data |
| `OVER` / `TUCK` / `2DUP` (**fan-out**) | **sum** the duplicated value's two cotangents (like `DUP`) | NOT a permutation — `OVER ( a b -- a b a )` copies `a`, so its adjoint adds the two cotangents of `a`; mis-modeling it as a permutation silently drops a gradient term (and `SCALE`'s VJP body uses `OVER`) |
| `ROW` `ROW-SPAN` `GRID-CTX` `ROW-CTX` `MK-SPAN` `MK-MATRIX` | **lifted unchanged** | addressing/index/context carry no data gradient; the backward *recomputes* the same addressing (as `SOFTMAX-ROWS-BWD` recomputes `ROW`/`ROW-SPAN`/`ROW-CTX`) |

The table closes the system: the **only** adjoints that are not already M6
primitives are `BROADCAST` (the named form of the implicit broadcast inside
`B-`/`B/`) and the `BLOCK-MAX` arg-max **select** (a masked scatter — the one
genuinely new primitive the AD layer needs). Everything else reuses a forward
primitive. That is why `SOFTMAX-ROWS-BWD` is buildable the moment `BROADCAST` is
named, and why a full transformer block's backward needs only the `BLOCK-MAX`
select beyond M6 + matmul (M11).

## The reverse pass

Given a forward word `W`:

**v0 scope (straight-line only).** List-reversal reverses *dataflow*, not *control
flow*. A data-dependent `IF`/loop/`RECURSE` is exactly what forces PyTorch's
runtime tape (reversing it needs reversed iteration + per-iteration state), so v0
**rejects, fail-closed with a diagnostic,** any forward containing control flow
entering the reverse pass — it does not fall through to a type-correct-but-wrong
reversal. Control-flow reversal (loop adjoint = reversed loop with per-iteration
save/recompute) is a separate, larger capability with its own gradcheck; dot it.

1. **Linearise** `W` to its IR word list `w₁ … wₙ` (the checker already produces
   the typed sequence).
2. **Reverse and substitute:** emit `VJP[wₙ] … VJP[w₁]`.
3. **Thread cotangents** through the reversed effects: where the forward *fanned
   out* (`DUP`), the reverse *sums* (`+.`); where the forward *consumed two
   inputs*, the reverse *produces two cotangents*. Effects line up because the
   adjoint types are transposes.
4. **Supply saved values** to nonlinear adjoints, per the save-vs-recompute
   policy below.
5. **Simplify** (algebraic + peephole) so the derived backward collapses to the
   closed form rather than a literal reversal. This pass is new — see
   [`ptx.md`](ptx.md) on the missing IR/opt layer.
6. **Check** the result. The backward is an ordinary kernel; the v0 contracts
   (typed spaces, extent-relative bounds, mask / uniformity discipline) all apply.
7. **Gradcheck** the result on device (finite differences vs the analytic VJP).
   Step 6 proves *types*, not the *derivative* — see *What "verified" covers*.
   A backward that type-checks but fails gradcheck is a defect, not a pass.

## Worked example: softmax backward

Forward (from [`inference.md`](inference.md)):
`LOAD DUP BLOCK-MAX B- EXP. DUP BLOCK-SUM B/`. Reversing, substituting adjoints,
and simplifying yields the known closed form

> `dx = y ⊙ (dy − Σ(dy ⊙ y))`

expressible in the *same primitives* and fully checked. The signature shares the
`extent-r`/`extent-c` tokens across `y`, `dy`, `dx`, so a single `ctx` is valid for
all three spans (same token ⇒ proven agreement). **Caveat (same honesty as the
forward, ptx-sketch.md §"What is and isn't guaranteed"):** token *agreement* is
proven, but the *runtime* extent each token stands for is **asserted** at the
trusted `MK-SPAN*`/`MK-MATRIX` boundary, not checked. So the AD pass must mint each
gradient-buffer span to **share the primal's extent token** (via `MK-SPAN=` /
shared-token construction); only then is `len(dx)=len(y)` proven rather than
re-asserted. A gradient-buffer length mismatch is otherwise a trusted-boundary bug,
not a compile error:

The public checked surface still needs `ROW-LOAD-Z` or per-collective identities
before this can be ordinary source; the current device proof uses the emitter-only
`EMIT-ROW-LOAD-Z` for `dy` so inactive lanes contribute zero instead of `-inf`.

```forth
%BLOCK 256
KERNEL: SOFTMAX-ROWS-BWD ( y:matrix<space-global,f32,extent-r,extent-c>
                           dy:matrix<space-global,f32,extent-r,extent-c>
                           dx:matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   ROW {: r :}
   y  r ROW-SPAN {: ys :}
   dy r ROW-SPAN {: dys :}
   ys ROW-CTX {: c :}                  \ extents agree by shared token ⇒ valid for ys, dys, and dx's span
   ys  c ROW-LOAD {: yt :}             \ y  tile   (fan-out: ⊙dy and the final ⊙)
   dys c ROW-LOAD-Z {: dyt :}          \ target public zero-seeded dy load
   dyt yt *. BLOCK-SUM {: s :}         \ s = Σ(dy ⊙ y); generic mask-safe sum is dotted
   dyt s B-  yt *.                     \ (dy − s) ⊙ y  = dx
   dx r ROW-SPAN c ROW-STORE ;
```

Locals only at the fan-out tiles (`yt`, `dyt`); the math is point-free; the mask
token threads from row loads through `*.`/`B-` to `ROW-STORE`. Once the public
zero-seeded load or per-collective identity lands, the gradient is checked exactly
as the forward is; until then, the checked fixture and the emitter path are a v0
proof, not the final public source surface.

## Memory adjoints and accumulation

`LOAD` and `STORE` are adjoints: the reverse of a gather is a scatter of the
cotangent into the input's gradient buffer; the reverse of a store is a load from
the output's gradient buffer. When a forward value is read **more than once**
(fan-in across the grid), its cotangent contributions must **accumulate** — the
adjoint of a gather is a scatter-*add* (`red.global.add` / `atom.global.add`,
arch-gated on sm_87). A value read exactly once per row (softmax) needs a plain
store. **Correction (review 2026-06-26):** the effect system does *not* track read
multiplicity — `docs/effects.md` has no multiplicity/linearity effect, and read
multiplicity here is an inter-thread, *grid-global* aliasing property that a
per-thread checker structurally cannot see. So the AD pass must **not** silently
guess: scatter-*add* is the **conservative default** for every `LOAD` adjoint;
plain store is an opt-in refinement only behind a *proven-read-once* witness, never
an inference. A forward that reads an input across multiple blocks whose backward
uses plain store must be rejected (or must lower to scatter-add). Tracking
read-once soundly is a first-class **checker capability** (a substructural/affine
effect over gradient buffers) — dot it; do not assume it exists.

## Checkpointing / rematerialization

The nonlinear adjoints need saved primals/outputs — the compile-time replacement
for the tape. Two policies, chosen per value:

- **Save:** stash the forward value (e.g. the softmax output `y`) to global memory
  and reload it in the backward. Cheap for small per-row state.
- **Recompute:** re-run part of the forward inside the backward kernel. For fused
  kernels (attention) recompute usually wins — it trades cheap FLOPs for avoiding
  a global round-trip, the standard FlashAttention argument.

The pass picks save-vs-recompute by estimated cost; the checker validates either
way because both produce ordinary typed kernels.

## Modes

Reverse mode (above) is what ML wants: one scalar loss, many parameters, so one
backward pass yields all gradients. Forward mode (push a tangent through the
forward effect, no reversal) is cheaper for the opposite shape (few inputs, many
outputs) and is the trivial case for a concatenative IR — the tangent rides
alongside the primal with no list reversal. v0 targets reverse mode; forward mode
is a later, smaller addition.

## What is new work

- **A general PTX IR + opt layer** (fold/DCE/CSE/peephole). The simplify step
  below needs it; [`ptx.md`](ptx.md) §3 confirms only a gforth-bootstrap peephole
  exists, so this is built fresh and is a **prerequisite** of the simplifier, not
  part of it. (Alternatively scope AD-v0 to literal reversal and dot the
  closed-form simplifier as a follow-on.)
- **Reverse pass** over the typed IR word list (steps 1–4 above), **straight-line
  only** — fail-closed reject on `IF`/loop/`RECURSE`; control-flow reversal is a
  separate dotted capability.
- **Algebraic-simplify / peephole layer** so derived backwards reach closed form;
  runs on the IR layer above. **Each rewrite rule carries a numeric-equivalence
  test** — a wrong rewrite type-checks but changes the gradient.
- **Save-vs-recompute policy** with an **explicit documented cost model** + a test
  that save and recompute yield within-tol-identical gradients; + **scatter-add**
  lowering as the conservative default for accumulating adjoints. The primitive,
  `red.global.add.f32`, is **VERIFIED on sm_87** (2026-06-28): ptxas assembles it
  warning-free and a 256-thread launch atomically accumulates to 256.0 on the Orin
  (`tools/ptx/redadd-device-test.f`, closes habu-ptx-ad-verify) — no `atom.global.add`
  CAS-loop fallback needed.
- **`VJP:` table** for the M6 forward primitives, **each entry carrying a
  finite-difference gradcheck** (the table is itself hand-written backwards — the
  thing ML most fears; it is not trustworthy until gradchecked). `SOFTMAX-ROWS-BWD`
  is buildable once `BROADCAST` is named, the simplifier exists, and its derived
  form passes gradcheck (not merely the checker).
- **A gradcheck harness** (device-run central differences vs the analytic VJP) as a
  hard gate over every entry and every generated backward — see *What "verified"
  covers*.

## Why it matters

A wrong custom backward is the nastiest silent bug in ML and the place an LLM is
most likely to be subtly wrong. Habu can make mask/extent/address-space mistakes
in the backward a compile error, but derivative-rule mistakes still need the
device finite-difference gradcheck gate. The stronger claim is a checked gradient
surface plus numeric gradient validation, not static proof of calculus.
