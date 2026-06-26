# Habu → PTX: autograd

**Thesis:** reverse-mode automatic differentiation is a *syntactic reversal* of a
concatenative program, so it fits Habu without a runtime tape. A kernel is a
composition `w₁ w₂ … wₙ`; its gradient is the reversed pipeline of adjoints
`wₙ′ … w₂′ w₁′`. AD is a compile-time pass over the IR, and — the payoff — the
backward it emits is checked by the same type system as the forward, so the
gradient is **verified**: a mask / extent / address-space mistake in a gradient is
a compile error, not a corrupted training run. This is the forward-kernel thesis
of [`ptx.md`](ptx.md) applied to the part an LLM is *most* likely to get subtly
wrong. Surface conventions follow [`inference.md`](inference.md); the type system
is [`ptx-sketch.md`](ptx-sketch.md).

Status: **design, not implemented.** AD does not exist in the tree yet. It slots
after the forward collective milestone (M6): every adjoint the flagship
softmax→attention path needs is already a forward primitive. The genuinely new
work is the reverse pass, an algebraic-simplify layer, and a save-vs-recompute
policy — itemised under *What is new work*.

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

## The reverse pass

Given a forward word `W`:

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

## Worked example: softmax backward

Forward (from [`inference.md`](inference.md)):
`LOAD DUP BLOCK-MAX B- EXP. DUP BLOCK-SUM B/`. Reversing, substituting adjoints,
and simplifying yields the known closed form

> `dx = y ⊙ (dy − Σ(dy ⊙ y))`

expressible in the *same primitives* and fully checked. The signature shares the
`extent-r`/`extent-c` tokens across `y`, `dy`, `dx`, so a single `ctx` is valid for
all three spans (same token ⇒ proven agreement):

```forth
%BLOCK 1024
KERNEL: SOFTMAX-ROWS-BWD ( y:matrix<space-global,f32,extent-r,extent-c>
                           dy:matrix<space-global,f32,extent-r,extent-c>
                           dx:matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-1024
   ROW {: r :}
   y  r ROW-SPAN {: ys :}
   dy r ROW-SPAN {: dys :}
   ys ROW-CTX {: c :}                  \ extents agree by shared token ⇒ valid for ys, dys, and dx's span
   ys  c LOAD {: yt :}                 \ y  tile   (fan-out: ⊙dy and the final ⊙)
   dys c LOAD {: dyt :}                \ dy tile   (fan-out: ⊙y and the − s)
   dyt yt *. BLOCK-SUM {: s :}         \ s = Σ(dy ⊙ y)            uniform, mask-aware
   dyt s B-  yt *.                     \ (dy − s) ⊙ y  = dx
   dx r ROW-SPAN c STORE ;
```

Locals only at the fan-out tiles (`yt`, `dyt`); the math is point-free; the mask
token threads from `LOAD` through `*.`/`B-` to `STORE`. The gradient is checked
exactly as the forward is.

## Memory adjoints and accumulation

`LOAD` and `STORE` are adjoints: the reverse of a gather is a scatter of the
cotangent into the input's gradient buffer; the reverse of a store is a load from
the output's gradient buffer. When a forward value is read **more than once**
(fan-in across the grid), its cotangent contributions must **accumulate** — the
adjoint of a gather is a scatter-*add* (`red.global.add` / `atom.global.add`,
arch-gated on sm_87). A value read exactly once per row (softmax) needs a plain
store; the AD pass decides add-vs-store from the forward's read multiplicity,
which the type/effect system already tracks.

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

- **Reverse pass** over the typed IR word list (steps 1–4 above).
- **Algebraic-simplify / peephole layer** so derived backwards reach closed form;
  this is the IR/opt layer [`ptx.md`](ptx.md) already flags as not yet present.
- **Save-vs-recompute policy** + scatter-add lowering for accumulating adjoints.
- **`VJP:` table** for the M6 forward primitives (all listed above are primitives
  already specified; `SOFTMAX-ROWS-BWD` is buildable once `BROADCAST` is named).

## Why it matters

A wrong custom backward is the nastiest silent bug in ML and the place an LLM is
most likely to be subtly wrong. PyTorch cannot check it; here it is a compile
error. **Verified gradients** are a property the runtime-tape model structurally
cannot offer — and the strongest form of the [`ptx.md`](ptx.md) LLM-target
hypothesis, now covering the gradient and not only the forward.
