# Habu → PTX: local type inference

**Thesis:** in a checked concatenative kernel DSL, intermediate `{: x :}` bindings
should carry **no type annotation** — the checker already knows the top-of-stack
type, so it infers the binding. Annotation is reserved for the *contract edge*:
the kernel signature and the trusted constructors that assert extents. The rule is
**infer bodies, annotate boundaries** — the same boundary ML and Rust settled on,
for the same reason. This is a surface-ergonomics doc; the type system itself is
[`ptx-sketch.md`](ptx-sketch.md), the strategy is [`ptx.md`](ptx.md).

Inference here is not a convenience bolt-on. Because the types carry the
*relational* facts (address space, extent token, lane mask), inferring an
intermediate **threads the proof forward for free** — the later checked access
still fires, without the author restating it. That is the real prize; the saved
keystrokes are secondary.

## What is inferred vs annotated

| Position | Annotated? | Why |
| --- | --- | --- |
| Kernel signature `( in:… out:… -- )` | **Yes** | the checked *contract*; must not drift when the body is edited |
| Trusted constructors `MK-SPAN` / `MK-MATRIX` | **Yes** | the `from_raw_parts` boundary: the extent is *asserted* here, not inferred |
| Recursive / exported word effects | **Yes** | inference of a recursion needs a fixpoint; declared effect + verify |
| Intermediate `{: x :}` bindings | **No** | type = whatever the checker already computed on top of stack |
| Stack values left unnamed | **No** | the linear spine carries its own types |

The boundary is not aesthetic. See *Why not infer the signature* below.

## The mechanism

The checker walks a word and maintains the abstract stack at every point. A local
binding consumes the top *k* values; their inferred types are exactly the abstract
stack slots being popped. So `{: x :}` after `LOAD` binds `x` to whatever `LOAD`
pushed — already known — with **all its tokens intact**:

```
xs c ROW-LOAD        \ stack: ( … -- tile<f32,block-256,mask-live> )
        {: x :}      \ x : tile<f32,block-256,mask-live>   (inferred, tokens and all)
```

No unification is *introduced* by the binding; it is a read of state the checker
holds anyway. (Multi-value binds `{: a b :}` pop several slots, deepest name last,
matching the stack-comment convention.)

## Worked example: softmax-rows, annotated → inferred

The v0 sketch annotates every intermediate for didactic clarity:

```forth
%BLOCK 256
KERNEL: SOFTMAX-ROWS ( in:matrix<space-global,f32,extent-r,extent-c>
                       out:matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in:matrix<space-global,f32,extent-r,extent-c>  out:matrix<space-global,f32,extent-r,extent-c> :}
   ROW {: r:rowidx<extent-r> :}
   in r ROW-SPAN {: xs:span<space-global,f32,extent-c> :}
   xs ROW-CTX  {: c:rowctx<block-256,extent-c,mask-live> :}
   xs c ROW-LOAD   {: x:tile<f32,block-256,mask-live> :}
   x BLOCK-MAX {: m:uniform<f32> :}
   x m B- EXP. {: e:tile<f32,block-256,mask-live> :}
   e BLOCK-SUM {: s:uniform<f32> :}
   e s B/  out r ROW-SPAN c ROW-STORE ;
```

With local inference, every intermediate annotation is redundant — the checker
infers each — and the linear math rides the stack point-free, named only at the
fan-out / long-lived joints (`r` spans the kernel; `xs` fans to `ROW-CTX`+`LOAD`;
`c` spans `LOAD`→`STORE`):

```forth
%BLOCK 256
KERNEL: SOFTMAX-ROWS ( in:matrix<space-global,f32,extent-r,extent-c>
                       out:matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   ROW           {: r :}        \ long-lived: in-span + out-span
   in r ROW-SPAN {: xs :}       \ fan-out: ROW-CTX + LOAD
   xs ROW-CTX    {: c :}        \ long-lived: LOAD + STORE
   xs c ROW-LOAD  DUP BLOCK-MAX B- EXP.  DUP BLOCK-SUM B/  out r ROW-SPAN c ROW-STORE ;
```

The whole numerically-stable softmax is the two point-free phrases
`DUP BLOCK-MAX B- EXP.` and `DUP BLOCK-SUM B/`; each `DUP` feeds the fan-out
(`x`→max+subtract, `e`→sum+divide). Three names survive, each justified. The body
is shorter than the annotated form *and* shorter than a bare-stack form, and it
reads as dataflow rather than stack gymnastics.

`SAXPY` infers to almost nothing, since it is linear:

```forth
%BLOCK 256
KERNEL: SAXPY ( x:span<space-global,f32,extent-n>  y:span<space-global,f32,extent-n>  a:uniform<f32> -- )  GRID: ceil-n-256
   {: x y a :}                  \ inferred from the signature
   x GRID-CTX {: g :}           \ inferred: gridctx<block-256,extent-n,mask-live>
   x g LOAD  a SCALE  y g LOAD  +.  y g STORE ;
```

## The real prize: inference threads the proofs, not just the types

Inference here is over a system whose types carry **relational contracts**, so an
inferred intermediate keeps them. In the softmax body, `xs c ROW-LOAD` infers
`x : tile<f32,block-256,mask-live>` — and that `mask-live` token is the *same*
token `c` carries. So the closing `… c ROW-STORE` still discharges the mask check
(`ROW-STORE` requires the tile's mask token to equal the ctx's), even though nothing
in the body was annotated. The author wrote the dataflow; inference supplied and
**proved** the bookkeeping:

```forth
   xs c ROW-LOAD    \ x : tile<…,mask M>     where M is c's mask token
   …                \ B-, EXP., B/ preserve M (elementwise: same-mask in, same-mask out)
   … c ROW-STORE    \ ROW-STORE ( tile<…,M> span ctx<…,M> -- ) : M must match — it does, by inference
```

Lose inference and you would re-annotate `M` at every step; gain it and the
relation is carried, not restated. This is why inference is the *mechanism* of the
checked-but-terse surface, not a sugar layer on top of it.

## Why not infer the signature

Top-level effect inference is a trap: it lets a kernel's checked *contract* change
silently when the body is edited. Consider editing `SOFTMAX-ROWS` to forget the
final store of one column — with an inferred signature the kernel's declared
effect would quietly shift and nothing downstream complains; with an annotated
signature the body fails to unify with the declared `( in out -- )` and the edit
is rejected at the source. The annotation is the *promise*; inference fills the
inside and the checker proves the inside keeps the promise — exactly the README's
"inferred body effect unifies with the declared signature," now applied so that
only the **signature** needs spelling. The same logic forces explicit types at
`MK-SPAN`/`MK-MATRIX`: those assert a runtime length (the trusted boundary), so the
extent must be *named* there, never guessed.

## Three rules that keep inference sound and usable

1. **Branches and recursion still declare effects.** Straight-line code infers
   trivially. A conditional needs both arms to unify to one stack effect
   (row-polymorphic unification — the checker already does this); a recursive or
   exported word needs a declared effect because its inference would otherwise
   require a fixpoint. Keep declared effects on recursive/library words; infer the
   rest.
2. **Extent tokens stay nominal.** Inference may propagate only *proven*
   equalities (the same token), never *invent* agreement. A lone `MK-SPAN` mints a
   fresh `extent-n` that unifies with nothing, so two independent spans are not
   assumed equal length; `MK-SPAN=` is the explicit "these share `N`" constructor.
   Trusted constructor signatures spell this with `fresh-extent-*` /
   `fresh-mask-*` template atoms; each call mints rigid identities, while repeated
   templates inside one signature share one identity. Inference carries these
   tokens through bindings; it must not unify two fresh tokens just because the
   values flow together.
3. **Protect the diagnostics — they are the product.** Full inference's classic
   failure is reporting a mask/extent mismatch *far* from its cause. Since "the
   checker catches the bug" is the whole pitch, a misplaced error undermines it.
   Two mitigations: (a) annotations double as error *anchors* — adding one back at
   a suspect point turns a global unification failure into a local "expected `X`,
   got `Y`"; (b) a **`:type` form** — proposed `{: x:? :}` — binds `x` and prints
   the inferred type (including its tokens), so a human or an LLM can ask "what did
   you infer here?" mid-kernel without committing to an annotation.

## Division of labor with an LLM author

This is the clean split for [`ptx.md`](ptx.md)'s LLM-target hypothesis: the model
emits the **dataflow** — which word feeds which, the structure it is good at — and
inference + the checker supply and *prove* the **types**, the bookkeeping it is
worst at and most likely to fumble (a wrong mask, a mismatched extent, a global
load on a shared span). Fewer tokens to emit, fewer places to be wrong, and every
load-bearing one is verified. Inference turns "AI-authored kernel" from "trust the
model" into "the model proposes, the checker disposes." It also reshapes the
ergonomics argument: a stricter language is *cheaper* when the model writes most of
it, because the strictness is what makes the model's output trustworthy.

## Scope / status

- Design surface, paired with the v0 type system in [`ptx-sketch.md`](ptx-sketch.md).
  The checker's row-polymorphic stack-effect machinery already binds locals; the
  change this doc specifies is **dropping the required annotation on intermediates**
  and confining annotation to the signature + trusted constructors.
- The `{: x:? :}` show-inferred form is a **proposal**, not implemented.
- Inference does not weaken any guarantee: every contract in
  [`ptx-sketch.md`](ptx-sketch.md) (typed spaces, extent-relative bounds, mask /
  uniformity discipline) is still checked — it is just no longer *spelled* at every
  intermediate. The signature and the trusted boundaries remain the only places a
  human asserts, rather than proves, a type.
