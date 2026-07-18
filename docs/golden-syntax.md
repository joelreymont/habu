# Golden-authoring syntax: making dataflow definitions read like the math

Design exploration (no implementation). The gathered GEMM of
`docs/case-tma-stride.md` / `docs/tma-gather.md` is the working example:

    O[m,n] = Σk A[ix[m],k] · B[n,k]     A: M×K, B: N×K, O: M×N, ix: M

## The problem with the current idiom

`maki/matmul.f` / `maki/attention.f` style — `{: :}` locals + manual stride
arithmetic:

```forth
: GG-EL ( ptr a ptr a ptr a n n n n -- r ) {: ab:ptr ixb:ptr bb:ptr m:n nn:n k:n kk:n :}
   0.0  kk 0 ?do
      ab  ixb m I-GET kk *  i +  T-GET   bb  nn kk *  i +  T-GET   f* f+
   loop ;
```

Correct, checkable, and honest about cost — but the index arithmetic is where
the math lives and where the eye slides off. `ixb m I-GET kk * i +` encodes
`A[ix[m], i]` only if you already know it does; a transposed pair of extents
type-checks fine and fails only at GOLDEN time. For a system whose
highest-volume author is an LLM, the gap between "reads plausibly" and "is
right" is exactly where defects breed. The stated style goal
(`docs/forth.md`: compose small typed words into nice-reading checked DSLs)
points at closing this gap with words, not comments.

## Candidate A — accessor-defining words (words only, no checker change)

A defining word per tensor bakes the stride math once:

```forth
M K  TENSOR: AT     N K  TENSOR: BT     M N  TENSOR: OT     M  ITENSOR: IXT

: GGEMM ( -- )
   M 0 ?do  N 0 ?do
      0.0  K 0 ?do
         j IXT I@  i  AT @   k' i BT @  f* f+     \ A[ix[m],k] * B[n,k]
      loop  j i OT !
   loop loop ;
```

`TENSOR:` defines `AT @ ( r c -- val )` / `AT ! ( val r c -- )` closing over
base and extents. Wins: index math written once per tensor, not per use;
bodies read as subscripts. Cost: one defining word (~30 lines). Limit: the
checker still sees only `n n -- r`; swapping `j`/`i` at a call site still
type-checks. A alone is the readable-but-unverified version of the goal — a
skeleton for B, not a resting point.

## Candidate B — extent-typed accessors (the workhorse; bounded checker work)

Same surface as A, but `TENSOR:` declares nominal extent roles and the
accessor signatures carry them:

```forth
EXTENT: #M   EXTENT: #N   EXTENT: #K

#M #K TENSOR: AT      \ AT @ ( idx<#M> idx<#K> -- val )
#N #K TENSOR: BT      \ BT @ ( idx<#N> idx<#K> -- val )
#M    ITENSOR: IXT    \ IXT I@ ( idx<#M> -- idx<#M'> )  — value-level row index
```

Now `i j AT @` with the loop roles flipped is an **author-time type error**:
`idx<#N>` where `idx<#M>` was demanded. This is MISSING.md Foundation A
(data-driven integer roles) applied to maki — the extent roles are declarable
integer types, not new checker special cases; loop words bind their induction
variable to the extent they iterate (`#K ?DO` yields `idx<#K>`). The gather
index tensor's *element* type is itself `idx<#M'>` — the row space it selects
into — which is precisely the fact `uniqidxctx` wants witnessed later on the
device side. One declaration feeds both the golden and the plan.

Cost: rides on Foundation A (already the named prize in MISSING.md);
maki-side it is the same ~30-line defining word as A plus signature emission.
No new parser, no new syntax class — words all the way down.

### Status: what shipped, and the loop-induction gap (be honest)

Candidate B shipped as `maki/extent.f` + `maki/extent-tensor.f` (`EXTENT:` /
`TENSOR:` / `ITENSOR:`, index family `ix<extent>`, accessors `NAME@`/`NAME!` and
gather `NAME@`). The delivered author-time flip protection is on **accessor
calls**: feeding an `ix` of the wrong extent to an accessor is a checker reject.

**The `#K ?DO` yields `idx<#K>` promise above (line 68) is NOT yet implemented.**
A `#K 0 ?DO` loop hands the checker only the extent *size* (a plain `n`), not the
extent *type*, so the loop's induction variable stays a bare `n`. The shipped
surface crosses it explicitly — `#K 0 ?DO  i >#K …  loop` — and that injector
`>#K` accepts **any** plain `n`: it range-guards the value at runtime (throws
`E-EXT-RANGE` outside `[0,#K)`) but does **not** statically bind the counter to
the extent the loop iterates. So `i >#M` written inside a `#K`-loop type-checks.
Closing that hole — a loop form whose induction variable is typed `ix<#K>` —
needs a checker/loop-semantics change and is tracked by follow-up dot
`habu-extent-bound-loop-a70a49b3`. Until it lands, the explicit `>#extent`
crossing at loop entry is the honest, range-guarded interim binding, and the
accessor-call flip protection is where the static guarantee actually bites.

## Candidate C — the spec word (the schematic; later, generated *from* B)

The CAD endgame: the einsum-like statement is the design artifact itself.

```forth
SPEC: GGEMM  O[m n] =  A[ ix[m] k ] B[n k] * +Σk ;
```

One spec, three derivations: (1) the CPU golden — generated as Candidate-B
code, so it is checked, not trusted; (2) the dataflow the planner consumes
(`idxctx` over `ix`, contraction over `#K`) — today reconstructed by hand in
the emitters; (3) the shape/extent obligations for PROMOTE. The autograd
layer already walks compositions; a spec walker is the same species. Cost: a
small parsing word over the existing sig-grammar machinery — but its value
depends on B existing (without extent roles the generated code is Candidate-A
quality, plausible-not-proven), so it is sequenced after, not instead.

## Decision

**C is the default.** Once `SPEC:` lands, it is *the* way goldens are written:
a new dataflow definition is a `SPEC:` line, and hand-written accessor bodies
(B) are the escape hatch for what the spec grammar cannot yet express — not
the norm. B is C's substrate, not a destination: build order is Foundation A
(checker roles) → B (extent-typed accessors, ~30 lines) → C generating B.
A is subsumed (it is B minus the signatures — build it once, with them).
This keeps faith with both constraints: the small-system budget (one defining
word + Foundation A the checker needs anyway; the spec word is ~a page of
parsing on machinery that exists) and the no-stopgap rule (every stage is
checked; nothing readable-but-unverified ships as an idiom). The gathered
GEMM lands as the first `SPEC:` golden and becomes the `docs/tma-gather.md`
regression; attention/matmul migrate to `SPEC:` lines opportunistically, not
as a rewrite campaign.

### Status: SPEC: shipped (what it expresses, what still needs hand-written B)

`SPEC:` shipped as `maki/spec.f`. Surface (one design choice per line; alternatives
in the dot report): a token before `[` is a `TENSOR:`/`ITENSOR:` name matched exactly
(SPEC: appends `@`/`!`); a bare token inside `[...]` is a lower-case index variable
whose extent is `#` + its upper-case (`m` → `#M`, crossing `>#M`); a gather is
`NAME[var]` nested in a factor bracket; the product combiner is `*` and the reduction
is `+SUM <index>` (the ASCII spelling of `+Σ` — the multi-byte Σ was rejected as
fragile and hard to type). From one parse SPEC: derives three things: (1) the checked
candidate-B golden — two words `NAME-EL` (the contraction element) and `NAME` (the
free loops + store), certified through the same `XG-EVAL` boundary the accessors use,
so it is checked, not trusted; (2) a dataflow record (free vs contraction index
variables, per-factor index structure including gathers) exposed as `SPEC-*` query
words; (3) the PROMOTE shape obligations (output-shape extents + contraction extents)
exposed as `SPEC-*-EXTENT@`.

**What SPEC: expresses:** the gathered-GEMM family — a multiply-then-sum contraction
with up to two free (output) indices and up to two contraction indices (the habu
`i`/`j` loop-counter limit), any number of product factors, and a gather on any
factor index. `SPEC: GGEMM O[m n] = A[ IX[m] k ] B[n k] * +SUM k ;` reproduces the
gathered GEMM golden and is proven numerically equal to a plain-buffer reference
(`maki/spec-test.f`).

**What still needs hand-written candidate-B bodies:** any op the multiply-then-sum
schematic cannot state — nonlinearities, softmax, normalization, movement (reshape /
transpose / concat / scatter), pure elementwise with no reduction, and outputs or
contractions beyond two indices. Those stay hand-written `NAME@`/`NAME!` accessor
bodies (candidate B is the escape hatch, exactly as this Decision intends).

**Two honest limits carried forward:** the generated loop counter is still not
extent-typed, so generated bodies use the explicit `i >#EXT` crossing exactly as the
hand-written GGEMM does (tracked by `habu-extent-bound-loop-a70a49b3`); an
extent-transposed spec is therefore caught by the checker on the generated accessor
call (a load-time reject), not by the loop. And the dataflow / shape-obligation
records (2)+(3) are self-contained: no live maki-planner or PROMOTE consumer reads
them yet (the planner drops the contraction axis and PROMOTE consumes gate verdicts,
not shapes), so the integration boundary is a future gate in `maki/cad.f` that reads
these records.
