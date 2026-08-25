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
`NAME[var]` nested in a factor bracket; the product combiner is `*` or `·` and the
reduction is the trailing `+SUM <index>` or the prefix `Σ<index>` (see "Unicode math
spellings" below). From one parse SPEC: derives three things: (1) the checked
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

### Status: Unicode math spellings (the equation surface reads like the math)

The ASCII-only retreat is reversed (dot `habu-unicode-math-spellings`). The equation
lexer normalizes the small confusable set so identical-looking codepoints are ONE
token — no silent lookalike soup:

| meaning        | ASCII                | Unicode (both lex to the ASCII token)                 |
|----------------|----------------------|--------------------------------------------------------|
| summation      | `+SUM <index>`       | `Σ<index>` — U+03A3 GREEK CAPITAL SIGMA · U+2211 N-ARY SUMMATION |
| product        | `*`                  | `·` — U+00B7 MIDDLE DOT · U+22C5 DOT OPERATOR           |

The summation is accepted in **two grammatical positions**: the **prefix** form
`Σ<contraction indices> <factors>` (as the pitch writes it) and the **trailing** form
`<factors> +SUM <contraction indices>`. Both parse to the identical equation. The
product token is accepted **infix** between factors (`A · B`) or as the **trailing**
combiner (`A B *`); either sets the "multiply all factors" flag.

**Canonical pretty form** (one, fixed here): prefix summation, infix product, real
Unicode glyphs —

    O[m n] = Σk A[ix[m] k] · B[n k]

The ASCII spelling `O[m n] = A[ ix[m] k ] B[n k] * +SUM k` stays legal forever
(terminals, greps, diffs) and is the only form used inside byte-oriented Forth tests
that must not carry multi-byte bytes.

**Fail closed on everything else:** any OTHER non-ASCII byte in an equation is the
named `E-SPEC-SYNTAX` reject, and the diagnostic (`SPEC-REJECT$`) names the offending
codepoint as `U+<hex>` (e.g. a stray U+2212 MINUS SIGN reports `U+2212`). Only the four
confusable codepoints above are decoded — no general Unicode machinery, no tables.
`maki/spec-test.f` proves both members of each pair produce byte-identical kernel
output, the pitch line runs as written, and the stray-codepoint reject fires with the
codepoint named.

### Status: broadcast and elementwise forms (the sublayer glue)

The contraction-only grammar (`OUT[free] = factors [*] +Σ ct`) cannot state the
three broadcast shape classes the Model CAD checker legalizes (`maki/cad.f`
`SHP-LEGAL?`): row broadcast `1×C` (bias add), scalar `1×1` (scale), and same-shape
elementwise (residual / add / mul). The multi-head attention sublayer (output-projection
bias, residual adds) and the GPT-2 block need them. Dot `habu-spec-broadcast-forms`
adds an **elementwise form** to the same recursive-descent parser:

    OUT[free] = term { (+ | ·) term }          -- NO +Σ / Σ  (that is the contraction form)
    term      = TENSOR [ index... ]            -- index list is a SUFFIX of `free`

The presence of a reduction (`+Σ` / trailing `+SUM`) selects the contraction form; its
**absence** selects the elementwise form. A term's index list must be a **suffix** of the
output's free list, and the missing **leading** axes are the broadcast (replicated) axes —
so the shape class falls out of the rank difference:

| class                         | `SHP-LEGAL?`     | term rank vs output | example (canonical)          |
|-------------------------------|------------------|---------------------|------------------------------|
| same-shape (residual/add/mul) | `SHP-SAME-OK?`   | equal               | `O[m n] = A[m n] + B[m n]`    |
| row broadcast `1×C` (bias)    | `SHP-ROW-OK?`    | output − 1          | `O[m n] = A[m n] + b[n]`      |
| scalar `1×1` (scale)          | `SHP-SCALE-OK?`  | 0 (empty)           | *(wall — see below)*          |

**Canonical spelling.** Elementwise product reuses the existing product token: infix `·`
(U+00B7 MIDDLE DOT, ASCII `*` stays legal). Elementwise **addition** is `+` — **ASCII
only, no new Unicode codepoint** (mathematical `+` has no blessed confusable; any non-ASCII
byte remains the named `E-SPEC-SYNTAX` reject naming the codepoint). A single expression
takes **one** combiner: `+` and `·` cannot mix (avoids precedence), and a reduction with a
`+` is rejected (`+` is elementwise-only). New authored example lines use the canonical
form; the ASCII spelling (`*`) stays legal for byte-oriented tests.

**Derived artifacts** are the same three `SPEC:` already derives — the checked
candidate-B golden (element word + free-loop store, reusing the extent-typed accessors),
the planner dataflow record (`SPEC-*`, now `SPEC-CT-N = 0` for the elementwise forms), and
the PROMOTE shape obligations (`SPEC-*-EXTENT@`). A **wrong-extent broadcast** (a `1×C`
declared over the wrong extent) is the same generated-accessor **checker reject** the
transposed contraction produces; a **non-suffix** index list (a column `R×1` broadcast) is
the named `E-SPEC-ARITY` reject.

**Derived adjoints** (gradchecked in `maki/spec-test.f` against a central finite
difference): the adjoint of an elementwise form is **another `SPEC:` equation** riding the
same parse+emit+register pipeline. For `O[free] = t0 (+|·) t1 …`, factor `Tj`'s gradient is
`dO` (carried by the output tensor name) reduced over `Tj`'s broadcast axes — additive:
same-shape → a copy `dA[m n] = dO[m n]`, row-broadcast bias → the column-sum contraction
`db[n] = dO[m n] +SUM m`; multiplicative (product rule): same-shape hadamard →
`dA[m n] = dO[m n] · B[m n]`. Every non-scalar adjoint stays inside the grammar; an
out-of-grammar one (e.g. a MUL with >3 factors) fails closed at `E-CAD-GRAD`, never a wrong
gradient.

**The scalar `1×1` wall (interim).** A scalar-broadcast term is a **rank-0** factor tensor
(`S[]`), and its full-sum adjoint `dS[] = Σ …` is a rank-0 output. `maki/extent-tensor.f`'s
accessor generator cannot emit a rank-0 accessor (`TENSOR: S ( )` references an undefined
`x0` in the row-major offset), so the scalar form is **unauthorable** and fails closed as a
rank mismatch (`E-SPEC-ARITY`). This is an **interim** wall, not the design: the correct
long-term fix is **rank-0 accessors in `maki/extent-tensor.f`** (emit `0` for the offset and
an empty projection when the extent list is empty), after which the scalar form — forward
and its full-sum adjoint — falls out of this same suffix machinery with **no grammar
change**. Until then, a compile-time scalar stays a plain named op (`ATTN-SCALE!`), exactly
as the attention temperature already is.

### Status: batched free-extent forms (BTC-2)

The contraction grammar above sums over `+Σ` and outputs 1–2 free axes. Batched attention
needs a **free (non-contracted) batch/head axis that rides every factor and the output**, so
the GGEMM is the *same* contraction **replicated** over it (`docs/batch-sequence-design.md`
§5 BTC-2, §4.3). Dot `habu-extent-roles-b` adds this on the BTC-7 product/factorization
substrate:

    B FREE-EXTENT: #B      -- an extent ROLE that is free (batch / head / replication)
    S[b h i j] = Σk Q[b h i k] · K[b h j k]     -- b,h batch; i,j GEMM output; k contracted

**Canonical spelling.** A batch/head axis is declared with **`FREE-EXTENT:`** (identical to
`EXTENT:` — same size word, same `>#name` injector — but the role is marked *free*). In the
equation the **batch indices lead** the output index list (`S[b h i j]`, not `S[i j b h]`);
they must appear in **every** factor. The `Σ`/`·` operators and the `#`-per-index-var
convention are unchanged. `i` (query) and `j` (key) are distinct extents even at equal
length — the transposed-operand pattern.

**Free extents type as free.** `FREE-EXTENT:` marks the role free two ways: maki-side (so
`SPEC:` splits the leading batch indices from the GEMM indices) and in the checker's free
set. A **contraction over a free extent is then a load-time checker reject** (exit 70
class): `SPEC:` emits a per-equation witness `<NAME>-RSUM ( ix<ct-ext> -- redx<ct-ext> )`,
and the BTC-7 rule rejects `redx<free>` at signature parse — the cross-sequence leak is a
type error, not a runtime bug. An inner (head-dim) contraction extent compiles silently.

**Derived artifacts** are the same three, extended: the checked candidate-B golden is now
three words — `<NAME>-EL` (unchanged: every free index is a projected arg, loop the
contraction), `<NAME>-GEMM` (takes the batch indices as `ix` args, loops the GEMM axes) and
`<NAME>` (loops the batch axes, calls `-GEMM`) — the split keeps each word within habu's two
loop counters. The dataflow record gains `SPEC-BATCH-N` / `SPEC-BATCH@` (the replication
axes) and the PROMOTE record `SPEC-BATCH-EXTENT@` (their magnitudes — the free-extent strides
/ TMA box dims the planner derives under the SMEM budget, `docs/tma-gather.md:90-92`; the live
planner consumer is BTC-6, not this leg).

**Derived adjoints** (gradchecked in `maki/spec-batched-test.f` against a central finite
difference, with **batch isolation** proven — a cross-batch perturbation has zero effect on
another batch's gradient): the adjoint of a batched contraction is the batched **transposed**
contraction with the **same free extents riding along**. For `S[b h i j] = Σk Q[b h i k] ·
K[b h j k]`, `dQ[b h i k] = dS[b h i j] · K[b h j k] +Σj` — another batched equation whose
`b,h` are free on both sides, so they are never summed. It rides the same
`EQ-ADJ-BODY`+emit pipeline as the contraction adjoints; batched equations are not composable
2D ops (rank->2 factors), so the `<NAME>-ADJj` words are generated for gradcheck but not
registered. Mis-formed batched specs fail closed before codegen: a batch axis after a plain
output index, a factor missing a batch axis, or a free role in an elementwise form are each
the named `E-SPEC-ARITY` reject.
