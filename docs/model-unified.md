# Unifying MODEL: onto the equation engine

Decision record + design, ratified 2026-07-19 (Joel, dot
`habu-decide-model-retirement-bd76a741`): **the familiar name `MODEL:` survives;
the machinery behind it is replaced** — the DEFTYPE pattern. The old
single-running-value DSL with the `x:RxC` shape grammar retires; the equation
engine (today's `SPEC:`, `maki/spec.f`) becomes the op-authoring backend; at the
end `SPEC:` is renamed `MODEL:` territory-wide. This doc is the design the
migration dots implement. Every current-behavior claim carries a file:line.

## What exists today (the two surfaces)

- **`MODEL:`** (`maki/cad.f:22`): `MODEL: NAME ( x:RxC w:RxC ... -- y ) OP OP ... ;`
  — a package-scoped colon definer whose body is op words plus named references
  (`>V NAME` binding at `cad.f:568`, name references + transposed refs via
  `CAP-TOKEN` at `cad.f:559`). The whole composition is checker-certified at
  declaration time through `CAP-COMPILE-RUN` (`cad.f:598`). Shapes come from the
  positional `x:RxC` grammar (`PARSE-SHAPE`, `cad.f:461`); ops dispatch over the
  32-entry op-kind enum (`maki/op-kind.f:20`) with hand-written host references
  (`maki/executor.f:343`) and hand-written adjoints (`maki/backward.f:372`).
- **`SPEC:`** (`maki/spec.f:391`): one einsum line
  (`O[m n] = A[ IX[m] k ] B[n k] * +SUM k`) derives (1) the checked kernel words
  `NAME-EL`/`NAME` through the certified `XG-EVAL` boundary, (2) the dataflow
  record (`SPEC-*` queries), (3) the shape obligations (`SPEC-*-EXTENT@`).
  Extents/tensors come from `EXTENT:`/`TENSOR:` declarations, not a per-model
  grammar. It generates single kernels; it has no graph, no autograd.

## End state (the one surface)

```
128 EXTENT: #T   64 EXTENT: #C   ...
TENSOR: X ( #T #C )   TENSOR: W1 ( #C #F )  ...

MODEL: ATT-SCORE  S[q k] = Q[q c] K[k c] * +SUM c ;     \ an equation op
MODEL: GPT-BLOCK ( x w1 w2 -- y )                        \ a composition
   LAYERNORM  ATT-SCORE  SOFTMAX-ROW  ...  >V h
   h RESIDUAL-ADD ;
```

One keyword, two body forms distinguished by what follows the name: an `=`-line
is an equation (the current SPEC: grammar, verbatim); a `( sig )` opens a
composition (the current MODEL: body forms minus the shape grammar — op words,
`>V` naming, references). Both are certified at declaration time exactly as
today. Shapes are never written in a model line: an equation carries them via
its extents; a composition derives them from its operands' declarations, checked
for extent compatibility at declaration time (replacing `SHP-LEGAL?`'s 2D
broadcast classes, `cad.f:338`, with extent unification).

## How an equation joins the trainable graph

**One new op-kind, not one per equation.** The op-kind enum gains a single
`equation` kind whose attrs cell (the `maki/model-ir.f:116` mechanism the
segment-attention op already uses) carries the equation's registry slot. The
executor arm dispatches to the equation's generated host word; nothing else in
the executor/arena substrate changes. Registration happens at equation-
declaration time: name → (kernel xt, dataflow record, extents) in the equation
registry `maki/spec.f` already keeps.

**Derived adjoints — the payoff.** The adjoint of an einsum is another einsum:
for `O[free] = F0 F1 ... * +SUM ct`, the gradient w.r.t. factor `Fj` is the
equation whose output indices are `Fj`'s indices, whose factors are `dO` plus
every other `Fi`, summed over all indices not free in `Fj`. The declaration
generates these adjoint equations mechanically through the same parser +
emitter (they are ordinary equations), so the backward arm (`maki/backward.f:372`)
gets ONE `equation` case that runs the pre-derived adjoints — where every other
op-kind needed a hand-written `BW-STEP-*`. Correctness protocol: every derived
adjoint is finite-difference-checked by the same harness that validates the
hand-written ones (`maki/adam-train.f` gradient checks) before the kind is
enabled for training.

**Honest limit — gathers.** The adjoint of a gather read (`A[ IX[m] k ]`) is a
scatter-ADD into `dA`, which the multiply-then-sum grammar cannot state. Until a
scatter-add primitive exists, an equation containing a gather registers
forward-only: using it in a composition under training is a declaration-time
named reject (`E-CAD-GRAD`), never a wrong gradient. The scatter-add primitive
is its own dot; embedding lookup (nanoGPT's `wte`) needs it and is the natural
acceptance case.

## Stages (each gate-provable, in order)

1. **Equation op-kind**: `equation` kind + registry wiring + executor forward
   arm + extent-unification shape check for compositions that call equations.
   Acceptance: attention's `S = Q Kᵀ` runs inside a composition, matching
   `maki/attention.f` MM-NT.
2. **Derived adjoints**: the adjoint generator + finite-difference validation +
   the single backward arm; gather equations reject under training.
   Acceptance: equation-op GEMM trains identically to the `matmul` op-kind on
   the `maki/adam-train.f:224` fixture.
3. **Migrate**: every `MODEL:` user in maki re-declares without the shape
   grammar (inputs by tensor declaration; ops unchanged). The old grammar's
   uses drop to zero.
4. **Delete**: `PARSE-SHAPE` (`cad.f:461`) and the positional shape-binding
   path; `SHP-LEGAL?` broadcast classes replaced by extent unification.
5. **Rename**: `SPEC:` → the freed `MODEL:` spelling; `maki/spec.f` →
   `maki/model.f`; suites, FILEMAP, docs. (Same choreography as the DEFTYPE
   stage-C rename, e64b6b84.)

Until stage 5, the two keywords coexist with the freeze already in force: the
old `MODEL:` gains no capabilities (`habu-fix-model-dsl-d066701e`).

## Open questions (decided here unless Joel overrides)

- **Nonlinearity spelling inside compositions**: keep the existing op words
  (`GELU`, `SOFTMAX-ROW`, ...) unchanged — they are already domain-named.
- **The 2D IR stays.** Equations lower onto the same rows×cols memory substrate
  per the ratified BTC design (`docs/batch-sequence-design.md` Option D);
  equation extents map to (rows, cols) at lowering, batch via the segment
  attribute. A true N-D IR is explicitly out of scope for this program.
- **`MODEL-CAND:`/dry-run seams** (`cad.f`): survive unchanged; the equation
  form inherits `SPEC-CHECK$`/`SPEC-CAND:` (`maki/spec.f:404`) as its test seam.
