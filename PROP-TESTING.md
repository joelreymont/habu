# PROP-TESTING.md — Property-Based Soundness Testing for the Checker

## The one invariant

habu's entire value rests on a single promise:

> **If the checker certifies a definition (`CHECK!` ⇒ −1), the body's real runtime
> stack effect equals its declared `( in -- out )`.**

A **false-certification** — a definition stamped certified whose body actually
does something else — is the only bug class that silently breaks the value
proposition. The three verdicts are asymmetric: `0` (rejected) and `1`
(uncheckable) are *safe* (they make no correctness claim); only `−1` (certified)
can be a *lie*. So the whole soundness question is narrow:

> **For every program the checker returns −1, is that program actually correct?**

The example-based gate (`T{ … }T`) only covers cases someone thought to write.
This session alone found three native false-certs (`leave`, bool-as-`i64`,
malformed-sig) — all of which passed the full green gate and were caught only by
hand review. This harness replaces *heroic review* with a *mechanical, continuous*
proof: every run, thousands of fresh programs are generated, checked, and **run**,
and any certify-but-wrong fails the build.

## Why execution, not a second checker

A false-cert is "checker says correct, but it isn't," so catching it needs a
**second source of truth about correctness**. We deliberately do **not** use the
gforth-hosted checker as a differential oracle: that would chain the native
checker to a bootstrap artifact forever (every native change mirrored in
`bootstrap/src` to keep the diff meaningful). The stronger, gforth-free oracle is
**the program's own behavior** — run it in `bin/habu` and measure what it really
does to the stack.

This is property-based testing: the *property* is the soundness invariant, the
*generator* produces typed Forth definitions, a failing case is a false-cert, and
**shrinking** reduces it to a minimal repro automatically (the by-hand work that
produced `?do 99 leave loop` this session). All the leverage is in the generator
— it must emit *well-formed, executable, terminating* definitions biased toward
the checker's hard edges, not random token soup that bounces off the parser.

### Honest scope

- **Arity / shape soundness** (the class that corrupts results — a body that
  leaves 2 where the sig says 1, `leave` carrying an extra value, a loop that
  grows the stack): **caught directly by run-and-compare.** This is the target.
- **Type-refinement** (bool-as-`i64`): the runtime erases types (`bool` and `i64`
  are the same 64-bit cell), so execution cannot see it. This class is weaker
  (bit-compatible, does not corrupt results) and belongs to *keep the type
  lattice correct + targeted tests*, **not** to this harness.
- **Metamorphic invariants** (composition, subsumption, render→reparse) need only
  the one checker and catch some cases execution can't — a second, complementary
  property (Phase 2).

## Architecture

```
generator ──▶ programs ──▶ [Pass 1: CHECK!] ──▶ verdicts ──┐
   (Python)   (Forth text)   (bin/habu)                     │
                                                            ▼
                              certified subset ──▶ [Pass 2: run + measure]
                                                     (bin/habu)
                                                            │
                                            measured arity ≠ declared? ──▶ shrink ──▶ report
```

Driver + generator live in `tools/` (Python, like `forth_lex.py`,
`check-all-errors.py`). The generated *programs* are Forth, run by `bin/habu`
(native, no gforth). Python is dev tooling, not part of the engine.

### The oracle protocol (validated against bin/habu)

The engine has **no `depth`** primitive, and its `CHECK!` hook rejects any
un-checkable helper, so measurement uses a marker sentinel and a helper defined
with checking *off*:

```forth
0 set-check                              \ harness words compile unchecked
variable D   -987654321 constant MK
: NAB  0 D ! BEGIN dup MK <> WHILE drop D @ 1+ D ! REPEAT drop D @ ;
\ NAB ( …vals MK -- n ) : consume everything above MK (and MK), return the count
```

Per program with declared `( i64*in -- i64*out )`:

```forth
MK  7 7 … 7   Gi   NAB . cr             \ push marker, `in` dummies, run Gi, count residual
```

`NAB` returns the number of items `Gi` left above the marker = the **measured
out-arity**; it also clears the stack (consumes the marker), so the next program
starts clean. The property: **measured == declared `out`**. If `Gi` consumed more
than its `in` dummies it eats `MK` and underflows `NAB` → the process traps →
**that is the false-cert signal** (consumed-too-much).

### Two-pass, batched (pipe mode does not recover from errors)

A rejected def is undefined, so its run line `MK … Gi NAB` raises *unknown word*,
which **aborts the whole pipe** (verified). Therefore:

- **Pass 1 — verdicts.** Install `: VH CHECK! dup . ; ' VH set-check`. Feed a
  whole batch of `s" #i#" type cr  : Gi ( sig ) body ;`. The hook prints the
  verdict per def on **stdout**; the reject diagnostic goes to **stderr** (so
  stdout stays clean `#i# / verdict` pairs). Defs never run here → no aborts.
- **Pass 2 — measure.** With `0 set-check`, re-feed only the **certified**
  subset's defs (define unchecked — they already certified) plus one runner each.
  All define successfully → no unknown-word aborts. A *certified-but-underflows*
  program traps mid-batch → the driver sees missing tags, **bisects** the batch to
  pinpoint, and records it as a false-cert (runtime trap).

Batch size ~100–500; one `bin/habu` spawn per batch per pass. 100k programs ≈
~2·(100k/B) spawns, parallelizable across worker processes. v1 may start
one-process-per-program for simplicity and add batching as the scaling step.

## The generator (v1 — the safe integer sublanguage)

Programs must be **runnable** (never fault), **terminating**, and **side-effect
free**, so v1 restricts to a sublanguage with all-`i64` inputs and bounded
control flow. The body is built by a **stack-depth-tracked random walk** starting
at `depth = in_count`, only ever picking an op whose input arity ≤ current depth
(so the body never underflows its inputs *at generation time*). Because the walk
tracks the net depth change, the generator knows the body's **true** out-arity and
sets the declared sig to either match (intended-certify) or be perturbed by ±1
(intended-reject) — a ~70/30 mix to exercise both verdicts.

**Allowed ops (weighted toward the checker's hard edges):**

| group | tokens |
| --- | --- |
| literals | small ints |
| stack | `dup drop swap over nip rot` |
| arith/logic | `+ - * and or xor 1+ 1- negate` |
| compare (→flag, i64-compatible) | `= <> < > 0= 0<` |
| branches | `if … else … then` (sub-bodies generated with chosen relative net) |
| loops | `<lo+k> <lo> ?do <neutral-or-perturbed body, may use i> loop` |
| locals | `{: a b :}` then references |
| return stack | balanced `>r … r> / r@` runs |
| quotations | `[: … ;] execute` |

**Forbidden in v1 (non-runnable or fault-prone):** `/ mod` (div0 traps),
`@ ! c@ c!` (need valid addresses), `here , allot create does>` (data region /
not a pure function), recursion (divergence risk), `."` (absent in habu),
`leave`-without-bounded-loop. Phase 2 adds typed inputs (`addr`/`str` over a
pre-allocated safe buffer, `char`, quotation params) and memory ops with supplied
buffers.

The generator is **seeded** (`--seed`) so any CI failure is reproducible.

## Failure taxonomy (what the driver reports)

| signal | meaning |
| --- | --- |
| certified, measured `out` ≠ declared `out` | **FALSE-CERT (arity)** — the headline bug |
| certified, Pass 2 traps / underflows | **FALSE-CERT (consumes too much)** |
| rejected, but runs and matches the sig | **false-reject** (incompleteness — logged, not fatal) |
| uncheckable (1) | ignored (sound by construction) |

Only FALSE-CERT classes fail the build. False-rejects are logged for precision
work but never gate-fail (rejecting a valid program is safe).

## Shrinking

On any FALSE-CERT, delta-debug the failing `(sig, body)` while preserving the
failure (still certifies **and** still mismatches): drop leading/trailing tokens,
remove whole balanced `if/loop/quot` spans, shrink literals, drop local bindings.
Emit the minimal program + declared sig + measured-vs-declared + seed. This is
exactly the three-token repros (`: BAD ( i64 -- i64 ) dup ;`,
`?do 99 leave loop`) produced by hand this session — automated.

## Phase 2 — metamorphic self-consistency (no oracle needed)

Properties that hold within the single checker and catch cases execution can't:

- **Composition.** If `A : ( x -- y )` and `B : ( y -- z )` both certify, then
  `: AB A B ;` must certify with `( x -- z )` and run-match.
- **Subsumption.** If a body certifies under `( i64 -- i64 )`, it must certify
  under the more general `( n -- n )` (and concrete must not be *more* permissive
  than generic).
- **Render round-trip.** A certified effect rendered to a string, re-parsed, and
  re-checked must be stable.

## Gate integration & running

- **Smoke (in `test/run.sh`):** fixed seed, a few hundred programs, parallel
  workers, sub-second; fails the gate on any FALSE-CERT.
- **Sweep (manual / CI nightly):** `tools/prop-test.py --count 100000 --seed N
  --jobs K`; emits a JSONL report and a minimized repro per failure.
- **Regression:** every minimized counterexample is frozen as a `T{ … }T` case so
  it can never silently return.

## File plan

- `tools/prop_gen.py` — the typed-Forth generator (safe sublanguage, seeded).
- `tools/prop-test.py` — the two-pass driver: batch-check, batch-run-measure,
  compare, shrink, report (imports `prop_gen`).
- `test/run.sh` — smoke invocation.
- `test/prop-corpus/` — frozen minimized counterexamples (regression).
