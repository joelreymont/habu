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
old host-side checker as a differential oracle: that would chain the native
checker to a retired mirror forever. The stronger, gforth-free oracle is
**the program's own behavior** — run it in `bin/hb` and measure what it really
does to the stack.

This is property-based testing: the *property* is the soundness invariant, the
*generator* produces typed Forth definitions, and a failing case is a false-cert.
All the leverage is in the generator — it must emit *well-formed, executable,
terminating* definitions biased toward the checker's hard edges, not random token
soup that bounces off the parser.

### Honest scope

- **Arity / shape soundness** (the class that corrupts results — a body that
  leaves 2 where the sig says 1, `leave` carrying an extra value, a loop that
  grows the stack): **caught directly by run-and-compare.** This is the target.
- **Type-refinement** (bool-as-`i64`): the runtime erases types (`bool` and `i64`
  are the same 64-bit cell), so execution cannot see it. This class is weaker
  (bit-compatible, does not corrupt results) and belongs to *keep the type
  lattice correct + targeted tests*, **not** to this harness.
  Beyond execution, **leave/exit baits** — non-neutral `leave` / divergent `exit`
  programs a sound checker must reject — guard that class against regression: if
  one ever certifies, its real arity differs from its declared sig and the harness
  fails.

## Architecture — self-hosted, in-process

The whole harness is **checked-Forth's untyped tooling tier, run by `bin/hb`**
(`test/prop-test.f`): the PRNG, the generator, the driver and the measurement are
all habu. Each program is defined, checked and run **in the same process** via
the engine's re-entrant `evaluate` — no host scripting, no gforth, no per-program
spawning.

```
for each program (seeded PRNG):
  GEN          build ": G ( i64*in -- i64*out ) <body> ;" in a buffer
  ['] VH set-check   evaluate <def>     \ VH = `CHECK! dup VERD !` → VERD = verdict
  VERD = -1 ?  →  evaluate "depth BASE ! <in×7> G depth BASE @ - CLEAR-MEAS"
                                                   \ measured out-arity
               EVALERR=0 and measured ≠ declared  →  FALSE-CERT (arity)
               EVALERR=1 (trap, consumed too much) →  FALSE-CERT (trap)
```

`evaluate` ( a u -- ) was added to the engine for exactly this: it saves the
outer input + compile state, runs the string through the interpret loop, and
returns to the caller — restoring state and setting `EVALERR` on a clean end or
on an error (so a bad generated program is discarded, not fatal). See LESSONS,
"Re-entrant EVALUATE". The check hook **must leave the verdict on the stack**
(`CHECK! dup VERD !`, not `CHECK! VERD !`) — dropping it underflows the
compiler's stack and corrupts the next `evaluate`.

### The oracle protocol (validated against bin/hb)

Measurement uses the ANS Forth `depth` primitive. The baseline is stored in a
variable instead of on the data stack, so the oracle does not depend on any
distinguished stack value that a generated program can collide with or consume:

```forth
variable BASE  variable MC
TRUSTED: CLEAR-MEAS  ( vals... n -- n )
   dup MC !  BEGIN MC @ 0 > WHILE  swap drop  MC @ 1- MC !  REPEAT ;
```

Per program with declared `( i64*in -- i64*out )`:

```forth
depth BASE !  7 7 … 7   Gi   depth BASE @ - CLEAR-MEAS
```

`depth BASE @ -` returns the number of items `Gi` left above the pre-run baseline
= the **measured out-arity**. `CLEAR-MEAS` is the one trusted arbitrary-tail
measurement boundary: it drops those residual values and leaves only the count,
so the next program starts clean. The property:
**measured == declared `out`**. If `Gi` consumes more than its `in` dummies, the
measurement is negative or `evaluate` recovers from a trap; either is a
**false-cert signal** (consumed-too-much).

## The generator (the safe integer sublanguage)

Programs must be **runnable** (never fault), **terminating**, and **side-effect
free**, so the generator emits a sublanguage with all-`i64` inputs and bounded
control flow. The body is built by a **stack-depth-tracked random walk** starting
at `depth = in_count`, only ever picking an op whose input arity ≤ current depth
(so the body never underflows its inputs *at generation time*). Because the walk
tracks the net depth change, the generator knows the body's **true** out-arity and
sets the declared sig to either match (intended-certify) or be perturbed by ±1
(intended-reject) — a ~70/30 mix to exercise both verdicts.

**Emitted ops:**

| group | tokens |
| --- | --- |
| literals | small ints |
| stack | `dup drop swap over nip` |
| arith/logic | `+ - * and or xor 1+ 1- negate` |
| branches (net 0) | `dup 0= if 1+ else 1- then` |
| loops (net 0) | `3 0 ?do loop` |
| return stack (net 0) | `>r r>` |
| quotations (net 0) | `[: 1+ ;] execute` |
| locals | optional top-level `{: a b c :}` binding of generated inputs |
| baits (must reject) | non-neutral `leave`, divergent `exit` |

**Excluded (non-runnable or fault-prone):** `/ mod` (div0 traps),
`@ ! c@ c!` (need valid addresses), `here , allot create does>` (data region /
not a pure function), recursion (divergence risk), `."` as a random generator op
(output side effect; it is covered by targeted parsing-word tests).

The generator is **seeded**. The default smoke uses seed `1`, count `250`; an
argv run can override both with `bin/hb <seed> <count> < test/prop-test.f`, so
any CI failure is reproducible by seed and iteration.

## Failure taxonomy (what the driver reports)

| signal | meaning |
| --- | --- |
| certified, measured `out` ≠ declared `out` | **FALSE-CERT (arity)** — the headline bug |
| certified, Pass 2 traps / underflows | **FALSE-CERT (consumes too much)** |
| rejected, but an unchecked copy runs and matches the sig | **false-reject** (incompleteness — logged, not fatal) |
| metamorphic relation says certify, checker rejects | **inconsistency** (subsumption/composition/round-trip — bounded examples logged, not fatal) |
| uncheckable (1) | ignored (sound by construction) |

Only FALSE-CERT classes fail the build. False-rejects are counted only after the
body is compiled unchecked and measured by execution; they are logged for
precision work but never gate-fail (rejecting a valid program is safe).

## Metamorphic amplifiers

Run-and-compare on a single random program is one source of truth; **metamorphic
relations** add three more, each deriving a *new* program from a checked one whose
verdict is forced by the original's. Every derived program that the checker
certifies is itself run-and-compared, so a false-cert hiding behind composition or
generalization is caught; a derived program the checker *rejects* when the relation
says it must certify is a **checker inconsistency** (logged, non-fatal — like a
false-reject, it is a precision gap, not a soundness break).

- **Subsumption** — `n` (generic int) subsumes `i64`. For every body certified at
  `( i64*in -- i64*out )`, the SAME body must certify at `( n*in -- n*out )`
  (habu's arithmetic is generic over int width; `( n -- n ) 1+` certifies). The
  generic version, if certified, is also run: same arity must hold.
- **Composition** — generate `A:(x -- y)` and `B:(y -- z)`, both certified with
  arities chained (`B`'s in-count = `A`'s out-count), then check `: C A B ;`. It
  must certify `( x -- z )` and run to arity `z`. This exercises the checker's
  *effect composition* (USIG lookup + row unification), which single-body
  execution cannot reach.
- **Render round-trip** — render the just-certified body's inferred effect back to
  text (`REND-SIG`), re-declare the SAME body with that exact rendered sig, and
  re-check. It must re-certify. This pins `render → parse → check` as a fixpoint;
  a divergence is a render/parser bug.

## Shrinking

When run-and-compare flags a FALSE-CERT, the harness **delta-debugs** the failing
body to a minimal counterexample before reporting it: drop one trailing token,
re-check that the reduced body *still certifies and still mismatches*, keep the
drop if so, restore it if not, repeat to a fixpoint. Token surgery only moves the
body-length cursor (the bytes stay put), so a rejected reduction is a single
assignment to undo. The report prints seed, iteration, expected-vs-measured (or
trap), the original `: G ( in -- out ) … ;`, and the minimized counterexample. A
sound checker never produces a real false-cert to shrink, so `SELFTEST-SHRINK` exercises
the loop on an achievable predicate ("still certifies"): it reduces
`dup drop 1+ 1- negate 1+ 1-` to `dup drop` and fails the build if it does not.

## Unbounded sweeps & the complete forget

A `: G … ;` grows THREE persistent stores: code (`CP`), the name dict (`NDICT`)
and the checker's certified-signature table (`UEND`, the `USIGS` cursor). The
per-program **forget** restores exactly those three, so the sweep reuses the same
memory every iteration and runs **unbounded** — 50 000+ programs in one process
with no growth. (Per-check transient pools reset themselves: the term arena and
the quot-effect pool `QEN` in the checker's `NEW`, the codegen scratch at `:`.)
Two checkpoint levels nest cleanly: a program-level mark, and a variant-level mark
so shrinking and the metamorphic amplifiers can define-check-discard derived words
inside a program's own checkpoint. Bump the `RUN` count for a longer sweep; there
is no dict cap to hit.

## Gate integration & running

- **Run:** `bin/hb < test/prop-test.f` (exit 0 = clean; `die`/nonzero on a
  false-cert). Defaults are seed `1`, count `250`, preserving the smoke behavior.
- **Override:** `bin/hb 123 1000 < test/prop-test.f` runs seed `123`, count
  `1000` when the stdin script has access to `ARGC`/`ARGV`.
- **Smoke (in `test/run.f`):** 250 programs, sub-second, in-process; fails the
  gate on any FALSE-CERT. `SELFTEST` sanity-checks the comparison predicate,
  `SELFTEST-SHRINK` exercises shrinking, and `BAITS` asserts programs a sound
  checker must reject stay rejected.
- **Sweep:** pass a larger count on argv — the complete forget reuses
  CP/NDICT/USIGS every iteration, so a single process runs unbounded (50 000+
  programs verified, no growth).
- **Regression:** freeze any counterexample as a `BAIT` in `prop-test.f` (a
  program that must not certify) so it can never silently return. Current baits
  cover non-neutral `leave`, divergent `exit`, bool-as-`i64`, malformed
  signatures, and malformed quotation signatures.

## What the generator emits

- The **linear integer sublanguage**: literals, `dup drop swap over nip`,
  `+ - * and or xor 1+ 1- negate` — the arity-soundness core.
- Net-0 **structural ops**: bounded `?do/loop`, balanced `if/else/then`, balanced
  `>r/r>`, and `[: 1+ ;] execute` — so a regression in their arity modelling
  shows up as a certified-but-wrong measurement.
- Optional **locals**: generated inputs may be bound with `{: a b c :}` and then
  referenced as ordinary value producers, exercising local-scope stack effects
  without making the body fault-prone.
- **Leave/exit baits**: non-neutral `leave` and divergent `exit` (where this
  session's false-certs lived) that a sound checker rejects; certifying one is a
  `die`.

## File plan

- `test/prop-test.f` — the whole self-hosted harness: PRNG + generator + driver +
  measurement, run by `bin/hb`, in-process via `evaluate`.
- `test/run.f` — smoke invocation.
