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

The whole harness is **checked-Forth's untyped tooling tier, run by `bin/habu`**
(`test/prop-test.f`): the PRNG, the generator, the driver and the measurement are
all habu. Each program is defined, checked and run **in the same process** via
the engine's re-entrant `evaluate` — no Python, no gforth, no per-program
spawning.

```
for each program (seeded PRNG):
  GEN          build ": G ( i64*in -- i64*out ) <body> ;" in a buffer
  ['] VH set-check   evaluate <def>     \ VH = `CHECK! dup VERD !` → VERD = verdict
  0 set-check
  VERD = -1 ?  →  evaluate "MK <in×7> G NAB"   \ run G, NAB = measured out-arity
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
| arith/logic | `+ - and or 1+ 1- negate` |
| branches (net 0) | `dup 0= if 1+ else 1- then` |
| loops (net 0) | `3 0 ?do loop` |
| return stack (net 0) | `>r r>` |
| baits (must reject) | non-neutral `leave`, divergent `exit` |

**Excluded (non-runnable or fault-prone):** `/ mod` (div0 traps),
`@ ! c@ c!` (need valid addresses), `here , allot create does>` (data region /
not a pure function), recursion (divergence risk), `."` (absent in habu).

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

## Gate integration & running

- **Run:** `bin/habu < test/prop-test.f` (exit 0 = clean; `die`/nonzero on a
  false-cert). Fixed seed in the script = reproducible.
- **Smoke (in `test/run.sh`):** 250 programs, sub-second, in-process; fails the
  gate on any FALSE-CERT. A `SELFTEST` first proves the arity comparison fires
  (a sound checker won't hand us a real false-cert to test against), and `BAITS`
  asserts the leave/exit programs that a sound checker must reject stay rejected.
- **Sweep:** bump the `1 250 RUN` count in the script (the dict grows by one per
  certified def, so a single process is bounded by the ~1600-word dict cap).
- **Regression:** freeze any counterexample as a `BAIT` in `prop-test.f` (a
  program that must not certify) so it can never silently return.

## What the generator emits

- The **linear integer sublanguage**: literals, `dup drop swap over nip`,
  `+ - * and or xor 1+ 1- negate` — the arity-soundness core.
- Net-0 **structural ops**: bounded `?do/loop`, balanced `if/else/then`, balanced
  `>r/r>` — so a regression in their arity modelling shows up as a
  certified-but-wrong measurement.
- **Leave/exit baits**: non-neutral `leave` and divergent `exit` (where this
  session's false-certs lived) that a sound checker rejects; certifying one is a
  `die`.

## File plan

- `test/prop-test.f` — the whole self-hosted harness: PRNG + generator + driver +
  measurement, run by `bin/habu`, in-process via `evaluate`.
- `test/run.sh` — smoke invocation.
