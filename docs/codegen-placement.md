# Where a routine lands, and what it costs

Measured by `tools/codegen-align-sweep.f` (hand-run, unscheduled):

```sh
bin/hb --load tools/codegen-align-sweep.f
```

Host: Apple M2 Max, `hw.cachelinesize` 128, `hw.pagesize` 16384, `hw.l1icachesize`
131072. The machine was **not** quiet during these runs (an unrelated build was
holding one core), which is why per-run spreads are large and why every number
below is a fastest-of-31 over interleaved rounds. Two full runs are quoted side
by side throughout; where they disagree, believe neither number more than the
pair.

## The question

`tools/codegen-workload.f` reported two byte-identical engine publications of one
144-byte body, reached by two byte-identical drivers, 18 to 35 per cent apart on
a workload that calls the body once per byte — reproducibly, tracking the
*callee's* publication. That is an order of magnitude more than either code
generator does to that body, so the workload could not report a code-generation
result on that shape at all. Dot `habu-explain-the-20-0dac3ebd` asked for the
mechanism and for a decision about aligning a routine's entry in the publication
seam.

## What the sweep does

One arm is one package holding its own publication of the workload's own four
subject strings and its own driver compiled from the workload's own `SCAN-BODY$`
text. The driver names the subject bare, and a bare tail resolves in the open
package first, so every arm's driver is the same characters and reaches its own
copy. A placement is chosen by publishing real filler definitions (20-byte and
24-byte records) in front of the subject until the engine's own code pointer sits
at the wanted address; nothing pokes engine state, and the seam refuses if the
pointer did not land where the arithmetic said.

Two steers per arm, not one. The subject is placed, then the pointer is steered
*again* before the driver is compiled. Without the second steer the driver would
sit a fixed distance behind the subject and the call site would carry the
subject's residue with it — the two variables would move together and no curve
could tell them apart.

170 arms in five phases plus a baseline, all timed in one interleaved pass:

| phase | swept | held still |
| --- | --- | --- |
| 1 | engine callee entry, 32 four-byte positions, twice each | call site |
| 2 | call site, 32 positions | callee entry |
| 3 | chain callee entry, 32 positions | call site |
| 4 | page offset, 8 positions incl. one whose body straddles a page | line residue |
| 5 | **branchless** callee of the same 144 bytes, 32 positions | call site |
| baseline | — | the same loop with the call removed |

Everything the timings would be meaningless without is checked and throws: each
arm landed where it was steered; every arm's subject is byte-identical machine
code to its publisher's reference; every arm's driver is byte-identical except
the one branch displacement that differs by construction; each driver holds
exactly one call and it enters that arm's own copy; every arm running one program
reached the same answer; and the branchless stand-in really has no branch (the
copy rule's own count of unmovable instructions is 0 for it and 4 for the engine
fold) at exactly the fold's 144 bytes. The chain fold's own count was 6 when the
tables below were taken and is 0 now — see *What the chain does with this body
today* at the end — and the sweep refuses if it is anything else.

`ps/call` is the arm's fastest run **minus the baseline's**, divided by the
204800 calls a run makes. The subtraction matters: the driver pays a loop, a byte
load and an add per byte as well as the call, and dividing the whole time by the
byte count would report all four as the call.

## The numbers

Per-residue minimum `ps/call`, two runs.

### Phase 1 — engine-published fold, callee entry swept

| entry mod 64 | 0 | 4 | 8 | 12 | 16 | 20 | 24 | 28 | 32 | 36 | 40 | 44 | 48 | 52 | 56 | 60 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| run A | 2341 | 2441 | 2108 | 2450 | 2743 | **4208** | **4302** | **4234** | **4300** | **4340** | 2631 | 2648 | 2543 | 2842 | 2531 | 2409 |
| run B | 2233 | 2449 | 2403 | 2428 | 2802 | **4210** | **4168** | **4134** | **4199** | **4208** | 2566 | 2689 | 2679 | 2500 | 2419 | 2375 |

Four arms per residue (two replicates × two 128-byte halves). Best 2.11 ns,
worst 4.34 ns per call: **2.06x** in run A, 1.89x in run B. The band is a
contiguous window of entry offsets, it repeats with period **64**, and both
128-byte halves behave identically — so the period is 64, not the host's 128-byte
line.

The replicate floor — the widest disagreement between arms steered to the *same*
residue at different absolute addresses — is 76 to 90 parts per thousand. The
band is 700 to 800 parts per thousand. The residue explains the effect; the
address does not.

### Phase 2 — call site swept, callee entry pinned

| site mod 64 | 0 | 4 | 8 | 12 | 16 | 20 | 24 | 28 | 32 | 36 | 40 | 44 | 48 | 52 | 56 | 60 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| run A | 2587 | 2517 | 2717 | 2566 | 2659 | 2357 | 2564 | 2284 | 2447 | 2555 | 2877 | 2545 | 2407 | 2568 | 2607 | 2628 |
| run B | 2438 | 2139 | 2715 | 2501 | 2567 | 2456 | 2632 | 2364 | 2499 | 2383 | 2717 | 2472 | 2461 | 2456 | 2554 | 2405 |

Span 1.26x and 1.27x, no band, no repeat. Moving the whole driver — its entry,
its loop branch and the `bl` itself — across every position in the period does
nothing the noise does not already do.

### Phase 3 — chain-published fold, callee entry swept

| entry mod 64 | 0 | 4 | 8 | 12 | 16 | 20 | 24 | 28 | 32 | 36 | 40 | 44 | 48 | 52 | 56 | 60 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| run A | 5004 | 4911 | 4633 | 4936 | 5169 | 4915 | **3503** | **2988** | **2974** | **3155** | **3227** | **2704** | 4985 | 5045 | 5109 | 5132 |
| run B | 4605 | 4958 | 5027 | 4848 | 4942 | 4915 | **3232** | **2955** | **2947** | **3140** | **3341** | **2855** | 4797 | 5112 | 4590 | 4840 |

Bold is the *fast* window. Same period of 64, a band of the same size — and in
very nearly the complementary position. The chain's good placements are inside
the engine's bad ones.

### Phase 4 — page offset, line residue pinned

Eight offsets in one 16 KiB page (0, 128, 256, 4096, 8192, 12288, 16128, 16256;
the last one's 144-byte body crosses the page boundary): span 95 and 75 parts per
thousand, which is the replicate floor. Page position, page crossing included,
does nothing.

A third run taken while the host's load average had climbed to 5.8 put that span
at 286 — and is worth quoting rather than dropping, because of *where* it came
from. The two inflated arms sat at page offsets 0 and 12288, neither of them a
boundary, while the arm whose body really crosses the page came out mid-pack at
2685 ps/call. A phase with only eight arms has a max-minus-min that one noisy arm
can move; a page effect would have moved the crossing arm, and did not.

### Phase 5 — branchless callee, same 144 bytes, entry swept

Span **28** parts per thousand in run B, 95 in run A — at or under the replicate
floor, with no band anywhere. The stand-in is published under the same name, is
reached by the same driver text, and the engine compiles it to a record of
exactly the fold's 144 bytes, so it occupies exactly the same instruction-fetch
footprint at every entry offset the sweep visits. The only thing it does not have
is a branch.

## (a) The mechanism

**It is keyed to the addresses of the callee's own branch instructions, and to
nothing else that was swept.** Named candidates, and what the sweep did to them:

- **64-byte line straddling** — the host's line is 128 bytes, so this reading of
  the question is wrong on its own terms. The effect's period *is* 64, but no
  instruction ever straddles anything: every instruction is 4 bytes and 4-byte
  aligned, and both 128-byte halves of the period behave identically.
- **128-byte pair / fetch window** — refuted. If the period were the line the two
  halves of phase 1 would differ; they do not.
- **Page crossing** — refuted by phase 4, including the arm whose body really does
  cross a page boundary: 7.5 to 9.5 per cent, which is the floor.
- **Branch-target alignment of the call site** — refuted by phase 2: 26 per cent
  across the whole period, no structure, while phase 1 shows 100 per cent with a
  clean band at the same time on the same host in the same interleaved pass.
- **Any front-end effect — fetch blocks, decode windows, instruction TLB** —
  refuted by phase 5. Same 144 bytes, same footprint at every offset, same
  driver, same name: the band disappears into the floor when the branches are
  taken out. A mechanism that depends on where the bytes are and not on what they
  do cannot behave that way.

What survives is the callee's branches. Phase 2 narrows it further: the driver's
own loop branch and call site move across the whole period without effect, so it
is not the callee's branches aliasing the *caller's*. It is an interaction among
the callee's own branch instruction addresses, with a period of 64 bytes — that
is, address-indexed branch-prediction state, where one contiguous window of entry
offsets puts the routine's own branches into colliding entries.

The bodies agree with that reading. The engine's fold is 36 instructions with 4
branch instructions in 148 bytes, one per 37 bytes, and its bad window is 5 of 16
positions. The chain's fold is 18 instructions with 6 branch instructions in 76
bytes, one per 13 bytes — three times denser — and its bad window is 10 of 16.
The branchless stand-in has none and no window at all.

Naming the exact predictor structure inside an Apple M2 is beyond what this
measurement can do: the host exposes no counter for mispredictions here. What the
sweep does establish, and what the decision below needs, is that the variable is
the callee entry address modulo 64, that its effect exists only for a callee with
data-dependent branches, and that the *sign of a given offset is a property of
the body*, not of the offset.

## (b) Is the chain's leaf really slower?

Yes — about a quarter slower, and that survives holding the placement still.

| | run A | run B |
| --- | ---: | ---: |
| engine fold, best placement | 2108 ps/call | 2233 ps/call |
| chain fold, best placement | 2704 ps/call | 2855 ps/call |
| chain penalty at best-vs-best | **+28 %** | **+28 %** |

At a placement drawn at random the gap is much wider — the median of the
per-residue minima is about 2.6 ns for the engine and about 4.8 ns for the chain,
so roughly 1.8x — but that comparison mixes the two effects and is the number the
workload could not interpret. The honest figure is each body at its own best
placement, and there the chain loses 28 per cent.

The cause is visible in the instruction streams (both read through
`tools/codegen-workload-scan.f`'s walk). For `c 65 < if c exit then c 90 > if c
exit then c 32 or`:

- The engine emits one `cmp` + `cset` + `neg` + **`cbz`** per test, and one
  unconditional `b` to the common epilogue per early exit: 4 branch instructions
  in 148 bytes.
- The chain emits `cmp` + **`b.cond` to the then-arm** + **`b` to the else-arm**
  per test, and a `b` to the join per arm: 6 branch instructions in 76 bytes.

So the chain halves the instruction count and *increases* the branch count by
half, packing three times as many branches per byte. That is exactly the input
the mechanism above is sensitive to, and it is why the chain's bad window covers
ten of sixteen entry offsets where the engine's covers five. The chain's smaller
code is genuinely smaller; on a call-per-byte shape with unpredictable data it is
not faster, and the reason is the branch-around idiom, not the size.

### The cross-tool disagreement is not a measurement error

The dot recorded `tools/codegen-compare.f` saying 0.82 ns/call for the chain's
fold where the workload's per-call derivation said 3.13 — 3.8x apart. Both are
right about different things. `tools/codegen-compare-cases2.f` times the fold as
`[: EXACTLY-A CODEGEN-CORPUS2:SYM-FOLD-C drop ;]` — the same **constant** byte on
every iteration, so every branch in the body goes the same way every time and
never mispredicts. The workload feeds it a pseudo-random printable byte per call,
so both tests are near coin-flips.

The scale is right for that reading: the branchless 144-byte stand-in measured
here costs 0.92 to 1.16 ns per call in the same driver, and
`tools/codegen-compare.f`'s loop is tighter still (no byte load, no buffer
index). A perfectly predicted call to this body belongs around 1 ns; a
data-driven one costs 2.1 to 5.5 ns depending on where it landed. The two numbers
never described the same workload.

## (c) Should the publication seam align a routine's entry?

**No.**

The dot's premise was that an aligner "would raise every hot leaf call in the
system by the same 20 per cent". The measurement says the effect is not an
alignment penalty that alignment removes. It is a per-body window, and a fixed
aligner picks one residue for every routine in the system:

Picoseconds per call, run A / run B:

| | engine fold | chain fold |
| --- | ---: | ---: |
| cost at residue 0, where a 64-byte aligner puts everything | 2341 / 2233 | 5004 / 4605 |
| mean over the whole period | 3067 / 3029 | 4274 / 4194 |
| best placement | 2108 / 2233 | 2704 / 2855 |
| aligner against a placement drawn at random | **−24 % / −26 %** | **+17 % / +10 %** |

One body gains a quarter, the other loses a tenth to a sixth, from the same rule,
on the same host, in the same run. The expected value over the two bodies is a few per
cent with a sign nobody can predict from the source. Multiplying that by however
many hot leaves the system has does not help: the leaf count multiplies a number
whose sign is decided per body. A count is only worth taking once the per-leaf
expectation is positive, and it is not.

Three further reasons the arithmetic does not turn around:

1. **It only applies to leaves with data-dependent branches.** Phase 5 is flat, so
   a branchless leaf gains nothing. A leaf small enough to be branchless is also
   usually small enough that the engine copies it into its caller and there is no
   call to speed up at all.
2. **The ceiling is the call, not the program.** Even the perfect per-body choice
   is worth at most about 2.2 ns per call here, on a workload deliberately shaped
   as one call per byte. A program that is not one call in a loop sees that
   fraction of a fraction.
3. **Padding is not free.** A 64-byte aligner spends up to 60 bytes of the code
   arena per published routine, which is a real cost against a real reserve, paid
   for an expected gain of approximately nothing.

What the measurement *does* recommend is attacking the cause instead of the
symptom: the chain's branch-around idiom is what makes its leaf both slower at
matched placement and more sensitive to placement. Turning `cmp / b.cond then / b
else` into a `csel`-shaped selection where the arms are small removes branches
rather than moving them, which is the only change here that improves every
placement at once. That was filed as its own dot and has landed; the section
below is what it did to these numbers.

The `tools/codegen-compare.f` constant-input measurement is filed separately: it
is not wrong, but its per-call figures for branchy corpus words are a
perfect-prediction number and the committed tables do not say so.

## What the chain does with this body today

`src/compiler/native/select.f` now if-converts a selection whose arms are single
values into a machine select, so the chain's copy of this fold has **no branch at
all**: 48 bytes where it used to be 76, and 0 branch instructions where it used to
be 6. The engine's copy is untouched — 36 instructions, 148 bytes, 4 branches — so
phase 1 is the same measurement it always was and phase 3 is a measurement of a
different body.

Two fresh runs of the same sweep, on the same host, read the same way as the
tables above:

| | run A | run B |
| --- | ---: | ---: |
| engine fold, best placement | 2082 ps/call | 1705 ps/call |
| chain fold, best placement | **698 ps/call** | **611 ps/call** |
| chain against engine, best vs best | **−66 %** | **−64 %** |
| engine fold, mean over the period | 3004 | 2789 |
| chain fold, mean over the period | 794 | 689 |
| engine fold, span across the period | 2.05x | 2.47x |
| chain fold, span across the period | **1.44x** | **1.48x** |
| branchless 144-byte stand-in, span | 1.34x | 1.38x |

Both halves of part (b) turn over. The chain leaf was 28 per cent slower than the
engine's at matched best placement and is now about a third of it, and that is not
a placement result: the branch is gone, so the same body runs without the
mispredictions the pseudo-random byte stream used to buy. The placement
sensitivity went with it — the chain's span across the whole period has fallen
from roughly 1.9x to 1.44x, which is the branchless stand-in's own span in the
same run. Phase 3 no longer has a band anywhere; what is left is the harness
floor, and where the routine lands no longer decides what it costs.

Part (c) is unchanged, and this result makes the case against an aligner stronger
rather than weaker. The effect exists only for a callee with data-dependent
branches; the answer to a leaf that has them is to take them out, which improves
every placement at once, and not to choose a residue for it, which improves one
placement and worsens another.
