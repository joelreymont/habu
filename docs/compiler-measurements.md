# Does the optimizing tier pay for itself?

Measured 2026-09-16 on Omarchy/Asahi aarch64 (M2 Max, 12 cores: cpu0-3 at
capacity 561, cpu4-11 at 1024), against the release engine
`/tmp/hazel-release/hb`, 5,832,896 bytes, sha256 `7e490c6031cc317c…`. Every
number below was taken with that engine; the tools are
`tools/tier-census.f`, `tools/tier-census-join.f`, `tools/tier-dump.f` and
`tools/tier-bench.f`, plus the existing `tools/compile-floor.f`.

**Answer: yes, decisively, at run time — and the bloat is real but small and
has one cause.** On the same 1,772 words, tier 1 halves the call count and runs
1.1x to 9.3x faster, while emitting 3.9 percent more bytes and costing 24x the
compile time. Every byte of the growth, and more, is inline relocatable-address
stencils on cold guard paths.

## Method

Tier 0 is the direct JIT (`src/habu/habu2.f`); tier 1 is the IR compiler
(`src/compiler/ir`, `src/compiler/native`), selected with `1 set-tier`. The two
tiers are compared by loading identical source at each tier in a separate
process — the tier is engine-global and a file loads once — and reading each
word's baked span out of the running dictionary with `XREF-START` /
`XREF-CODE-BYTES`.

Only code compiled after `set-tier` belongs to the selected tier. The engine's
baked words — the checker, the compiler itself, `lib/string.f`, the whole cold
prefix — were compiled when the engine was built and do not change, so nothing
already in the engine can appear in either column. That is why the corpus below
is library and tool source rather than `src/core`, and why every benchmark word
comes from a file loaded after the selection.

Corpus (13 entries, 1,772 words after their own `require` closure):
`lib/byte-edit.f`, `lib/array.f`, `lib/fmt.f`, `lib/float.f`,
`lib/json-read.f`, `lib/json-write.f`, `lib/unicode.f`, `lib/argv.f`,
`lib/fs.f`, `lib/task.f`, `lib/build.f`, `tools/lint/text.f`,
`tools/public-signatures-core.f`.

Timings are wall clock on a machine that had other lanes building throughout;
the load average is quoted with every one, and every timing run is pinned to a
performance core (`taskset -c 8`), because an unpinned run that lands on cpu0-3
reads 1.6x slow and that difference is larger than several of the results.

## 1. Code size per word

Corpus totals, `tools/tier-census-join.f`:

| metric | tier 0 | tier 1 | delta |
|---|---:|---:|---:|
| words | 1772 | 1772 | 0 |
| bytes | 169,600 | 176,276 | +6,676 (+3.9%) |
| instructions | 42,400 | 44,069 | +1,669 |
| `bl` (calls) | 9,250 | 4,135 | −5,115 (−55.2%) |
| `ldr` via sp | 4,027 | 3,922 | −105 (−2.6%) |
| `str` via sp | 2,456 | 2,402 | −54 (−2.1%) |
| `mov` reg→reg | 39 | 126 | +87 |
| `movk` | 805 | 5,366 | **+4,561 (+566%)** |

A representative sample, `b`=bytes, `sp`=frame loads+stores, `mvk`=`movk`:

| word | b0 | b1 | db | bl0 | bl1 | sp0 | sp1 | mvk0 | mvk1 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `EDIT:E-STORAGE` | 16 | 16 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| `ARRAY:EVEN?` | 40 | 28 | −12 | 2 | 0 | 2 | 0 | 0 | 0 |
| `ARRAY:A-LEN` | 60 | 96 | +36 | 4 | 2 | 2 | 2 | 0 | 3 |
| `SOURCE-QPATH-CHECK` | 48 | 112 | +64 | 4 | 3 | 2 | 2 | 0 | 3 |
| `FIT-I64` | 56 | 120 | +64 | 6 | 2 | 2 | 2 | 0 | 3 |
| `FILE-SIZE` | 92 | 204 | +112 | 12 | 7 | 2 | 2 | 0 | 6 |
| `FS-CHECK-RECORD` | 140 | 340 | +200 | 18 | 12 | 2 | 2 | 0 | 12 |
| `ARRAY:A-SUM` | 280 | 144 | −136 | 5 | 1 | 7 | 12 | 0 | 0 |
| `ARRAY:A-MIN` | 284 | 196 | −88 | 6 | 2 | 8 | 21 | 0 | 0 |
| `ARRAY:A-PREFIX-SUM!` | 428 | 220 | −208 | 12 | 2 | 10 | 25 | 0 | 0 |
| `ARRAY:A-RUNMAX!` | 428 | 236 | −192 | 12 | 3 | 10 | 25 | 0 | 0 |
| `DECODE-LEAD` | 532 | 208 | −324 | 20 | 6 | 29 | 2 | 0 | 0 |
| `ESC-BYTE` | 540 | 308 | −232 | 34 | 10 | 20 | 2 | 0 | 3 |
| `JW-ESC-C` | 544 | 364 | −180 | 41 | 16 | 13 | 2 | 0 | 0 |
| `PS-JSON-ESC-C` | 536 | 364 | −172 | 39 | 16 | 13 | 2 | 0 | 0 |
| `LINT-ORDER:CMP-CI` | 444 | 260 | −184 | 16 | 2 | 18 | 17 | 0 | 0 |
| `LINT-FIND-SUB` | 272 | 220 | −52 | 8 | 4 | 13 | 16 | 0 | 0 |
| `SD-LOCAL-NAME-LEN` | 336 | 168 | −168 | 3 | 1 | 7 | 21 | 0 | 0 |
| `READ-VALUE` | 504 | 280 | −224 | 25 | 11 | 20 | 14 | 0 | 3 |
| `WRITE-EDITS` | 476 | 324 | −152 | 15 | 4 | 19 | 38 | 0 | 0 |
| `STEP` | 668 | 508 | −160 | 56 | 12 | 26 | 16 | 0 | 6 |
| `SCAN-SURROGATE` | 380 | 580 | +200 | 30 | 19 | 15 | 9 | 0 | 18 |
| `LINT-SPLIT:SPLIT-LINES` | 368 | 488 | +120 | 42 | 7 | 10 | 30 | 0 | 24 |
| `SD-SCAN-STRING` | 272 | 476 | +204 | 30 | 11 | 4 | 15 | 0 | 24 |
| `PS-LEX-COMMENT` | 276 | 488 | +212 | 53 | 14 | 2 | 2 | 0 | 33 |
| `JR:INIT` | 336 | 532 | +196 | 19 | 14 | 15 | 14 | 0 | 15 |
| `FS-WRITE-BY-FLAGS` | 656 | 1160 | +504 | 66 | 25 | 21 | 44 | 0 | 60 |
| `READ-ALL` | 684 | 1272 | +588 | 89 | 27 | 13 | 19 | 0 | 93 |

The sample is not cherry-picked for the pattern, but the pattern is exact:
**every word that grew has a large `mvk1` and `mvk0` of zero; every word that
shrank has `mvk1` of zero or near it.** `docs/compiler-ir-design.md` pins each
relocatable address to a fixed four-instruction `MOVZ`/`MOVK` stencil so a later
pass can recognize it, so one inline address costs 16 bytes and `movk` over
three counts them. Corpus-wide that is 1,789 stencils at tier 1 against 268 at
tier 0: **24,336 bytes of extra address materialization against a net growth of
6,676**. Tier 1's folding and inlining are saving about 17,700 bytes elsewhere
and spending 24,300 putting addresses inline.

### Three words side by side

`ARRAY:A-LEN`, whose whole source is `dup 0 < if E-A-BOUNDS throw then >LEN ;`
— 60 bytes at tier 0, 96 at tier 1:

```
tier 0 (60 B, 15 instructions)          tier 1 (96 B, 24 instructions)
  sub  sp, sp, #0x10                      sub  sp, sp, #0x10
  str  x30, [sp]                          str  x30, [sp]
  bl   <guard>                            ldur x0, [x19, #-8]
  mov  x16, #0x0                          cmp  x0, #0x0
  str  x16, [x19]                         b.lt 0x18
  add  x19, x19, #0x8                     b    0x54          <- hot path ends
  bl   <check>                          0x18: mov  x0, #-2001   <- cold throw
  sub  x19, x19, #0x8                     str  x0, [x19]         path, inline
  ldr  x9, [x19]                          add  x19, x19, #0x8
  cbz  x9, 0x30                           bl   <throw>
  bl   <throw ctx>                        mov  x0, #0x3a98     \
  bl   <throw>                            movk x0, #0x4080,16   | one address,
0x30:                                     movk x0, #0x3, 32     | four
  ldr  x30, [sp]                          movk x0, #0x0, 48    /  instructions
  add  sp, sp, #0x10                      mov  x1, #0x13
  ret                                     mov  x2, #0x58
                                          stur x0, [x19, #-8]
                                          str  x1, [x19]
                                          str  x2, [x19, #8]
                                          add  x19, x19, #0x10
                                          bl   <diagnose>
                                        0x54:
                                          ldr  x30, [sp]
                                          add  sp, sp, #0x10
                                          ret
```

Tier 1's hot path is nine instructions and no call; tier 0's is thirteen with
two calls. Tier 1 is unambiguously the better code to *run*. It is larger only
because fifteen instructions of never-taken diagnostic sit between the test and
the epilogue, forcing an extra unconditional branch on the hot path as well.

`SOURCE-QPATH-CHECK` (`SOURCE-PATH-SAFE? 0= if E-FS-PATH-UNSAFE throw then ;`)
is the same shape at 48 → 112 bytes, with 60 of the 112 on the cold path, and it
also shows a second, smaller defect: tier 1 opens with

```
  sub  x19, x19, #0x10
  add  x19, x19, #0x10
```

a data-stack adjustment undone immediately, with nothing between — the
data-stack twin of the frame-slot identity copy `regalloc.f MB-IDENTITY-COPY?`
already drops.

`UTF8:DECODE-LEAD` is the win, 532 → 208 bytes, frame traffic 29 → 2. Tier 0
spills all four locals to the frame and calls out for every comparison:

```
tier 0                                  tier 1
  sub  sp, sp, #0x20                      sub  x19, x19, #0x8
  sub  x19, x19, #0x8                     ldur x0, [x19, #-8]
  ldr  x9, [x19]                          ldr  x1, [x19]
  str  x9, [sp]                           cmp  x1, #0x80
  sub  x19, x19, #0x8                     b.lt 0x60
  ldr  x9, [x19]                          mov  x2, x0
  str  x9, [sp, #8]                       mov  x3, x1
  ... two more locals spilled ...         cmp  x3, #0xc0
  ldr  x9, [sp]                           b.lt 0x78
  str  x9, [x19]                          mov  x0, x2
  add  x19, x19, #0x8                     mov  x1, x3
  bl   <ASCII-LIMIT>                      cmp  x1, #0xe0
  bl   <less-than>                        b.lt 0x8c
  ...                                     ...
```

Everything stays in registers and the constants fold into the compare
immediates. The four `mov` shuffles visible there are a residue the coalescer
missed, but the census falsifies them as a priority: 126 such moves in the whole
corpus, about 350 bytes.

## 2. Run time

`tools/tier-bench.f`, six benchmarks over code compiled at the selected tier,
plus `harness` (the timing loop itself over an empty body, so the driver's own
tier-dependent cost is visible). Three timed runs inside the tool, three
invocations per tier; the figure is the median of the three invocation medians,
microseconds. Pinned to cpu8; load average 4.99 before, 5.37 after.

| benchmark | what it runs | tier 0 | tier 1 | tier 0 / tier 1 |
|---|---|---:|---:|---:|
| `harness` | 1,000,000 empty loop iterations | 5,540 | 4,928 | 1.12x |
| `arith` | `ARRAY:A-SUM` over 2.1M cells | 8,034 | **862** | **9.32x** |
| `branch` | `ARRAY:A-COUNT-EVEN`, same data | 11,722 | 7,764 | 1.51x |
| `search` | `LINT-CONTAINS?`, absent needle over `src/core/checker.f` | 9,928 | **1,863** | **5.33x** |
| `fold` | `FOLD-TO` case-folding that text 32x | 348,307 | 125,708 | 2.77x |
| `move` | `LINT-BMOVE` of that text 64x | 372,022 | 225,196 | 1.65x |
| `lines` | `LINT-SPLIT:SPLIT-LINES` over it 32x, a source tokenizer | 22,181 | 9,321 | 2.38x |

Tier 1 wins every benchmark. The spread is the point: work whose inner loop is
arithmetic on typed values gains almost an order of magnitude, because tier 0
calls a primitive per token and tier 1 folds them; work dominated by one
byte-at-a-time loop with a call already amortized gains 1.6x-2.8x.

## 3. Compile time, and what the self-build costs

Compiling the same 1,772 words, pinned to cpu8, three runs each, load 9.7-10.1:

| | tier 0 | tier 1 | ratio |
|---|---:|---:|---:|
| whole corpus | 0.187 s | 4.454 s | **23.8x** |
| per word | 0.105 ms | 2.51 ms | |
| `compile-floor` trivial definition | 28 µs | 723 µs | 25.8x |
| `compile-floor` three-op definition | — | 489 µs | |

**The self-build cannot be run at tier 0, and the tier is not the tool's to
choose.** `EXECUTABLE-BUILD:WITH` opens a build scope, and `src/habu/habu1.f`
`BBUILDENTER` saves the caller's tier in `NCOMP-DISPATCH:BUILD-TIER-CELL` and
stores 1 into `TIER-CELL`; `EXECUTABLE-JIT-REFUSE` then refuses any JIT compile
inside that scope with `hb: executable build requires native tier 1`, exit 70.
Measured: editing `tools/native-build.f` line 2 from `1 set-tier` to
`0 set-tier` and rebuilding through
`bin/hb --load tools/build-fixpoint-refresh.f -- install` produced a
**byte-identical engine** (sha256 `52988371a1e8f044…` both times) in 41.58 s
against 39.65 s for the unmodified tree, at load 8.3. The `set-tier` line at the
head of a build tool documents the intent; it does not select anything the build
scope has not already forced.

What tier 1 costs the self-build can therefore only be computed, not switched
off. The build's own census reports 5,374 certified words — 3,566 in the boot
prefix, 1,808 assembled — and the prefix already compiles at tier 0. At the
measured rates the 1,808 assembled words cost about 4.5 s of the 39.65 s build
at tier 1 and would cost about 0.19 s at tier 0; moving the 3,566-word prefix to
tier 1 would add about 8.6 s (+22 percent of the build) and make the prefix's
code faster in exactly the proportions section 2 measures. Compilation is
roughly 12 percent of this build either way: the wall clock is dominated by the
stage chain, the capture and the checking, not by which compiler runs.

## 4. The pre-IR compiler

**Tier 0 *is* the pre-IR compiler.** There is no deleted "old native compiler"
to resurrect: `src/compiler/{ir,native}` was added alongside `src/habu/habu2.f`,
never over it, and `docs/compiler-ir-design.md` §2.1 describes `habu2.f` as the
single tightly coupled path it replaced. Every tier-0 column above is therefore
the pre-IR baseline, measured today, on today's engine.

The historical tree is a different question and the answer is no. The last
mainline revision with no `src/compiler/` at all is `8af13d1ee4fd` (2026-08-05);
`set-tier` itself arrived at `3e024d582ba5` (2026-09-11). `8af13d1e` does not
build on this machine, for two independent reasons:

- Its own `tools/bootstrap.sh` emits the stage0 seed without the boot-hide
  prologue (line 195 at that revision). The very next commit batch,
  `faa7ab312da5`, made it unconditional and records what it fixed: the no-binary
  recovery died at `src/habu/xref.f` INSTALL with
  `hook: non-certified definition: install at 'is'`, exit 70. That seed's source
  list is also missing `src/core/bytes.f`, a boot-prefix row, fixed 17 days later
  by `63c6e86773e6`.
- Building it with a current engine is refused by a checker rule that postdates
  it. `src/core/bytes.f:9` at that revision is
  `: BYTE-VIEW ( ptr a -- ptr u8 ) ;`, and today's engine answers
  `E-NONPARAMETRIC-EFFECT … declared type variable 'a' is specialized to a
  concrete type`, exit 70 (measured). Separately, the `cast:` reader keyword
  landed `8818e2cb6fb7` (2026-08-19), and `LESSONS.md` records that no engine can
  read across that seam in either direction.

It would become buildable only by backporting four later commits, at which point
it is no longer the pinned revision.

## Verdict and the ranked fixes

Tier 1 pays for itself. It halves the calls, wins every run-time benchmark by
1.1x to 9.3x, and costs 3.9 percent more bytes and 24x the compile time. The
compile-time multiplier is the real price, and the tree already pays it only
where it matters — the boot prefix compiles at tier 0 and executables are forced
to tier 1. The bloat Joel sees is real, has one dominant cause, and is worth
fixing on its own terms, because the same defect that adds the bytes also puts
cold code between a hot test and its epilogue.

1. **Outline the cold throw path** (`src/compiler/native/elaborate.f`,
   `emit.f`). A guarded word lays its whole diagnostic tail inline: 15 of
   `ARRAY:A-LEN`'s 24 instructions, 60 of `SOURCE-QPATH-CHECK`'s 112 bytes. It
   is also what puts the address stencils there — corpus-wide 1,521 extra
   four-instruction stencils, 24,336 bytes, against a total regression of 6,676.
   Moving those blocks past the epilogue removes the growth, removes the extra
   unconditional branch on the hot path, and shortens every guarded word in the
   tree.
2. **Replace the four-instruction `MOVZ`/`MOVK` address stencil.** 5,366 `movk`
   at tier 1 against 805 at tier 0 is 21,464 bytes, 12 percent of all tier-1
   code, to materialize pointers a literal pool or an `ADRP`/`ADD` pair would
   carry in 8. The stencil exists so a later pass can recognize and rewrite the
   address (`docs/compiler-ir-design.md`), so this is a real format change and
   the AOT capture path has to agree — which is why it ranks second even though
   it is the larger single number.
3. **Extend the identity-copy elision to the data-stack pointer.**
   `regalloc.f MB-IDENTITY-COPY?` drops a frame-slot copy onto itself; the same
   shape survives on `x19`, where `SOURCE-QPATH-CHECK` opens
   `sub x19,#0x10` / `add x19,#0x10` with nothing between. Small — a few hundred
   bytes corpus-wide — but it is the identical defect in a second place and the
   fix is understood.

Not worth doing: register-to-register move elimination. `UTF8:DECODE-LEAD` makes
it look like a pattern, but the census counts 126 such moves in 1,772 words,
about 350 bytes.

## Reproducing

```sh
E=/tmp/hazel-release/hb
C="lib/byte-edit.f lib/array.f lib/fmt.f lib/float.f lib/json-read.f \
lib/json-write.f lib/unicode.f lib/argv.f lib/fs.f lib/task.f lib/build.f \
tools/lint/text.f tools/public-signatures-core.f"

taskset -c 8 $E --load tools/tier-census.f -- 0 /tmp/t0.txt $C
taskset -c 8 $E --load tools/tier-census.f -- 1 /tmp/t1.txt $C
$E --load tools/tier-census-join.f -- /tmp/t0.txt /tmp/t1.txt

$E --load tools/tier-dump.f -- 0 ARRAY:A-LEN /tmp/a0.bin lib/array.f
$E --load tools/tier-dump.f -- 1 ARRAY:A-LEN /tmp/a1.bin lib/array.f
objdump -b binary -m aarch64 -D /tmp/a1.bin

taskset -c 8 $E --load tools/tier-bench.f -- 0 src/core/checker.f
taskset -c 8 $E --load tools/tier-bench.f -- 1 src/core/checker.f
```

Pin the timing runs, quote the load average with every number, and never point
`HABU_FIXPOINT_ENGINE` at a shared engine: `tools/build-fixpoint.f` promotes its
result over that path, which will replace the binary other lanes are measuring.

## What was not measured

- The self-build's hottest words (`IR-ARENA:RD@`, `ptr-field`, `cell+`) are
  baked into the engine and cannot be recompiled at tier 0 in process, so no
  benchmark here runs them at both tiers. Section 3 covers the self-build
  through compile rates instead.
- `src/core` and `src/compiler` are in the engine's baked prefix, so no checker
  or compiler word appears in the census. `tools/public-signatures-core.f` and
  `tools/lint/text.f` stand in for checker-shaped work — declaration scanning,
  tokenizing and comparison over real source — and they are real tools, not
  models of one.
- Tier-1 code size and instruction counts are deterministic and were identical
  across every run; the timings are wall clock on a shared machine and carry
  their load averages. The `branch` benchmark at tier 1 is the least stable
  figure here (6,844-8,811 µs across invocations).
- Nothing here measures the checker, only the two compilers behind it.
