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

Those tier-1 figures are the ones this table was taken at. Section 5 measures
where that time goes and reports what four changes since then removed: the same
trivial definition now costs 524 µs and the same corpus 3.880 s, so the ratio
above is 20.7x rather than 23.8x. The tier-0 column is untouched by them.

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

## 5. Where the per-word compile time goes

Measured 2026-09-16 with the engine's own sampling profiler (`prof-on` /
`prof-off` / `prof-report`, docs/debugging.md), against the engine
`/tmp/hazel-fH5`, sha256 `4242ccc8549fe369…`, which is this tree plus the
profiler slices. Sampling interval 250 µs; load average is quoted with each
run; both runs pinned to cpu10.

### Method

`set-tier` is engine-global and the compiler's own words are baked, so a
profile of compilation is taken by selecting the tier at the head of a driver
file, arming the clock, and loading real source through the engine's own
`included` / `required`:

```forth
package PFDRV
private
TRUSTED: SET ( n -- ) set-tier ;
public
: SELECT1 ( -- ) 1 SET ;
;package

PFDRV:SELECT1
250 prof-rate
0 prof-on
s" /tmp/gen-trivial.f" included      \ or the corpus `required` one file at a time
prof-off
prof-report
```

Two workloads:

- **floor** — 4,000 definitions `: PFTn ( n -- n ) 1 + ;`, i.e. the trivial body
  `tools/compile-floor.f` times, 4,000 times over. 11,523 samples, 0 `new`, 0
  `foreign`: 2.881 s, **720 µs per word**, which is the floor that tool reports
  on the same box (720 / 718 / 741 µs, load 12.1).
- **corpus** — the same 13 library and tool files section 1 censuses, 1,772
  words. 17,804 samples, 2 `foreign`: 4.451 s, **2.51 ms per word**, the same
  rate section 3 measured by wall clock.

`prof-report` selects its rows by **exclusive** count and prints 24 of them, so
the tables below are ordered that way; the inclusive column is on every row and
a word too cheap in itself to make the 24 cannot appear at all. Inclusive comes
from a frame-pointer-less stack walk and is a superset — an uninitialised spill
slot still holding a return address adds a frame — so treat exclusive as the
measurement and inclusive as the attribution.

### The floor: 4,000 trivial definitions, 720 µs each

Load average 8.31. `exc`/`inc` are samples, `%` of the 11,523 delivered.

| exc | % | inc | % | word | top callers (share of this row's exclusive) |
|---:|---:|---:|---:|---|---|
| 1405 | 12.1 | 1407 | 12.2 | `(PROT-SPAN)` | `!` 94.6, `c!` 5.1 |
| 1356 | 11.7 | 1673 | 14.5 | `IR-ARENA:RD@` | `IR-OP:RC@` 15.3, `IR-OP:RHDR-CK` 9.6, `IR-SCHEMA:RC@` 7.5, `IR-FUN:BC@` 6.6, `IR-FUN:BHDR-CK` 5.2 |
| 562 | 4.8 | 562 | 4.8 | `ptr-field` | `IR-ARENA:RD@` 41.8, `NULL-PTR` 32.3, `IR-ARENA:ADATA-FIELD` 7.1 |
| 548 | 4.7 | 548 | 4.7 | `IR-ARENA:RD-SIZE` | `IR-ARENA:FROZEN-READER` 18.4, `IR-OP:RHDR-CK` 14.4, `IR-OP:CNT` 8.9, `IR-FUN:BHDR-CK` 7.2 |
| 333 | 2.8 | 1663 | 14.4 | `!` | `CDIGEST:SLOT!` 39.3, `IR-ARENA:ACOUNT!` 7.8, `MEM:WB-RUN` 5.4 |
| 261 | 2.2 | 332 | 2.8 | `IR-BUILD:FIND-B` | `IR-BUILD:RESOLVE` 100 |
| 260 | 2.2 | 498 | 4.3 | `NULL-PTR-CELL` | `NULL-PTR` 100 |
| 242 | 2.1 | 242 | 2.1 | `data-base` | `NULL-PTR-CELL` 98.3 |
| 209 | 1.8 | 3489 | 30.2 | `CDIGEST:NATIVE-SLOT?` | `CDIGEST:SLOT!` 58.8, `CDIGEST:SLOT@` 41.1 |
| 191 | 1.6 | 327 | 2.8 | `IR-ARENA:LIVE-SLOT` | `IR-ARENA:OPEN-LIVE` 51.8, `IR-ARENA:PUSH` 29.3 |
| 168 | 1.4 | 1578 | 13.6 | `NULL-PTR` | `CDIGEST:NATIVE-SLOT?` 100 |
| 164 | 1.4 | 164 | 1.4 | `cell-view` | `IR-ARENA:RD@` 50.0, `CDIGEST:SLOT!` 26.2, `CDIGEST:SLOT@` 20.1 |
| 158 | 1.3 | 1639 | 14.2 | `CDIGEST:SLOT!` | `IR-SYM:BUCKETS-CLONE` 31.6, `IR-ARENA:PUSH` 24.0, `IR-ARENA:APPEND-SPAN` 23.4, `IR-ARENA:COPY-CELLS` 12.0, `IR-CTX:HDR!` 5.6 |
| 155 | 1.3 | 936 | 8.1 | `CDIGEST:SLOT@` | `IR-ARENA:APPEND-SPAN` 36.1, `IR-SYM:BUCKETS-CLONE` 29.0, `IR-CTX:HDR@` 16.7, `IR-ARENA:COPY-CELLS` 9.0 |
| 153 | 1.3 | 153 | 1.3 | `IR-CTX:SERIAL-LIVE?` | `IR-ARENA:LIVE-SLOT` 54.9, `IR-BUILD:RESOLVE` 25.4, `IR-ARENA:SWEEP` 9.8 |
| 128 | 1.1 | 320 | 2.7 | `IR-SCHEMA:SCAN-NAME` | `IR-SCHEMA:ROW-OF` 91.4, `IR-SCHEMA:DEFINED?` 7.8 |
| 106 | 0.9 | 106 | 0.9 | `IR-ID:LOCAL-N` | `NFROZEN:SAME-SYM?` 49.0, `IR-OP:ROW-ORD` 11.3 |
| 91 | 0.7 | 91 | 0.7 | `mod` | `IR-OP:RSHAPE-CK` 29.6, `IR-FUN:BSHAPE-CK` 20.8, `IR-SCHEMA:RSHAPE-CK` 12.0 |
| 81 | 0.7 | 81 | 0.7 | `IR-ARENA:AGEN@` | `IR-ARENA:SWEEP` 53.0, `IR-ARENA:PUSH` 24.6, `IR-ARENA:FREE-SLOT` 22.2 |
| 75 | 0.6 | 75 | 0.6 | `munmap` | `DYNAMIC-STORAGE:RELEASE` 50.6, `MEM:RELEASE-RANGE` 26.6, `IR-CTX:CHUNKS-FREE` 22.6 |
| 72 | 0.6 | 72 | 0.6 | `IR-ID:KEY-SERIAL` | `IR-ID:PACK-N` 100 |
| 72 | 0.6 | 160 | 1.3 | `NFROZEN:SAME-SYM?` | `A64EMIT:OPCODE-SLOT` 37.5, `A64COMB:OPCODE-SLOT` 31.9, `A64RAV:ATTR-INT` 16.6 |
| 71 | 0.6 | 71 | 0.6 | `IR-BUILD:BGEN@` | `IR-BUILD:FIND-B` 100 |
| 69 | 0.5 | 69 | 0.5 | `IR-OP:FROW-FIELDS` | `IR-OP:FROW-FIELD` 69.5, `IR-OP:FTILE-CK` 30.4 |

### The corpus: 1,772 real words, 2.51 ms each

Load average 7.27. `%` of the 17,804 delivered.

| exc | % | inc | % | word | top callers |
|---:|---:|---:|---:|---|---|
| 3155 | 17.7 | 3982 | 22.3 | `IR-ARENA:RD@` | `IR-OP:RC@` 16.6, `IR-SCHEMA:RC@` 13.1, `IR-OP:RHDR-CK` 8.8, `IR-FUN:BC@` 7.3, `IR-OP:ROW-AT` 4.5 |
| 1632 | 9.1 | 1639 | 9.2 | `(PROT-SPAN)` | `!` 94.7, `c!` 5.0 |
| 988 | 5.5 | 988 | 5.5 | `IR-ARENA:RD-SIZE` | `IR-ARENA:FROZEN-READER` 19.5, `IR-OP:RHDR-CK` 15.5, `IR-OP:CNT` 10.2, `IR-FUN:BHDR-CK` 10.1, `IR-OP:PHDR-CK` 6.1 |
| 977 | 5.4 | 977 | 5.4 | `ptr-field` | `IR-ARENA:RD@` 59.3, `NULL-PTR` 10.4, `IR-ARENA:ADATA-FIELD` 3.7, `A64RAV:D-AT-BUF` 3.2 |
| 323 | 1.8 | 1873 | 10.5 | `!` | `CDIGEST:SLOT!` 30.3, `IR-OP:FROW-USE` 6.8, `IR-ARENA:ACOUNT!` 4.6 |
| 305 | 1.7 | 305 | 1.7 | `cell-view` | `IR-ARENA:RD@` 80.9, `CDIGEST:SLOT@` 8.1 |
| 292 | 1.6 | 499 | 2.8 | `IR-ARENA:LIVE-SLOT` | `IR-ARENA:OPEN-LIVE` 56.8, `IR-ARENA:READ` 20.2, `IR-ARENA:PUSH` 15.7 |
| 284 | 1.5 | 907 | 5.0 | `IR-SCHEMA:SCAN-NAME` | `IR-SCHEMA:ROW-OF` 87.6, `IR-SCHEMA:DEFINED?` 10.9 |
| 256 | 1.4 | 331 | 1.8 | `IR-BUILD:FIND-B` | `IR-BUILD:RESOLVE` 100 |
| 251 | 1.4 | 251 | 1.4 | `IR-ID:LOCAL-N` | `NFROZEN:SAME-SYM?` 53.3, `IR-OP:ROW-ORD` 11.9 |
| 191 | 1.0 | 191 | 1.0 | `mod` | `IR-OP:RSHAPE-CK` 27.7, `IR-OP:AT-N` 15.1, `IR-FUN:BSHAPE-CK` 13.6 |
| 187 | 1.0 | 187 | 1.0 | `IR-CTX:SERIAL-LIVE?` | `IR-ARENA:LIVE-SLOT` 75.9, `IR-BUILD:RESOLVE` 15.5 |
| 187 | 1.0 | 398 | 2.2 | `NFROZEN:SAME-SYM?` | `A64EMIT:OPCODE-SLOT` 24.0, `A64COMB:OPCODE-SLOT` 22.9, `A64SEL:OPCODE-SLOT` 13.3 |
| 174 | 0.9 | 174 | 0.9 | `IR-OP:FROW-FIELDS` | `IR-OP:FROW-FIELD` 80.4, `IR-OP:FTILE-CK` 19.5 |
| 147 | 0.8 | 147 | 0.8 | `cell+` | `A64RAV:D-AT-BUF` 20.4, `A64RAV:C-AT-BUF` 7.4, `A64RA:B-ST-BUF` 5.4 |
| 147 | 0.8 | 292 | 1.6 | `NULL-PTR-CELL` | `NULL-PTR` 100 |
| 145 | 0.8 | 146 | 0.8 | `data-base` | `NULL-PTR-CELL` 100 |
| 137 | 0.7 | 137 | 0.7 | `IR-OP:ROW-CELL` | `IR-OP:RC@` 100 |
| 129 | 0.7 | 1427 | 8.0 | `CDIGEST:NATIVE-SLOT?` | `CDIGEST:SLOT!` 67.4, `CDIGEST:SLOT@` 32.5 |
| 114 | 0.6 | 114 | 0.6 | `NFROZEN:S-READ` | `NFROZEN:RD` 99.1 |
| 112 | 0.6 | 971 | 5.4 | `CDIGEST:SLOT!` | `IR-ARENA:PUSH` 31.2, `IR-ARENA:COPY-CELLS` 23.2, `IR-SYM:BUCKETS-ZERO` 14.2, `IR-SYM:BUCKETS-CLONE` 11.6, `IR-ARENA:APPEND-SPAN` 9.8 |
| 109 | 0.6 | 109 | 0.6 | `IR-ID:KEY-SERIAL` | `IR-ID:PACK-N` 100 |
| 101 | 0.5 | 1050 | 5.8 | `IR-OP:FTILE-CK` | `IR-OP:FROW-USE` 100 |
| 92 | 0.5 | 297 | 1.6 | `IR-ARENA:FROZEN-READER` | `IR-OP:FROW-USE` 35.8, `IR-OP:ROPS` 18.4, `IR-OP:ROPCODE@` 10.8 |

### The same two profiles, per phase, by inclusive time

`prof-report` selects its rows by exclusive count, and every word that names a
phase spends almost none: `NCOMP:SELECTED`, `IR-BUILD:FREEZE`, `A64SEL:SELECT`
and `A64RAV:ACCEPT` are call frames, not loops. The table below was taken with a
**measurement engine, not this tree**: `src/habu/prof.f` with `PROF-ROWS` at 120
and the selection loop's key moved from the exclusive counter to the entry's
`ENT-INCL` field, so the repeated maximum ranks by inclusive. Nothing of that is
committed; the engine was built, read and discarded, and the two workloads,
interval and pinning are the ones above.

**Read inclusive as a lower bound for a shallow word.** The stack walk scans
`PROF-WALK-CELLS` — 64 cells — of the interrupted stack and never leaves its 4
KiB block, so a word whose frame sits far below SP while a deep callee runs is
missed. That is why `NCOMP:WORK` reads 15.4 percent when it is in fact the whole
compile, and why `catch` reads 46.2: a quotation's frames are what the window
can still see. The phases in the middle of the pipeline — the verifiers, the
allocator, the table builders — have their own subtrees inside the window and
are the numbers to use.

Inclusive samples and percent of the delivered samples, floor (11,462) and
corpus (17,898):

| phase | floor | corpus |
|---|---:|---:|
| `IR-BUILD:TABLES-TRY` — create a module's seventeen tables | **2979 25.9%** | 849 4.7% |
| ` └ IR-SYM:NEW-FROM` — clone the dialect's interner | 1521 13.2% | 428 2.3% |
| ` └ └ IR-ARENA:APPEND-SPAN` — pool and rows, cell by cell | 736 6.4% | — |
| ` └ └ IR-SYM:INDEX-CLONE` / `BUCKETS-CLONE` | 726 6.3% / 731 6.3% | — |
| `A64IR:ENSURE-OP` — define the dialect's opcodes into the module | 978 8.5% | 924 5.1% |
| ` └ A64IR:DEFINE-ONE` → `IR-BUILD:DEFINE-OP` | 869 7.5% / 944 8.2% | 710 3.9% / 569 3.1% |
| ` └ IR-SCHEMA:DEFINE` | 568 4.9% | — |
| `NELAB:COLON` — elaborate the body | 299 2.6% | 636 3.5% |
| ` └ NELAB:SCAN-FUN` / `BUILD-FUN` | 271 2.3% / 423 3.6% | 1007 5.6% / 996 5.5% |
| `IR-BUILD:FREEZE` | 775 6.7% | 531 2.9% |
| ` └ IR-BUILD:VERIFY-CK` → **`IR-VERIFY:VERIFY`** | 1202 10.4% / **1234 10.7%** | 1734 9.6% / **2388 13.3%** |
| ` └ └ IR-VERIFY:OP-CK` / `OPS-CK` / `ATTRS-CK` | 755 6.5% / 756 6.5% / 309 2.6% | 1930 10.7% / 1875 10.4% / 1045 5.8% |
| `A64SEL:SELECT` — instruction selection | 838 7.3% | 412 2.3% |
| `A64COMB:REWRITE` — the combine rebuild | 438 3.8% | — |
| `A64RA:ALLOCATE` — register allocation | 621 5.4% | 650 3.6% |
| ` └ A64RA:WALK` / `MEASURE-ALL` | 768 6.7% / 279 2.4% | 2201 12.2% / 946 5.2% |
| **`A64RAV:ACCEPT`** — verify the allocation | **525 4.5%** | **1277 7.1%** |
| ` └ A64RAV:VERIFY` / `WALK` | 607 5.2% / 556 4.8% | 1122 6.2% / 1320 7.3% |
| `A64EMIT:EMIT` | 297 2.5% | — |
| `SHA256-FINAL` — the canonical digest, all of it | 267 2.3% | — |
| `IR-OP:FROW-USE` — resolve one operation's row window | 1115 9.7% | 2617 14.6% |
| `IR-SCHEMA:ROW-OF` → `SCAN-NAME` | 372 3.2% / 307 2.6% | 1074 6.0% / 950 5.3% |
| `IR-ARENA:RD@` | 1725 15.0% | 3966 22.1% |
| `CDIGEST:SLOT!` / `SLOT@` — as the arena's cell accessor | 1632 14.2% / 888 7.7% | 987 5.5% / 485 2.7% |

A dash is a phase that did not reach the 120 rows printed, so it is under about
2.5 percent on that workload.

**What this changes.**

- **Verification is the largest single phase, on both workloads.** The IR's own
  freeze verifier is 10.7 percent of the floor and 13.3 percent of the corpus,
  and the register-allocation verifier another 4.5 and 7.1 — together **15.2
  percent of the floor and 20.4 percent of the corpus**. Each runs once per
  module, and a trivial word builds three modules, so a word is verified three
  times over.
- **The canonical digest is not a cost.** `SHA256-FINAL` is 2.3 percent of the
  floor and does not reach the table on the corpus. `CDIGEST:SLOT!`/`SLOT@` are
  large, but they are not digesting: they are the canonical cell accessor the
  arena reads and writes its spans through, which is item 3 above.
- **Building a module is the floor's largest cost and the corpus's smallest.**
  `IR-BUILD:TABLES-TRY` is 25.9 percent of the floor and 4.7 percent of the
  corpus — the definition of a fixed per-module cost. Half of it is the interner
  clone; the dialect then defines its opcode schema into the fresh module again
  (`A64IR:ENSURE-OP`, 8.5 percent), which is a second copy of the same
  vocabulary in a second table.

### What the profile says

**There is no single fixed phase to delete.** The 24 rows sum to 59 percent of
the floor and 58 percent of the corpus, and the largest of them is 12
percent. The cost is one shape repeated: a cell of an IR arena is reached
through a call chain, and the compiler reaches a great many cells.

1. **Reading one IR record cell costs three calls.**
   `IR-ARENA:RD@` re-validates its reader token (generation, then state, then
   bound: four indexed loads) and then calls `ptr-field` and `cell-view`.
   Corpus-wide that is `RD@` 17.7 percent plus the 59.3 percent of `ptr-field`
   and the 80.9 percent of `cell-view` that come from it — **22.3 percent of all
   compile time in one accessor**, before its callers' own work. `RD-SIZE`
   (5.5 percent) is the same validation again for the count, and the callers
   named on both rows — `IR-OP:RHDR-CK`, `IR-FUN:BHDR-CK`, `IR-OP:PHDR-CK`,
   `IR-OP:CNT`, `IR-ARENA:FROZEN-READER` — are per-access header revalidation,
   not field reads: roughly a third of both rows.
2. **Writing one cell costs the span guard.** `(PROT-SPAN)` is 12.1 percent of
   the floor and 9.1 percent of the corpus, and 94.6 percent of it is reached
   from `!`. That is the engine's protected-span check, one call and nine band
   tests per store, and nothing in the compiler can make a store cheaper than
   one call — only make fewer of them.
3. **The bulk copies go through the canonical slot words, cell by cell.**
   `CDIGEST:SLOT@`/`SLOT!` exist so a preimage is byte-canonical on any host;
   each call asks `NATIVE-SLOT?`, which calls `NULL-PTR` → `NULL-PTR-CELL` →
   `data-base` → `ptr-field` to re-derive a process constant. On the floor
   workload that chain is `SLOT!` 1.3 + `SLOT@` 1.3 + `NATIVE-SLOT?` 1.8 +
   `NULL-PTR` 1.4 + `NULL-PTR-CELL` 2.2 + `data-base` 2.1 = **10.1 percent
   exclusive**, plus its shares of `ptr-field`, `cell-view` and `!`. The callers
   name exactly where: `IR-SYM:BUCKETS-CLONE`, `IR-ARENA:APPEND-SPAN`,
   `IR-ARENA:COPY-CELLS` and `IR-SYM:BUCKETS-ZERO` — the prototype clone every
   new builder takes — plus `IR-ARENA:PUSH` and `IR-CTX:HDR@`/`HDR!`.
   `IR-ARENA:CELL-AT` and `RD@` already bypass this path when `NATIVE-CELLS?`;
   the copies do not.
4. **The arena registry is scanned twice per arena created.**
   `IR-ARENA:NEW` runs `SWEEP` and then `FREE-SLOT`, each a loop over all 64
   registry slots, and a builder creates seventeen arenas. `IR-ARENA:AGEN@`
   (0.7 percent, 53.0 from `SWEEP` and 22.2 from `FREE-SLOT`) and
   `IR-CTX:SERIAL-LIVE?` (1.3 percent, 9.8 from `SWEEP`) are what is left of it
   after the loop bodies inline.
5. **The schema is searched by name, linearly, on every opcode query.**
   `IR-SCHEMA:ROW-OF` calls `SCAN-NAME`, which walks every row of the dialect's
   schema table comparing a symbol ordinal, one `IR-ARENA:RD@` per row. That is
   5.0 percent of the corpus inclusive and 13.1 percent of every `RD@` call.
   `NFROZEN:SAME-SYM?` (2.2 percent, from four dialects' `OPCODE-SLOT`) is the
   same question asked a second way.

**The register-allocation verifier spends its time in other words' rows.** No
`A64RAV` word reaches either top-24 list on its own account: it appears there
only as a caller of `ptr-field` (3.2 + 2.6 percent of that row), `cell+` (20.4 +
7.4 percent) and `!` (3.7 percent) in the corpus, and nowhere at all on the
floor. The inclusive table below prices it properly at 4.5 percent of the floor
and 7.1 percent of the corpus — real, proportional to the module's values and
blocks, and second to the IR's own freeze verifier rather than first.

**What is fixed, measured without the profiler.** `tools/compile-floor.f` times
two bodies: `1 +`, which carries a combinable pair, and `swap drop`, which does
not. A pair makes `NCOMP:COMBINED` rebuild the module into a fresh builder;
without one the module is handed straight back. On this box that is 720 µs
against 490 µs — so **one extra module rebuild costs 230 µs**, on a two-operation
body where the rewriting itself is nothing. The trivial word builds three
modules (the HIR builder, the selection's A64 builder, the combine rebuild), and
three times 230 µs is the whole floor. What is fixed per word is therefore *the
builder*: seventeen arenas, two registry scans each, and a verbatim clone of
the dialect's whole interned vocabulary — items 3 and 4 above, paid once per
module built rather than once per word.

## 6. What a per-word scratch region has to prove

The shape this is written against: one scratch region per compilation, a mark
at the word's start, every table an index-addressed slice taken from it, a
release at the word's end; passes hand each other offsets rather than handles;
sizes are named constants from measured high-water marks with a named refusal
instead of growth by copy; no registry, no generations, no per-creation scans.
The checker's nominal index types stay compile-time types, so a raw cell still
cannot become an index. This section is what that lane has to establish, and it
is written from the source rather than from the plan.

**The three subsections that follow describe d2b2c3e6**, the tree this section
was written against, in its present tense. The lane landed three commits on it
and the four subsections after those say what each one changed and what it was
worth, so read the description first and the outcome second; where they
disagree, the outcome is the tree.

### What is actually freed today, and when

**Storage is never reclaimed while a word compiles.** `IR-CTX:SCRATCH-TAKE`
(src/compiler/ir/context.f) is a chunked bump cursor whose own contract line is
"Every returned span stays at the same address until the context ends"; the one
`munmap` in that file is `CHUNKS-FREE`, called from the context teardown. `IR-ARENA:GROW-TO`
says the same thing from the other side: a doubled span is spent whether or not
the row that asked for it survives, because "context scratch is a monotonic bump
cursor with no free".

`IR-ARENA:ABORT` and `RETIRE` resolve a slot and then write `0 slot AGEN!`, and
that is the whole of them. They free a **registry row**, not a span. `IR-BUILD:RETIRE` and `ABORT` retire the module's
seventeen tables, which is seventeen registry rows and no memory. So the
mid-word "release" the compiler does today releases nothing but slots.

**Why it does it at all is slot pressure, and the pressure is the registry's
own.** A builder creates seventeen arenas (`IR-BUILD:TABLES-BUILD`: two symbol,
two type, two attribute, one source, two schema, three operation, three
function, two edge). The registry holds 64 rows (`SLOT-MAX`, arena.f) and the
builder registry 16. `NCOMP:EMITTED` holds a chain — the HIR builder, the tape,
the selection's A64 builder, the combine rebuild, then a lowering rebuild per
spill turn — and `IR-BUILD:RETIRE`'s own comment records what happens without
the mid-word retire: "three chained builders plus the model and tape arenas is
already the ceiling - which is how a routine that combined and then spilled ran
out (E-IR-ARENA-SLOTS)".

**So the seal does not defend against a dangling pointer.** It defends against a
handle whose registry row was given to a later arena. That is the fact the lane
turns into its proof: remove row reuse and the thing being caught cannot happen.

### Which reads check generation and state

Liveness and identity of the row, i.e. what a region would remove:

| site | what it asks |
|---|---|
| `IR-ARENA:LIVE-SLOT`, `FROZEN-SLOT` | handle non-zero; the stored handle equals the presented one (generation and slot); `IR-CTX:SERIAL-LIVE?` of the owner; the row's state |
| `IR-ARENA:RD@`, `RD-SIZE`, `RD-FIND` | the token's generation against the row's, then the token's state against the row's |
| `IR-ARENA:IDX-AT` | the generation packed into a cell-id against the row's |
| `IR-ARENA:FROZEN-READER` | the token was opened against a frozen row |
| `IR-ARENA:OWN-CHECK` | the writing context's serial against the row's owner |
| `IR-BUILD:RESOLVE` | a generation to a slot by scanning `BGENS`, then owner liveness; `LIVE-SLOT`/`FROZEN-SLOT` add the state |
| `IR-CTX:SERIAL-LIVE?` | the owning context's serial is still live |

Shape and identity, i.e. what a region does **not** remove: every table's
`*HDR-CK` (format tag and row-width modulus), `SERIAL-CK` (the module serial a
header binds to, which is what refuses a foreign key or a cross-module store
pairing), every ordinal bound check, and the window revalidations
`IR-SYM:SPAN-CK-N` and `IR-SCHEMA:WIN-CK-N`. These answer "is this the table I
think it is, and is this index inside it", not "is this object still alive".

### Which of those can observe a freed object

**None, today or after.** Storage is not freed mid-word now, so no read can
reach a freed span between a mark and a release; the release is the context
teardown, and the teardown is already where `RETIRE-OWNED` zeroes every row this
context owns before the mapping goes. Under the region the release moves to the
word's end and the same property holds for the same reason, with one thing to
prove rather than assume:

1. **The region only grows within a word.** No slice is handed back or reused
   before the release, so an offset a pass obtained stays valid — and stays
   pointing at the same bytes — for the rest of the word. This is what makes the
   generation unnecessary; it is not true of any design that recycles a slice.
2. **Two extents, not one.** The dialect prototype (`A64IR:PROTO`,
   `HIR`'s `PROTO`) and the session vocabulary (`HIR-WORD:SESS-POOL` /
   `SESS-ROWS`) live in the session context `NCOMP:SESSION-START` opens, which
   spans the whole load and is given up at capture. A word's tables read them
   (`IR-BUILD:SYM-NEW` copies from them today). So the shape is a session region
   and a word region, with word-to-session offsets allowed and never the
   reverse, and the obligation is that the session region's extent dominates
   every word region taken inside it — which `SESSION-OPEN` before every
   `IR-CTX:WITH-CONTEXT` already gives.
3. **Frozen is not liveness and does not go away.** `ST-LIVE` / `ST-FROZEN`
   answers "may this be appended to", and `IR-OP:FROW-USE` demands a frozen
   reader before it caches a row's facts. That becomes one sealed bit per slice,
   checked where the state is checked now. The liveness argument does not reach
   it.

### What the RETIRE contract becomes

Today `IR-BUILD:RETIRE` states its own contract: it cannot prove a module has no
readers, so it makes being wrong loud — a later read through that handle is
`E-IR-BUILD-RETIRED`, and through any view or index it handed out
`E-IR-ARENA-OWNER`, "rather than returning something plausible".

Under the region there is no mid-word retire: release happens once, at the word's
end, and a consumer that holds an offset past it is reading a released region.
That is the one guarantee the shape gives up, and it should not be given up
silently:

- **Inside the word**, a pass that reads a module a later pass superseded now
  gets intact, valid, stale data instead of a refusal. The honest replacement is
  static, not dynamic: a rewriting pass should **consume** its input — take the
  module offset by value and not return it — so holding the old one afterwards
  is a checker error at the call site rather than a runtime refusal that may
  never fire. `NCOMP:EMITTED`'s chain (`m0` → `CLOSED` → `SELECTED` →
  `COMBINED` → `LOWER-FIXPOINT`) is already written that way by hand; making it
  the type is what replaces the seal.
- **Across the word boundary**, the release must be loud. Nothing may hold an
  offset into a released region, and the cheapest honest check is one epoch cell
  per region compared at the region boundary — not per read, which is the cost
  this whole section exists to remove.

### What the lane must measure before it sizes anything

Slice sizes are to be named constants from measured high-water marks. The marks
do not exist yet: `IR-BUILD`'s `D-SYMS`, `D-OPS`, `D-VALS`, `D-OPOOL`, `D-FUNS`,
`D-BLKS`, `D-FPOOL` and their siblings are committed *ceilings*, chosen to be
large, and `PLAN-DEFAULT` hands every module the same ones. What a region needs
is the peak each table actually reaches over a real corpus, per table, with the
refusal that fires when a word exceeds it named and tested. The measurement is
the same shape as the ones above — a counter per table read at the end of a load
of the thirteen-file corpus — and it belongs to that lane's first commit, before
any size is written down.

### The marks, measured

Taken 2026-09-16 on d2b2c3e6 with a temporary counter per table, sampled where
`IR-BUILD` freezes or aborts a module's seventeen tables, and a second counter
on the context scratch cursor. Two loads: the thirteen-file corpus of section 8
at tier 1 (1,967 words) and the engine's own self-build, which is every
definition under `src/` and is the larger of the two by an order of magnitude.

| table | corpus peak | self-build peak |
|---|---:|---:|
| `T-SP` symbol pool | 471 | 857 |
| `T-SR` symbol rows | 999 | 1,569 |
| `T-TP` type pool | 16 | 30 |
| `T-TR` type rows | 23 | 47 |
| `T-AP` attribute pool | 3 | 3 |
| `T-AR` attribute rows | 283 | 1,208 |
| `T-SA` source rows | 9 | 9 |
| `T-QP` schema pool | 85 | 110 |
| `T-QR` schema rows | 678 | 798 |
| `T-OP` operand pool | 1,950 | 8,113 |
| `T-OV` value rows | 1,655 | 8,687 |
| `T-OR` operation rows | 4,407 | 20,763 |
| `T-FP` function attribute pool | 3 | 3 |
| `T-FR` function rows | 39 | 243 |
| `T-BR` block rows | 399 | 2,073 |
| `T-EP` predecessor pool | 60 | 307 |
| `T-ER` edge rows | 135 | 693 |

Every figure is cells. The peak is per module, over every module either load
built, so it is the worst single definition and not a sum.

The scratch cursor's own peak — every live context's storage at once, which is
the session's plus the deepest definition's — is 720,040 bytes over the corpus
and 2,885,144 bytes over the self-build. That is what the region's reservation
has to dominate, and it is two orders of magnitude below the 64 MiB the region
reserves.

### What the region itself was worth

The first commit of that lane replaced the per-context chunk chain and the
per-context header mapping with one reserved region, a mark taken when a context
is entered and a release when it leaves. Measured on the corpus and on
`tools/compile-floor.f`, engines built from d2b2c3e6 and from the commit, pinned
to cpu8:

| | before | after |
|---|---:|---:|
| `mmap` calls over compile-floor's 300 definitions | 774 | 173 |
| `munmap` calls over the same | 627 | 27 |
| minor faults, whole process | 2,467 | 1,271 |
| user instructions, compile-floor | 2,389,985,336 | 2,373,827,093 |
| user instructions, the 13-file corpus | 52,852,973,348 | 52,812,777,736 |

The instruction counts move by 0.68 and 0.08 percent because the work removed is
the kernel's: two mappings and two unmappings per definition, and the faults on
pages a definition had to take again because its predecessor gave them back. The
corpus census is byte-identical across the change (1,967 words, 179,012 bytes),
which is the point - this changes where tables live, not what is emitted.

### What the registry's checks cost, and what one token gave back

The denominator for everything below is one tier-1 definition of
`( n -- n ) 1 + ;`, measured as the difference between a `tools/tier-census.f`
run over 500 such definitions and one over a single definition, under
`perf stat -e instructions:u`, which is deterministic here to one part in 10^9.
On the region commit that is **5,805,240 user instructions per definition**.

An engine built with every generation, state and owner-liveness test in
`IR-ARENA` reduced to its bound check - unsound, built only to price them -
compiles the same definition in 4,887,014 and the thirteen-file corpus in
43,517,847,938 against 52,812,777,736. So the registry's checking, and the call
frames the checking keeps alive, is **15.8 percent of a trivial definition and
17.6 percent of the corpus**, and it is by a wide margin the largest remaining
item in the floor.

Counted over the same 500 definitions, per definition: 16,450 `RD@`, 2,905
`LIVE-SLOT`, 316 `FROZEN-SLOT`, 1,717 readers opened, 817 free-slot scan steps
and 37 arenas created. Each of the first three asked the same question twice -
load the row's handle, shift its generation out, compare, then shift the state
out of the presented token, load the row's state, compare - over two cells.

Storing the reader token itself as the row's one identity cell makes every one
of those a single indexed load and a single compare, with the mismatches told
apart off the read path. It keeps every test and every error: 5,326,094
instructions per definition (**-8.25 percent**), 47,763,870,582 over the corpus
(**-9.56 percent**), the census byte-identical. That is half of what the
unsound ablation showed; the other half is the owner-liveness probe that the
handle resolutions still make and the call frames it keeps.

A second commit deleted the owner-liveness probe the two handle resolutions
still made, and the sweep that backstopped it, on the invariant that every
context row is installed by `IR-CTX:CTX-TAKE` and given back by `CTX-RETIRE` -
armed with `finally`, so it runs on a throw too - and that `CTX-RETIRE`
announces the dying serial to `IR-ARENA:RETIRE-OWNED` before the region cursor
moves. `IR-ARENA:CAPTURE-PREPARE` is now where that invariant is tested rather
than swept over: with no context open, a row still holding a token is
`E-IR-ARENA-STATE`, and the check runs at every capture, which is every
self-build. That took a further 2.62 percent off the definition (5,186,689) and
2.38 percent off the corpus (46,627,445,818), again byte-identical.

Against the region commit the two together are **-10.7 percent per trivial
definition and -11.7 percent on the corpus**, and against d2b2c3e6 -11.8 percent
on the corpus. What remains of the ablation's 17.6 percent is `IR-BUILD`'s own
resolution: a builder handle carries only a generation, so `FIND-B` scans up to
sixteen rows per resolution and `USE` runs one per append.

### What growth by copy actually costs

Counted the same way, with a counter in `IR-ARENA:GROW-TO`. Per trivial
definition: 37 arenas created, 42 growth steps, **668 cells copied** and 4,638
cells taken from the region by the doubling series. Over the thirteen-file
corpus: 54,035 arenas, 100,925 growth steps, 4,353,335 cells copied.

668 cells is about 1,300 load-store pairs against 5,805,240 instructions, so
growth by copy is **under 0.05 percent of a definition**. The lane that was to
replace it with a fixed slice per table therefore has no measured cost to
remove, and a fixed slice has two prices: the sum of the self-build's per-table
marks is 45,513 cells, so three modules would reserve about 1.1 MB per
definition against the 37 KB the doubling series takes, and a word past a mark
would be a hard refusal where today it grows. Both numbers are here so the
decision is made on them.

### What this lane did not do, and why it is recorded here

Borrowing the dialect's interner instead of cloning it into every module was
designed and costed and then not done. The workable form is a side table inside
`IR-SYM` keyed by registry slot, exactly as the bucket index already is, holding
the borrowed pair and the base ordinal count, so no public signature changes —
`IR-SYM:FCOPY` has callers in `native/select.f` and `combine.f` that another
lane owns. It is sound because a prototype only ever appends, so its rows below
the borrow point are immutable, and a base hit at an ordinal at or above the
borrow point is treated as a miss. The work is that `CNT` means both "rows in
this arena" and "ordinals in this interner" and the borrow splits them across
eight call sites in a file whose interning invariant is machine-checked.

It was dropped on its measured value. The clone was 13.2 percent of the floor
when it was measured; making the canonical slot words copy whole runs
(section 5, item 3) took 9.1 percent of the floor off without changing any
semantics, which leaves the borrow four to five percent — in the file whose
registry and generations this section proposes to delete. Under the region the
clone has nowhere to go anyway: the prototype is a slice of the session region
and a word's interner is a slice of the word's, and borrowing is what an offset
into the session region already is.

### What deleting the registry was worth, and what it cost

The lane the subsections above asked for landed on 2026-09-17: the arena
descriptor moved into the region, a handle became the region offset of that
descriptor packed with its state and the region's release epoch, and the
64-slot registry, the generation counter, the allocation scope and the
retirement observer went with it.

**The denominator moved first.** The 5,186,689 above was measured on 30df3a77;
on 09263557, the tree this lane started from, the same trivial tier-1
definition is **4,977,673** user instructions and the thirteen-file corpus is
**50,738,237,045** over 2,123 words in 187,564 bytes — the corpus census grew
by 156 words since section 1 was written, so the corpus figures are not
comparable across that gap while the per-definition ones are. Against the
4,887,014 the unsound ablation showed, the registry machinery still standing
was therefore **1.8 percent** of a definition, not the 5.8 percent the dot was
written against: three earlier lanes had already taken most of it.

Every figure below is `perf stat -e instructions:u` on cpu8, the difference
between a `tools/tier-census.f` run over 500 trivial definitions and one over a
single definition, divided by 499. Load average 1.9 to 7.0 across the runs;
instruction counts do not move with it.

| engine | per trivial definition | against the base |
|---|---:|---:|
| 09263557, the base | 4,977,673 | — |
| offsets, base held in a `PERSISTED-PTR-VARIABLE` | 5,870,493 | +17.9% |
| the same, base in a marked bare cell | 5,268,343 | +5.8% |
| the same, warm field reads spelled out | 5,086,133 | +2.2% |
| the same, no per-resolution region-bound call | 4,963,087 | -0.3% |
| landed (bound check also out of `LIVE?`) | **4,950,225** | **-0.55%** |

Over the corpus the landed engine is **50,669,615,296** against 50,738,237,045,
**-0.14 percent**, with the census byte-identical on both sides (md5
`f84176888c0be2c8c21e36edb9e33c77`).

**What the three repairs were.** Each was found with the in-binary sampler over
2,000 trivial definitions, not guessed.

1. `PTR-VARIABLE` and `PERSISTED-PTR-VARIABLE` are `create` plus `does>`, so
   reading one is a call into the does> body which then calls `ptr-field`. The
   region base is read on every arena read: the does> body alone was **8.9
   percent** of the samples, 48 percent of them from `IR-ARENA:RD@` and 43 from
   its resolution helper. The base now lives in a bare cell marked with
   `ptr-cell-mark` — what the definer marks itself — and the reads spell out the
   one `ptr-field` the definer would have reached anyway.
2. The descriptor's field accessors (`D@`, `TOK-OFF`, `RBASE@`) were 101 samples
   between them, all of them a call frame around an add and a load. The words
   that run per read now spell their field reads out, exactly as `RD@` already
   spelled out its registry loads.
3. A region-bound test per handle resolution — `IR-CTX:REGION-HOLDS?`, which the
   dot asked for — was 49 samples, **1.7 percent** of a definition, for a
   question already answered: a descriptor in bytes the region has taken back
   was zeroed by its owner's teardown before the cursor moved, bytes handed out
   again carry a later epoch, and a span cannot be released while its owner
   lives because a scratch take by a context a deeper one encloses is now
   `E-IR-CTX-NESTED`. It is out of the read path and out of the tree.

**What is structural rather than measured.** The slot ceiling is gone: a
compilation may hold as many arenas as its region has bytes for, so
`E-IR-ARENA-SLOTS` and the mid-word retirement pressure that named it are gone,
and `IR-BUILD`'s "at most four modules live at once" is not a rule any more.
`IR-ARENA` lost `REGISTRY-CAP`, `REGISTRY-SLOT`, `RETIRE-OBSERVER!` and the
three `SCOPE-` words and gained one, `SIDE-FIELD`: the one observer's cell now
lives in the descriptor, so `IR-SYM`'s bucket index is born empty with its arena
and dies with it instead of being cleared through a retirement callback.

**What is left, measured.** `IR-BUILD:FIND-B` is 65 of 2,852 samples, **2.3
percent** of a trivial definition: a builder handle still carries only a
generation, so every resolution scans up to sixteen registry rows and `USE` runs
one per append. It is the same fix this lane made one layer down — pack the slot
into the handle, or move the builder's record into the region — and it is now
the largest registry cost in the compiler.

## 7. Where tier-0 compile time goes

Measured 2026-09-16 with `tools/tier0-profile.f` and the in-binary sampling
profiler (`docs/debugging.md`, "Sampling profiler") at a 50 µs interval, on an
engine built from this tree in `/tmp/hz-jit`, pinned with `taskset -c 8`, at a
1-minute load average of 7.4-9.9. Two workloads: the same 13-file corpus as
above (1,802 definitions), and 4,000 copies of the trivial definition
`compile-floor` uses, `: PVn ( n -- n ) 1 + ;`.

**The checker, not the JIT, owns tier-0 compile time.** Tier 0 is usually
described as "the direct JIT", and the reflex is to look at `src/habu/habu2.f`
for its cost. The profile says otherwise: not one tier-0 emitter appears in 64
reported rows at either workload, while `src/core/checker.f` words hold the top
of both, and turning the check hook off with `0 set-check` removes 73 percent of
the time for a trivial definition. What is left — tokenize, dictionary lookup,
emit and publish together — is 9.0 µs per trivial definition.

| workload | per definition | samples | `words` | `other` |
|---|---:|---:|---:|---:|
| corpus, checked | 83.6 µs | 3,012 | 2,629 | 379 |
| trivial, checked | 33.1 µs | 2,648 | 1,859 | 789 |
| trivial, `0 set-check` | 9.0 µs | 710 | 59 | 651 |

The last row is the phase split, measured rather than attributed. `words`
collapsing from 1,859 to 59 is the same statement from the other side: with the
hook off, almost nothing the profiler can name runs at all, because the
engine's own interpreter loop, tokenizer, tier-0 emitters and publish step are
unregistered engine text and land in `other`. That is also the limit of this
instrument — it cannot separate emit from tokenize from publish, only bound the
three of them together. The corpus has no `0 set-check` twin: it exports, and
an unchecked load of it dies `E-EXPORT-UNDEFINED` (7113).

### Per-phase inclusive, with callers

Inclusive percentages are of all samples. The stack walk is frame-pointer-less
and, as `docs/debugging.md` states, recognises return addresses by value, so
inclusive is a **superset** of the true chain: `DO-TOK1` reading higher than
`CHECK-SCAN`, which is its only caller, is that inflation made visible and
bounds it at about ten points. Exclusive counts are exact.

| phase | root | incl, corpus | incl, trivial | callers of the root |
|---|---|---:|---:|---|
| check | `CHECK-SCAN` | 55.6% | 37.9% | `CHECK` 100% |
| check, per token | `DO-TOK1` | 66.0% | 25.7% | `CHECK-SCAN` 100% |
| tokenize, fold | `TKF` | 20.9% | 6.3% | `DO-TOK1` 100% / 66.6% |
| lookup, symbols | `SYM-FIND` | 11.4% | 5.6% | `CHECKER-PKG-SYM?` 100% / `SYM-INTERN` 50% |
| lookup, signatures | `USIGS` | 8.6% | 8.5% | `E-PTR` 72.7% / 75.0% |
| lookup, hash | `HIDX-HASH` | 6.9% | — | `SYM-FIND` 83.3%, `HIDX-ROW-HASH` 16.6% |
| lookup, prims | `PRIM-FIRST-SCAN` | 5.3% | — | `PRIM-FIRST-IDX` 100% |
| effect interning | `E-INTERN` | — | 12.6% | `E-COPY*` 100% |
| store guard | `!` | 38.0% | 29.1% | `CORE-STR=`, `HIDX-H+`, `E-I-AK-RESET` |
| tokenize + emit + publish | (`other`) | ≤ 12.6% | ≤ 29.8% | not nameable, see above |

A dash means the row fell outside the 64 the report prints at that workload, not
that the word was idle.

### Top 30 exclusive words, corpus

| word | excl | % | incl | % | top callers |
|---|---:|---:|---:|---:|---|
| `(PROT-SPAN)` | 1006 | 33.3 | 1006 | 33.3 | `!` 97.6%, `c!` 2.3% |
| `ptr-field` | 197 | 6.5 | 197 | 6.5 | `PERSISTED-PTR-VARIABLE;does` 75.1%, `PTR-VARIABLE;does` 9.6% |
| `!` | 164 | 5.4 | 1146 | 38.0 | `CORE-STR=` 15.2%, `HIDX-H+` 12.8%, `E-I-AK-RESET` 6.7% |
| `CORE-STR=` | 93 | 3.0 | 289 | 9.5 | `CF-TOK?` 32.2%, `LAYOUT-XPORT-TOK?` 19.3%, `DO-TOK1` 12.9% |
| `TAG` | 60 | 1.9 | 60 | 1.9 | `ISVAR` 43.3%, `ISROW` 35.0% |
| `PERSISTED-PTR-VARIABLE;does` | 54 | 1.7 | 202 | 6.7 | `RVT` 20.3%, `USIGS` 18.5%, `TVT` 11.1% |
| `SYM-FOLD-C` | 46 | 1.5 | 46 | 1.5 | `HIDX-H+` 71.7%, `SYM-STR=CI` 28.2% |
| `HIDX-H$` | 41 | 1.3 | 183 | 6.0 | `HIDX-HASH` 100% |
| `CORE-STR=CI` | 38 | 1.2 | 87 | 2.8 | `UNSAFE-TOK?` 52.6%, `RETIRED-TOK?` 18.4% |
| `PE-ROW` | 35 | 1.1 | 35 | 1.1 | `PE-SYM@` 54.2%, `PE-FLAGS@` 45.7% |
| `RV-NEXT?` | 33 | 1.0 | 91 | 3.0 | `R-RES-WALK` 100% |
| `cell-view` | 32 | 1.0 | 32 | 1.0 | `EN.TAG` 46.8%, `EN.A` 9.3% |
| `PRIM-FIRST-SCAN` | 30 | 0.9 | 161 | 5.3 | `PRIM-FIRST-IDX` 100% |
| `RES-FALSE` | 29 | 0.9 | 29 | 0.9 | `TV-NEXT?` 24.1%, `RV-NEXT?` 20.6% |
| `PAY` | 24 | 0.7 | 24 | 0.7 | `RV-NEXT?` 41.6%, `P>TYPE` 16.6% |
| `ISVAR` | 17 | 0.5 | 43 | 1.4 | `TV-NEXT?` 70.5%, `T-BOUND-VAR?` 23.5% |
| `SYM-MATCH?` | 16 | 0.5 | 73 | 2.4 | `SYM-FIND` 100% |
| `SYM-STR=CI` | 15 | 0.4 | 28 | 0.9 | `SYM-MATCH?` 100% |
| `SYM-CAP` | 15 | 0.4 | 15 | 0.4 | `HIDX-CELL` 53.3%, `IDX-HEADS-CLEAR` 20.0% |
| `TV-NEXT?` | 13 | 0.4 | 72 | 2.3 | `T-RES-WALK` 100% |
| `XREF-CELL@` | 13 | 0.4 | 13 | 0.4 | `XREF-WORDLIST` 61.5%, `XREF-RAW-LEN` 23.0% |
| `T-RES` | 12 | 0.3 | 184 | 6.1 | `U-TYPE` 25.0%, `LIN-TYPE-COUNT*` 16.6% |
| `ISROW` | 11 | 0.3 | 32 | 1.0 | `R-BOUND-VAR?` 45.4% |
| `R-RES` | 11 | 0.3 | 209 | 6.9 | `LIN-ROW-COUNT` 36.3%, `U-ROW` 18.1% |
| `USIGS` | 11 | 0.3 | 260 | 8.6 | `E-PTR` 72.7% |
| `HIDX-MEM-FIELD` | 11 | 0.3 | 63 | 2.0 | `HIDX-MEM@` 100% |
| `E-KEY-N` | 11 | 0.3 | 23 | 0.7 | `E-NODE-KEYS=` 90.9% |
| `CHECKER-COLON-SCAN` | 11 | 0.3 | 46 | 1.5 | `CHECKER-QUALIFIED?` 100% |
| `PE-ACTIVE?` | 10 | 0.3 | 32 | 1.0 | `PRIM-FIRST-SCAN` 100% |
| `TOKFOLD` | 10 | 0.3 | 32 | 1.0 | `DO-TOK1` 90.0% |

The 64 reported rows cover 87.1 percent of `words` for the corpus and 88.6
percent for the trivial workload; the rest is a long tail below 5 samples.

### Top 30 exclusive words, trivial definition

| word | excl | % | incl | % | top callers |
|---|---:|---:|---:|---:|---|
| `(PROT-SPAN)` | 669 | 25.2 | 669 | 25.2 | `!` 97.0%, `c!` 2.9% |
| `ptr-field` | 146 | 5.5 | 146 | 5.5 | `PERSISTED-PTR-VARIABLE;does` 65.7%, `PTR-VARIABLE;does` 18.4% |
| `!` | 122 | 4.6 | 771 | 29.1 | `CORE-STR=` 11.4%, `CORE-STR=CI` 5.7%, `CHECK-RESET` 4.9% |
| `PERSISTED-PTR-VARIABLE;does` | 56 | 2.1 | 152 | 5.7 | `USIGS` 48.2%, `RVT` 14.2% |
| `cell-view` | 48 | 1.8 | 48 | 1.8 | `EN.TAG` 52.0%, `EN.A` 8.3% |
| `TAG` | 43 | 1.6 | 43 | 1.6 | `ISROW` 53.4%, `ISVAR` 20.9% |
| `RES-FALSE` | 41 | 1.5 | 41 | 1.5 | `RV-NEXT?` 31.7%, `TV-NEXT?` 21.9% |
| `CORE-STR=` | 33 | 1.2 | 149 | 5.6 | `LAYOUT-XPORT-TOK?` 33.3%, `CF-TOK?` 18.1%, `DELIM?` 12.1% |
| `USIGS` | 28 | 1.0 | 226 | 8.5 | `E-PTR` 75.0%, `E-OFF` 7.1% |
| `RV-NEXT?` | 25 | 0.9 | 86 | 3.2 | `R-RES-WALK` 100% |
| `E-KEY-N` | 21 | 0.7 | 52 | 1.9 | `E-NODE-KEYS=` 80.9% |
| `ISROW` | 19 | 0.7 | 42 | 1.5 | `RV-NEXT?` 63.1%, `R-BOUND-VAR?` 36.8% |
| `E-PTR` | 19 | 0.7 | 140 | 5.2 | `E-NODE-TAG` 52.6%, `E-KEY` 42.1% |
| `HIDX-H$` | 17 | 0.6 | 66 | 2.4 | `HIDX-HASH` 100% |
| `PAY` | 16 | 0.6 | 16 | 0.6 | `RV-NEXT?` 31.2%, `P>TYPE` 25.0% |
| `R-RES` | 16 | 0.6 | 218 | 8.2 | `LIN-ROW-COUNT` 31.2%, `E-RES` 25.0% |
| `E-NODE-KEYS=` | 16 | 0.6 | 124 | 4.6 | `E-NODE-SAME?` 100% |
| `CORE-STR=CI` | 15 | 0.5 | 32 | 1.2 | `UNSAFE-TOK?` 73.3%, `RETIRED-TOK?` 26.6% |
| `SYM-FOLD-C` | 15 | 0.5 | 15 | 0.5 | `HIDX-H+` 93.3% |
| `PTR-VARIABLE;does` | 14 | 0.5 | 41 | 1.5 | `HIDX-MEM-FIELD` 28.5%, `UIX-READY?` 21.4% |
| `ISVAR` | 13 | 0.4 | 22 | 0.8 | `TV-NEXT?` 76.9% |
| `T-RES-WALK` | 12 | 0.4 | 40 | 1.5 | `T-RES` 100% |
| `HIDX-CELL` | 12 | 0.4 | 41 | 1.5 | `IDX-HEAD!` 41.6%, `HIDX-BKT-CLEAR` 16.6% |
| `T-RES` | 11 | 0.4 | 112 | 4.2 | `HIDDEN-PARAM?` 36.3%, `U-TYPE` 18.1% |
| `CHECK-SCAN` | 11 | 0.4 | 1004 | 37.9 | `CHECK` 100% |
| `SYM-STR=CI` | 10 | 0.3 | 12 | 0.4 | `SYM-MATCH?` 100% |
| `SYM-CAP` | 10 | 0.3 | 10 | 0.3 | `HIDX-CELL` 60.0% |
| `E-KEY` | 9 | 0.3 | 88 | 3.3 | `E-NODE-KEYS=` 55.5%, `E-NODE-HASH` 44.4% |
| `@` | 8 | 0.3 | 8 | 0.3 | the profile tool's own counter |
| `RVT` | 8 | 0.3 | 38 | 1.4 | `RV@` 100% |

### Cut 1: the store guard's band walk

`src/habu/habu1.f` `ENGINE-EMIT:GUARD-SPAN` now reads its eight protected bands
from one table, takes the hull `[BAND-LO, BAND-HI)` from that same table, and
emits two compares in front of the walk: a span that starts at or above the
hull's end, or ends at or below its start, intersects no band and skips all
eight tests. `BAND-HI` is `DATA-START`, so every address the DP heap can reach
takes that exit — which is every variable the checker stores to. The rejection
set does not move: the hoist uses `GUARD-BAND`'s own two comparison forms
against the hull, the dynamic blob span still runs on every store, and
`test/protection-span.f` `TEST-HULL-EDGE` pins both hull edges a byte and a cell
either side, a span ending exactly at `BAND-LO`, a span starting exactly at
`BAND-HI`, a span wholly below the hull, and a span straddling all of it. All
nine answer identically on the engine before and after.
`bootstrap/cg/forth.fs` carries the same table and hull for its five-band
stage0 mirror, and its `PROT-GUARD` walks those rows too instead of keeping a
second copy of the list.

### Cut 2: two per-call full-table clears

`!`'s two largest callers after cut 1 were table clears, 11.5 and 7.8 percent of
its exclusive samples:

- `E-I-AK-RESET` (`src/core/checker.f`) UNBOUND-filled all 64 cells of `EI-AK`
  on every `E-INST-RESET`, which runs once per instantiated effect — once per
  checked call site — against the handful of fresh atoms a signature mints,
  usually none.
- `SEEN-RESET` (`src/core/render.f`) UNBOUND-filled all `MAXTV` = 1,280 cells on
  every entry into the renderer, which is per rendered term, against the handful
  of variables one diagnostic names.

Both now clear only the written span, in the high-water shape
`E-COPY-MAPS-RESET` beside them already used: the writer bumps a mark, the reset
clears `[0, mark)` and zeroes it. The invariant the mark needs — every cell above
it is already UNBOUND — is established by a one-time full fill at load
(`EI-AK`, which `allot` left as zeros) and re-opened wherever the array can stop
being UNBOUND: `SEEN-ENSURE` after a grow and `SEEN-SNAPSHOT-RESET` after it
zeroes the boot array at the capture seam. Diagnostics are unchanged: three
rendered multi-variable rejections produce byte-identical text before and after.

### What both cuts are worth

Five interleaved runs of each engine on the same core (`taskset -c 8`), medians,
1-minute load average 1.0-2.2 for the compile and run-time rows and 5.1-9.6 for
the self-build:

| | before | after cut 1 | after cut 2 | total |
|---|---:|---:|---:|---:|
| corpus, per definition | 82.2 µs | 69.6 µs | 66.0 µs | **-19.7%** |
| trivial, per definition | 32.8 µs | 28.6 µs | 27.9 µs | **-15.0%** |
| self-build, user CPU | 97.70 s | 93.45 s | 91.88 s | **-6.0%** |
| `(PROT-SPAN)` excl, corpus | 33.3% | 15.2% | 14.3% | |
| `!` inclusive, corpus | 38.0% | 22.2% | 20.1% | |

The store guard is on every store in the engine, not only the compiler's, so
run time moves with cut 1. `tools/tier-bench.f` at tier 0, two interleaved runs
of each engine, medians in µs:

| benchmark | before | after cut 1 | after cut 2 |
|---|---:|---:|---:|
| `harness` (a `SINK !` loop, nothing else) | 5,561 | 3,420 | 3,411 |
| `lines` | 22,231 | 18,364 | 18,270 |
| `move` | 375,654 | 341,433 | 339,844 |
| `fold` | 352,111 | 350,512 | 350,157 |
| `arith` | 8,075 | 8,071 | 8,072 |
| `branch` | 11,733 | 11,728 | 11,701 |
| `search` | 10,063 | 10,069 | 10,028 |

The three that move are the three that store; `arith`, `branch` and `search`
read and never write and sit inside the noise. Cut 2 is checker-only and, as
expected, moves no run-time row.

Both cuts reach a byte fixpoint one generation out, which is the ordinary shape
here: the AOT capture bakes the building engine's own DATA window, so the first
engine built by an engine that does not yet carry the change differs from the
one built by an engine that does. Cut 1: `A` built `B`, `B` built `C`,
`B == C`. Cut 2: `D` built `E`, `E` built `F`, `E == F`, and every engine
without the change builds the same `D` while every engine with it builds the
same `E` — checked across three builders and two rounds each.

The image grows 5,374,144 to 5,439,680 bytes. The content grows about 900 bytes
(`aot/code-blob` +332, `aot/data-run-bytes` +168, the rest smaller); the other
64 KiB is `image/text-pad` crossing a page boundary, and the next 64 KiB of work
absorbs it. `source/baked` is 0 in both: a native engine bakes no source.

### What is left on the tier-0 path, and why

Re-profiled on the engine carrying both cuts, corpus workload, 2,430 samples,
`words` 1,993 / `other` 431. Exclusive shares are exact; every per-token or
per-word repetition the audit named is listed with its measured share and its
disposition.

| what | measured, corpus | disposition |
|---|---:|---|
| eight-band scan per store | 33.3% → 14.3% | **cut** (above); what remains is the friend latch, the wrap test, the two hull compares and the dynamic blob span, all of which have to run |
| `ptr-field` + the two `;does` bodies | 9.5% excl | **left**: `0 ptr-field` is the identity on the address, so every read of a pointer-valued global pays a literal push and two calls for a type. Removing it means an immediate-fold row in the JIT's keyword and `VOPI-ENTRY` machinery (`src/habu/habu2.f`, `src/habu/jit.f`) — the files hazel-word-frame is changing — so it needs coordination, not a race |
| `CORE-STR=` + `CORE-STR=CI` ladders | 4.6% excl / 11.1% incl | **left**: `DO-TOK1` asks up to ~90 spelling questions per token across `LAYOUT-XPORT-TOK?` (18), `CF-TOK?` (~30), `RS-TOK?` (9), `UNSAFE-TOK?` (24), `RETIRED-TOK?` (7) and its own seven literals. The fix is one hashed lookup of the already-folded token into a shared keyword table and id comparisons after it; that is a ~400-line mechanical edit in the checker for a measured 5-7%, and it wants its own dot and its own review |
| `TAG`, `RES-FALSE`, `PAY`, `cell-view`, `PE-ROW` | 2.2, 2.0, 0.7, 1.3, 1.1% excl | **left, justified**: these are calls that push a constant or read one cell. They are tier-0 call overhead, not a copy, an allocation or a scan, and the fix for all of them at once is inlining small leaf prims in the JIT |
| `SYM-FOLD-C` re-folding stored names | 1.3% excl | **left, justified**: `SYM-PKG!`/`SYM-NAME!` store through `SYM-COPY-FOLD`, so `SYM-STR=CI` and `HIDX-H+` fold a side that is already folded. Real repeated work, but 1.8% and it needs the "every writer folds" invariant proved across the store before a fold can be dropped |
| `PRIM-FIRST-SCAN` | 1.1% excl / 5.5% incl | **left, justified**: entered only from `PRIM-FIRST-IDX`; a linear scan by name, but under 1.5% exclusive after cut 1 |
| `E-COPY*` / `E-INTERN` signature copy | 12.6% incl on the trivial word, below the corpus report's 64 rows | **left**: it dominates a definition with no body, which is the boot prefix's shape, and it is the next thing to measure after the two above |
| `VREC-COPY` | did not reach 64 rows at either workload | **left, justified**: not on the measured hot path for either workload |

Nothing on this list allocates per token: the checker's tables are sized once
and grown only on demand, and cut 2 removed the two clears that paid for a
table's capacity rather than its contents. `VREC-COPY-RESET`
(`src/core/checker.f`) still clears two whole `MAXTV` arrays per call and is the
same defect in a third place, but it never reached the 64 reported rows at
either workload — value-record parsing is not on this path — so it is listed
here rather than cut.

## 8. Where a tier-1 compile's time goes

**How this was measured, and how it was not.** The engine's sampling profiler
prints the top 24 rows by *exclusive* count, so it cannot attribute inclusive
time to a named phase: every phase word below has almost no exclusive time and
never reaches a row. The table therefore comes from the chain's own stopwatch
(`src/compiler/native/prof.f`), temporarily extended with one phase per stage,
measured, and reverted — the tree carries only the two phases a released engine
needs. The ablation table under it comes from engines built with one phase
disabled and differenced; both are stated where they are used.

Per-phase **inclusive** microseconds per definition, 1,000 definitions per body,
engine built from this lane's head, pinned to cpu8 at load average 5.8:

| phase | `( n -- n ) 1 + ;` | `( n n -- n ) swap drop ;` | `( n n -- n ) over + swap 3 and + ;` |
|---|---:|---:|---:|
| elaborate — the HIR build | 61.8 | 57.4 | 168.3 |
| HIR freeze — derive the edge table | 1.8 | 1.8 | 1.8 |
| selection, with its own freeze and that module's whole-module verification | 116.3 | 90.5 | 163.6 |
| register allocation | 53.9 | 46.9 | 75.2 |
| the allocation validator | 68.2 | 60.6 | 90.1 |
| the prune scan | 5.4 | 4.5 | 7.6 |
| the spill rewrite | 0 | 0 | 0 |
| emit | 25.4 | 20.1 | 33.4 |
| publish | 7.2 | 7.2 | 7.6 |
| **the chain, selection through publish** | **351.9** | **305.8** | **453.5** |

The chain's own row is not the sum of the rows inside it: 73.7, 74.2 and 74.2
microseconds of it are not in any phase. That residue is nearly constant in the
size of the body because it is per-word setup — creating the machine builder,
binding five dialects to it, the counted-loop scan, and retiring the HIR module —
and it is arena and registry work, which is where a different lane is looking.
`elaborate` stands outside the chain's row: it runs before it. Neither covers the
checker, the tape or the feed, which is the rest of the roughly 520 microseconds
`tools/compile-floor.f` reports for the trivial definition.

**What each removable phase cost, by ablation.** Each row is an engine built from
this lane's base commit with one thing disabled, differenced against the base
engine on the same box in the same minute:

| disabled | trivial | three-op | how |
|---|---:|---:|---:|
| the whole-module verifier's checks, on all three of a word's modules | −80 µs | −27 µs | `IR-VERIFY:VERIFY` reduced to its edge derivation |
| every SHA-256 of the word's source text (5 for the trivial body, 3 for the three-op one) | −14.2 µs | −11.1 µs | `CDIGEST:COMPUTE` stubbed |
| the combine pass, scan and rebuild both | −163 µs | −11 µs | `NCOMP:COMBINED` reduced to a pass-through |

One SHA-256 of a twenty-byte body costs about 49,000 instructions in this engine;
the rebuild of a two-operation module costs about 152 microseconds and the scan
that decides whether to rebuild about 11.

**What this lane then removed.** Instruction counts, 3,000 definitions per body,
`perf stat -e instructions:u`, which is deterministic here to one part in 10^9:

| commit | trivial | three-op | corpus |
|---|---:|---:|---:|
| verify a word's IR once, on the emitted module | −2.14 % | −1.10 % | −2.94 % |
| digest a word's source once, not once per module | −1.75 % | −1.35 % | −0.47 % |
| fold a producer into its reader during selection | −23.2 % | +0.33 % | −9.58 % |
| retire the combine pass down to the load it prunes | −2.66 % | −2.01 % | −2.08 % |
| **together** | **−28.2 %** | **−4.1 %** | **−14.5 %** |

In CPU time per definition: trivial 726.8 → 523.6 µs, three-op 504.3 → 488.2 µs,
and the 1,772-word corpus 4.483 → 3.880 s. The emitted code is not what paid for
it: the corpus census reports the same 1,772 words in 176,268 bytes against
176,276 before, the one word that moved having lost two instructions, and the
byte fixpoint converges at the second generation with the third build identical
to the second.

**Reproducing.** Build an engine from the tree under test, then, pinned and with
the load quoted:

```sh
E=/tmp/hz/engine
taskset -c 8 perf stat -e instructions:u,task-clock $E --load tools/compile-floor.f
taskset -c 8 $E --load tools/tier-census.f -- 1 /tmp/c.txt lib/byte-edit.f \
  lib/array.f lib/fmt.f lib/float.f lib/json-read.f lib/json-write.f \
  lib/unicode.f lib/argv.f lib/fs.f lib/task.f lib/build.f tools/lint/text.f \
  tools/public-signatures-core.f
```

The per-phase table needs the stopwatch extended again: one `NPROF:phase` per
stage, `NPROF:START`/`NPROF:STOP` around each call in `src/compiler/native/`
`compiler.f`, and a driver that opens a session, compiles a set and reads
`NPROF:NS@`. Take it, then take it back out: a phase per stage is a profiler, and
what a caller of the compiler cares about is already one number.

## 9. The pointer move a transfer carries

Tier 0 already pushed a cell with one instruction — `str x9,[x19],#8` writes the
pointer back as part of the store — and after the word-frame lane tier 1 did the
same for a routine's link register. It still spent two instructions on every
data-stack move: `str Xt,[x19]` then `add x19,x19,#8` to publish, and
`sub x19,x19,#8` then `ldr Xt,[x19]` to take. A linear disassembly of the engine
image found **11,270 such adjacent pairs**, 7,792 publishing and 3,478 taking.

The fix is in SELECTION and not in a rewrite pass, because the pair is born
there: `src/compiler/native/select.f` writes the store and the publish itself,
so it can write one operation instead of two. `a64.dpush`/`a64.dpop` and their
two twins in the D file are new A64IR forms carrying one attribute, `a64.dwb`,
and no slot at all — the post-indexed store transfers AT the base and the
pre-indexed load lands there, so the cell is the one the pointer stands at and
the form has no field for another. Selection fuses exactly where the transfer
reaches that cell: the last store of a publish run, the first load of a
take-back run, at the routine's two ends and at every call and trap site.

Two passes had to learn the forms rather than being told about them.
`regalloc-verify.f` re-derives the whole data-stack discipline from the module,
so every move it reads is now the operation's own attribute plus whatever the
transfer beside it carries; and `prune.f`, which removes a data-stack load
nothing reads, hands that load's move back to the operation in front of it
before dropping it — a fused form has nowhere to keep a move it does not
transfer for. `: W ( -- n ) SRC 0 > if 5 else 5 then ;` is the smallest body
that needs it, and without the hand-back it does not compile at all
(`E-A64RAV-DKEEP`); `test/compiler/native-fused-moves.f` pins it.

**What it removed.** The 1,967-word corpus of section 1, compiled at tier 1 by
an engine built from this lane's head and by one built from its base:

| metric | before | after | delta |
|---|---:|---:|---:|
| bytes | 179,012 | 173,688 | **−5,324 (−2.97%)** |
| instructions | 44,753 | 43,422 | **−1,331** |
| `bl` | 4,492 | 4,492 | 0 |
| `ldr` via sp | 4,117 | 4,117 | 0 |
| `str` via sp | 2,557 | 2,557 | 0 |
| `mov` reg→reg | 129 | 129 | 0 |
| `movk` | 5,953 | 5,953 | 0 |

Every removed byte is a removed instruction: 1,331 × 4 = 5,324 exactly, and no
word in the corpus grew. The same corpus at tier 0 is byte-identical across the
two engines, which is the control that says nothing but the optimizing tier
moved.

**Why the `sp` columns do not move, and what to read instead.**
`tools/tier-census.f`'s `ldr-sp`/`str-sp` columns count loads and stores whose
BASE REGISTER is x31 — the frame traffic a spill costs. The data stack is x19,
which those columns do not count and never did, so a data-stack fusion cannot
show up in them. The columns that answer for this change are `bytes` and
`instructions`; the pair count is their difference divided by four.

**The engine image.** `tools/engine-size.f` over a generation built with the new
compiler: `aot/code-blob` 1,790,872 → **1,757,748 bytes, −33,124**, with the
total image unchanged at 5,308,608 because `image/text-pad` absorbs it
(20,932 → 40,488). The image's own pair count falls from 11,270 to **three**,
and all three are at 0xac4c, 0xac54 and 0x1fc1c — inside the 126,976-byte
`engine/code` section, which `src/habu/habu1.f` emits by hand and no compiler
tier writes. 11,270 pairs are 45,080 bytes; the blob gave back 33,124 of them
because the same image now carries the four new forms and everything the chain
grew to emit, to check and to prune them.

**Run time,** `tools/tier-bench.f` at tier 1, the two engines interleaved over
fifteen invocations each, pinned to cpu8 at load average 10.9-13.3, median of
the fifteen per-invocation medians in microseconds:

| benchmark | before | after | after / before |
|---|---:|---:|---:|
| `harness` | 2,770 | 2,776 | 1.002 |
| `arith` | 860 | 863 | 1.003 |
| `branch` | 1,456 | 1,475 | 1.013 |
| `search` | 2,107 | **1,895** | **0.899** |
| `fold` | 80,768 | 80,574 | 0.998 |
| `move` | 134,517 | 134,450 | 1.000 |
| `lines` | 5,935 | 5,967 | 1.005 |

One benchmark gains 1.11x and the rest sit inside the tool's own variance, which
is what a change that removes one instruction per data-stack move and nothing
else should look like. The census says which is which: `search` runs
`LINT-CONTAINS?`, 112 → 108 bytes, one instruction out of a loop that crosses
the data stack once per byte, while `branch` runs `ARRAY:A-COUNT-EVEN`, whose
176 bytes are **byte-identical** across the two engines — so its 1.3 percent is
the measurement and not the code, as a quieter nine-round run at load 5.0
(1.001) says as well.

**What it did not reach, and why.** Where the pointer STANDS is chosen by
`select.f` `DPLACE-CHOOSE`, which counts one instruction for every required
place that is not the chosen one — a cost model written before a move could be
free. `( -- n n )` is the smallest case it now gets wrong: standing at 0 costs
one move and standing at 8 costs two, so it stands at 0, the last store lands a
cell under the pointer, and the publish keeps its `add x19,x19,#0x10`. Standing
at 8 would have cost two moves and fused one of them, for the same instruction
count today and a shorter body. Teaching the placement that a move beside a
transfer at the pointer is free is a separate change, and it has to be made in
`regalloc-verify.f` `VDPLACE-CK` at the same time, because that pass re-derives
the placement from the module.

**Reproducing.** Build an engine from the tree under test, then build a second
one WITH it — a compiler change reaches the emitted code only in the generation
its own compiler wrote, so generation B (built by the old engine) still carries
the old code and generation C is the first that does not. C and D are then byte
identical, which is this chain's fixpoint.

```sh
E=/tmp/hz-fuse
HB_TMP=$E/tmp HABU_FIXPOINT_ENGINE=$E/engine-A $E/engine-A \
  --load tools/native-build.f -- $E/gen/hb-B
HB_TMP=$E/tmp HABU_FIXPOINT_ENGINE=$E/engine-B $E/engine-B \
  --load tools/native-build.f -- $E/gen/hb-C
objdump -b binary -m aarch64 -D $E/gen/hb-C | grep -c 'str.*\[x19\], #'
```

## 10. What a pointer definer's read costs

Measured 2026-09-17 on the same machine, against a993db02, which already carries
the offset handles of section 6. Engines were built with the integrated host in
`/tmp/hazel-host-R2`, each build in a private `HB_TMP` with a private
`HABU_FIXPOINT_ENGINE`. Every instruction count is `perf stat -e instructions:u`
pinned to cpu8 or cpu10, the section 6 denominator: a `tools/tier-census.f` run
over 500 `: PVn ( n -- n ) 1 + ;` definitions minus one over a single
definition, divided by 499. Wall-clock rows say so and quote the load average,
which ran 17 to 22 throughout.

**Read generation 2 and not generation 1.** A compiler change reaches the
emitted code only in the generation its own compiler wrote, and both changes
here are compiler changes, so the engine a host without them builds still pays
the old price. Generation 1 is quoted once, below, precisely because it prices
the old path.

### What the cost actually was

Not `ptr-field`. `PTR-VARIABLE` and `PERSISTED-PTR-VARIABLE` were
`create ... does> ( -- ptr ptr a ) 0 ptr-field ;`, and section 7 measured
`ptr-field` plus the two clause bodies at 9.5 percent exclusive. By a993db02 the
`ptr-field` row had already gone: the engine's own clause bodies are compiled at
tier 1, where `ptr-field` is arithmetic this dialect expands inline
(`src/compiler/native/hir-word.f` CELL-INDEX), so the call was already gone and
the clause body had become

```
str  x30,[sp,#-16]!        the clause's frame
ldur x0,[x19,#-8]          the address create just pushed
mov  x1,#0                 the literal 0
mov  x2,#8                 CELL
madd x0,x1,x2,x0           x0 = x0 + 0*8, three instructions to compute nothing
stur x0,[x19,#-8]
ldr  x30,[sp],#16
ret
```

while the created word itself materialized its data address in four
`movz`/`movk`, pushed it, and `b`-ed into that. So the whole remaining cost of a
read was the **does> dispatch**: a call, a branch, a frame, and an identity.
`tools/tier0-profile.f -- trivial 50 4000` on a993db02 put
`PERSISTED-PTR-VARIABLE;does` at 81 of 2,348 samples exclusive (3.4 percent) and
`PTR-VARIABLE;does` at 24 (1.0 percent), with `ptr-field` nowhere in the 64
reported rows.

That is why the offset-handle lane held the arena's region base in a bare
`create`d cell marked with `ptr-cell-mark` instead, and why section 6's table
shows that exception buying 12.1 points.

### What changed

A clause that compiles no instruction runs after the created word has pushed its
data address and then does nothing to it, so it is a request for a type and not
for a behaviour. Both compilers now leave such a word alone: it keeps the RET
`create` emitted and the `DKIND:ADDR` stamp both compilers fold a mention
through. Tier 0 writes `movz x10,#0` over the `adr x10, D` its own opener
emitted, keyed on CP still standing one word past the clause entry slot at `;`
— the compiler's cursor, not a guess about the emitted code — and tier 1 stages a
zero clause entry into the `does-patch` call when the clause holds no token.
`bootstrap/cg/forth.fs` carries the same two changes.

The definers then drop the `0 ptr-field` they were spelling, which was the
identity on that address, and the arena's exception reverts to
`PERSISTED-PTR-VARIABLE`.

### What a read compiles to now

At tier 1, `FOO @` for a `PERSISTED-PTR-VARIABLE FOO`, in a word that calls
nothing else, from `tools/tier-dump.f`:

| | before | after |
|---|---|---|
| the reading word | `str x30,[sp,#-16]!` / `bl FOO` / `ldr x0,[x19,#-8]!` / `ldr x0,[x0]` / `str x0,[x19],#8` / `ldr x30,[sp],#16` / `ret` | `movz`+3 `movk` / `ldr x0,[x0]` / `str x0,[x19],#8` / `ret` |
| `FOO` | 4 address words, push, `b FOO;does` | 4 address words, push, `ret` |
| `FOO;does` | the eight instructions above | never entered |

The reading word is a leaf now: no call, no frame, and the fold is the same
relocatable `movz`/`movk` stencil a `create`d word's mention has always
produced. The bare marked cell the arena used spells `RBASE-CELL 0 ptr-field @`,
which is that stencil **plus** the `mov`/`mov`/`madd`, so the definer's read is
three instructions shorter than the exception that replaced it. At tier 0
neither form folds and both are one `bl`; what goes is the clause's branch and
frame.

### Per trivial tier-1 definition

Second-generation engines, cpu8, 499-definition difference:

| engine | per definition | against the base |
|---|---:|---:|
| a993db02, the base | 4,937,759 | — |
| the empty-clause elision + the definers | 4,916,373 | -0.43% |
| + the arena's base back on the definer | **4,819,662** | **-2.39%** |

Over the thirteen-file corpus of section 1, cpu10: 53,788,375,093 on the base
against **52,602,908,828**, **-2.20 percent**.

**The generation-1 row, which prices the old dispatch.** The same final tree
built once by the integrated host — an engine with no elision, so its
compilation of the arena's reads still pays the full does> dispatch — is
5,308,192 per definition, **+7.50 percent** against the base. That is the cost
the arena's exception existed to avoid, measured again from the other direction,
and it is what generation 2 removes.

### The profile rows

`tools/tier0-profile.f -- trivial 50 4000`, taskset cpu8, load average 17-20:

| row | base, 2,348 samples | landed, 2,250 samples |
|---|---:|---:|
| `PERSISTED-PTR-VARIABLE;does` excl | 81 (3.4%) | absent from the whole report |
| `PTR-VARIABLE;does` excl | 24 (1.0%) | absent from the whole report |
| `ptr-field` excl | outside the 64 rows | outside the 64 rows |

### Size, run time and the fixpoint

`tools/tier-census.f` over the same thirteen-file corpus, 2,133 words:

| | base | landed |
|---|---:|---:|
| tier 0, bytes | 157,836 | 157,836 (md5 identical) |
| tier 1, bytes | 188,200 | 188,368 (**+168, +0.089%**) |
| tier 1, `bl` | 4,973 | 4,952 (-21) |
| tier 1, `movk` | 6,184 | 6,247 (+63) |

**The tier-1 census grows, and this is the growth it should have.** Ten words
change — eight in `lib/ffi-abi.f`, one in `lib/genio.f`, one in `lib/task.f` —
and every one of them trades `bl` for `movk` at exactly three `movk` per call
removed. They are readers of buffers minted by `lib/codegen.f BUFFER` and of
`lib/task.f`'s `TASK`, definers that already wrote an empty clause and needed no
source change: 21 such reads stopped being calls and became inline address
literals, at +8 bytes each. That is the four-instruction address stencil ranked
second in this document's own fix list, not a new cause, and it is the price of
the -2.39 percent above. The lane was asked for bytes at or below the head's;
this is 168 above, and the cause is named rather than argued away.

`tools/tier-bench.f` at tier 1, `src/core/checker.f`, five interleaved runs of
each engine on cpu8 at load average 17-22, medians in µs: `harness` 2,765 /
2,774, `arith` 866 / 860, `branch` 1,445 / 1,469, `search` 1,902 / 1,905,
`fold` 81,282 / 81,417, `move` 139,660 / 138,990, `lines` 6,014 / 5,978. Every
row moves less than its own run-to-run spread and the two largest change
direction between rounds, so this reads as no change: none of these benchmarks
reads a pointer-valued global. The tier-0 rows are not reportable at this load —
one `move` sample came back at twice its median.

Two-generation byte fixpoint: the landed tree's generation 2 and generation 3
are byte identical, sha256 `7fbe0e29416bf201…`.

`bootstrap/cg/forth.fs` carries the same elision, and the seed's copy is the one
that compiles `src/core/pointer-storage.f` when there is no binary at all, so it
was run rather than argued for: the periodic no-binary check
(`HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh`) exits 0
on this tree with `bootstrap check OK`, through stage0, the stage engine's own
fixpoint, `hb-stdin-mk` and `hb-stdin`. That is further than the status note at
the head of `docs/bootstrap.md` describes, which is stale rather than wrong
about this change.

### What is left

The elision is keyed on a clause compiling nothing, so the four definers in the
tree that already wrote an empty clause — `lib/string.f BUFFER:`,
`lib/codegen.f BUFFER-E`, and `lib/task.f`'s `TASK` and `FACILITY` — get it
without a source change, which is where the census movement comes from. A clause
with a body still pays the dispatch, including one whose body
is the identity, which is what `test/does-empty-clause.f` pins. The empty
clause's own record and its three-instruction body are still published; nothing
branches into them, and removing them would mean unwinding CP, the derived name
and the frame cell at `;` in both compilers for twelve bytes and one dictionary
slot per definer.

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

taskset -c 8 $E --load tools/tier0-profile.f -- corpus 50 $C
taskset -c 8 $E --load tools/tier0-profile.f -- trivial 50 4000
taskset -c 8 $E --load tools/tier0-profile.f -- trivial-raw 50 4000
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
- Sections 1 to 4 measure the two compilers, not the checker. Section 5 measures
  the checker, and finds it owns tier-0 compile time; it in turn cannot separate
  emit from tokenize from publish, because all three are unregistered engine text
  that the profiler can only bound together as `other`.
