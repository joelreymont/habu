# Codegen parity — the chain against clang, and against itself

The code generator comparison measures three columns over four pinned corpora,
and reads the chain against two references.

| column | what it is | where its bytes come from |
|---|---|---|
| `old` | the emitter `bin/hb` uses today | the word's own dictionary record |
| `new` | the native chain being built | the word's own dictionary record |
| `clang` | `clang -O2` over a C twin of the same program | the reference object's symbol sizes |

* **First reference — clang.** The parity target. Measured live on every run,
  because what a host's toolchain emits is a fact about that host and pinning it
  would turn a compiler upgrade into a red gate. A gap is *informational*: it is
  the priority list, not a gate.
* **Second reference — the chain's own committed baseline.** Says whether the
  chain got better or worse since anybody last looked. A byte count that **grew**
  is a finding and the run exits non-zero; one that **shrank** is named so the
  baseline gets re-pinned deliberately.

Every optimisation lane reports both: how much of the clang gap it closed, and
how much it gained on our own baseline.

## Running it

```
bin/hb --load tools/codegen-compare.f
    Measure every corpus, print a report for each, compare against both
    committed tables, and print the cross-corpus ranking of the largest
    chain-vs-clang gaps. Timed; run it by hand on a quiet machine.

bin/hb --load tools/codegen-compare.f -- --update <corpus>|all
    Rewrite the ENGINE's committed table for that corpus.

bin/hb --load tools/codegen-compare.f -- --update-chain <corpus>|all
    Rewrite the CHAIN's committed table for that corpus. This is how an
    improvement is re-pinned. It is deliberately not a side effect of
    --update, which would erase the number that had just said the chain
    got smaller.
```

What the gates run is `tools/codegen-compare-test.f` and
`tools/codegen-compare-clang-test.f`, neither of which reads a clock.

## The files

| file | concern |
|---|---|
| `tools/clang/twins.c` | one C twin per corpus row, plus one empty function per call shape |
| `tools/codegen-compare-cc.f` | running clang, nm and size; deciding whether there is a third column at all |
| `tools/codegen-compare-macho.f` | per-symbol code sizes out of the reference object |
| `tools/codegen-compare-text.f` | the line and word cursor both readers share |
| `tools/codegen-compare-cabi.f` | the fourteen foreign-call shapes, and the only trusted boundary added |
| `tools/codegen-compare-clang.f` | one C twin measured as one row |
| `tools/codegen-compare-c1.f` … `-c4.f` | which twin stands for which row, on which pinned inputs |
| `tools/codegen-compare-gaps.f` | the cross-corpus ranking of the largest gaps |
| `tools/codegen-compare-ns.f` | a picosecond count written as nanoseconds |
| `test/compiler/codegen-chain-baseline*.txt` | the chain's committed tables |

## Five things to know before reading a number

**1. The reference's entry floor is per row, not per column.** A twin is reached
through an FFI call, which costs several times what a `bl` costs — about 7 ns for
an empty zero-argument call and about 22 ns for a three-argument one, against
about 2 ns for an empty habu call. So each reference row records a floor of its
own: the row's *own* timing body, run again with the twin replaced by
`tools/clang/twins.c`'s empty function of the *same signature*. What the
subtraction leaves is the emitted code, because the two runs marshal identical
arguments through identical stores into identical registers and differ only in
what the callee does with them. A single zero-argument floor would have left
every row carrying the marshalling of its own arity, and rows like `WIDE-ARITY`
would have read as forty nanoseconds of C.

**2. Sub-nanosecond bodies are at the resolution of the measurement.** A row
whose C body is two instructions can come out slightly *faster* than its own
floor. That is printed as a negative number rather than clamped to zero,
precisely so a reader can see which rows not to trust.

**3. Byte counts are `__text` only.** Clang writes literal pools —
`__literal8`, `__literal16`, `__const` — that cannot be attributed to any one
symbol, because a pool entry is shared by whichever functions need that constant.
The report prints the pool total beside the table so the remainder is visible
rather than quietly missing. Both habu columns have no pools at all: they
materialise a constant with move-wide instructions inside the routine, so a
row's byte count there is the whole cost.

**4. The two rankings have different stabilities, and the report says so.**

* **By bytes: exact, and identical on every run.** Measured: ten consecutive
  runs produced the same ordering, byte for byte. Ties fall straight through to
  measurement order; nothing about the byte table depends on a clock. (An
  earlier version broke byte ties with the *time* gap, and one run in ten came
  out in a different order on a column where every number is exact. That is why
  the tie-break is what it is.)
* **By time: stable at whole nanoseconds, and not below.** The key is the gap
  truncated to whole nanoseconds — the resolution the table prints — with ties
  broken by the byte gap (exact) and then by measurement order (fixed). The top
  of the table is stable run to run; rows whose gaps sit within a nanosecond of
  each other do trade places when one of them crosses a nanosecond boundary, and
  they are not being claimed to differ. Anyone reading position 7 against
  position 8 is reading noise.

**5. The reference column is Mach-O only, and absent loudly otherwise.** The
byte column comes out of `nm -m` and `size -m`, so a non-Mach-O host has two
columns and one printed line saying why. A missing `clang` is told from a
`clang` that refused by the completion code: POSIX reserves 127 for "command not
found", so 127 means absent and any other nonzero code means the reference does
not build — which throws, with the tool's own diagnosis printed.

## What is not done to clang

The build passes `-O2 -arch arm64 -fno-math-errno` and nothing else that changes
code generation.

* `-fno-math-errno` so a square root is the `fsqrt` instruction the habu word is,
  rather than a libm call with an `errno` test around it. Without it every float
  row would be measuring a call into a shared library.
* **Not** `-ffast-math`. Every recorded double is compared bit for bit against
  the same pins the other two columns are held to, and reassociating a sum
  changes them. A reference that answered differently would not be a reference.

No twin is hand-tuned and no inlining is forced or forbidden. Where clang
recognises a closed form for a loop, or folds a dead store the engine keeps, it
is allowed to: that difference **is** the measurement. The twins own their own
copies of the pinned data — they are a different program, not a second
compilation of the same one — and what makes that honest is not the memory but
the answers, which are compared against the same pins.

## Where the twins take a signed-overflow liberty

Two twins do in C what the habu words do on the machine, and C calls it
undefined:

* `hc3_fround` converts an infinity to an integer. On arm64 that is `fcvtzs`,
  which saturates, and the pinned answer `9223372036854775807` is the engine's
  own. A NaN converts to 0 on both.
* `hc1_fact`, `hc4_call_pressure` and `hc4_big_consts` multiply and shift signed
  values. Every pinned input is small enough that no product overflows; the
  answers are compared against the engine's, so a compiler that took a different
  liberty would be reported as a wrong answer rather than absorbed.
