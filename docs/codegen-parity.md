# Codegen parity — the chain against clang, and against itself

The code generator **judge** reads five pinned corpora, compiles every subject
through both habu code generators from **one text**, measures a C twin of the
same program beside them, and writes one artifact.

| column | what it is | where its bytes come from |
|---|---|---|
| `old` | the emitter `bin/hb` uses today | the word's own dictionary record |
| `chain` | the native chain being built | the derived word's own dictionary record |
| `clang` | `clang -O2` over a C twin of the same program | the reference object's symbol sizes |

The `clang` column is **printed below the artifact's marker line and is not
compared**; the two habu columns and the two data-stack columns are. Note 4
below says why.

`ds-old` and `ds-new` sit beside them: how often each habu column's emitted code
touches the caller's own data stack. That is the structural difference between
the two generators — the engine moves every intermediate through a slot in
memory, the chain holds it in a register — and unlike a cost it is exact and
does not turn on host load.

* **First reference — clang.** The parity target. Measured live on every run,
  because what a host's toolchain emits is a fact about that host. A gap is
  *informational*: it is the priority list, not a gate — and because it is a
  fact about a host rather than about habu, it sits below the marker line with
  the costs, outside the bytes the check compares.
* **Second reference — the chain against itself.** `test/compiler/judge-baseline.txt`
  is what this tree emits. The check is byte for byte over the half habu's own
  chain determines, so anything that moved has to be re-pinned deliberately; on a disagreement the run also **names each
  row and which way it went** — bigger is a regression against ourselves,
  smaller is progress, the engine's own column moving either way is a finding.

## Running it

```
bin/hb --load tools/judge.f
    Judge every corpus and print the artifact on stdout. This is how the
    committed file is regenerated:

      bin/hb --load tools/judge.f > test/compiler/judge-baseline.txt

bin/hb --load tools/judge.f -- --check
    Judge every corpus and compare what this tree produces with the committed
    artifact, byte for byte. Names the line they first differ on, adjudicates
    each moved column, and exits non-zero.

bin/hb --load tools/judge-timed.f
    THE COST CLAIM: the chain's code is not slower than the engine's, row by
    row. Run BY HAND on a quiet machine. No suite schedules it.

bin/hb --load tools/judge-fuzz.f
    The differential oracle's full sweep: straight-line integer programs from a
    constant seed, one text through both generators, same cell required back.
```

What the gates run is `tools/judge-test.f`, `tools/judge/base-test.f`,
`tools/judge/ref-test.f`, `tools/judge/src-test.f` and
`tools/judge-fuzz-test.f`, none of which reads a clock.

## The files

| file | concern |
|---|---|
| `tools/judge.f` | the command line entry, and the artifact |
| `tools/judge/src.f` | reading a corpus source file as the definitions both generators compile |
| `tools/judge/chain.f` | compiling one of them through the chain, or recording the refusal |
| `tools/judge/pass.f` | one stated row into four passes; the pinned inputs a row is valued on |
| `tools/judge/cost.f` | generating, certifying and timing one row's program in one column |
| `tools/judge/traffic.f` | the caller's data stack, counted in emitted code |
| `tools/judge/row.f` | the judged table, its verdicts, and the cost direction |
| `tools/judge/report.f` | the artifact as text |
| `tools/judge/base.f` | reading a committed artifact back as rows, and which way each column moved |
| `tools/judge/check.f` | the run, and where it disagrees with what is committed |
| `tools/judge/fuzz.f` | the differential oracle |
| `tools/judge/corpus{1..5}.f` | which subject is judged, on which pinned inputs, against which twin |
| `tools/clang/twins.c` | one C twin per corpus row, plus one empty function per call shape |
| `tools/codegen-compare-cc.f` | running clang, nm and size; deciding whether there is a third column at all |
| `tools/codegen-compare-macho.f` | per-symbol code sizes out of the reference object |
| `tools/codegen-compare-text.f` | the line and word cursor the readers share |
| `tools/codegen-compare-cabi.f` | the fourteen foreign-call shapes, and the only trusted boundary added |
| `tools/codegen-compare-clang.f` | whether there is a reference column, and how big the object it came out of is |
| `tools/codegen-compare-corpus{,2..5}.f` | the pinned corpora themselves |
| `test/compiler/judge-baseline.txt` | what this tree emits |

The timing discipline and the projections a measured value crosses live in
`tools/judge/cost.f`, beside the generator that writes both into a body.

The `codegen-compare-` prefix on the last five rows is the old comparison
harness's, which was deleted; those files are the parts of it the judge kept,
and renaming them is cosmetic work of its own.

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
precisely so a reader can see which rows not to trust. The artifact also prints
the noise floor the run measured for itself — the widest gap between two
measurements of one program — and a difference between two columns smaller than
that is not a difference.

**3. Byte counts are `__text` only, and the remainder is printed.** Clang writes
literal pools — `__literal8`, `__literal16`, `__const` — that cannot be
attributed to any one symbol, because a pool entry is shared by whichever
functions need that constant. The artifact prints the whole `__text`, how much
of it the rows name, and the pool total, so what the per-twin column cannot
account for is visible rather than quietly missing. Both habu columns have no
pools at all: they materialise a constant with move-wide instructions inside the
routine, so a row's byte count there is the whole cost.

**4. Only what habu's own chain determines is compared; the rest is printed.** A
habu byte count and a data-stack access count are the same number on every host
in every run, so they are compared against the committed artifact exactly. Two
things are not. A cost is a measurement, and a machine with every core busy —
which is what a gate is — moves one by more than any honest tolerance would
catch. And a `clang` cell is a fact about a host's toolchain that no mutation of
habu can falsify, which is exactly what `docs/proofs.md` refuses to let stand
inside a gate; while one was among the compared bytes, the scheduled suite was
red on every host without the column for no habu change at all. So the
artifact's checked half ends at a marker line, and the reference column and the
costs are printed under it, with the spread the run measured. The one claim made
about the costs is a DIRECTION, in `tools/judge-timed.f`, against that measured
spread rather than a fixed tolerance.

What the reference still moves **above** the marker is an ANSWER, not a byte
count: a twin that disagreed with the engine on a pinned input is counted in
`columns disagreeing on the answer`, which is a finding rather than a fact about
a host. The tally there also reports how many pinned inputs the C twins carry a
program for — a number the corpora and `tools/clang/twins.c` state between them,
and so the same on every host — while how many the column actually reached this
run is printed below the marker, beside the reason there is none.

**5. The reference column is Mach-O only, and absent loudly otherwise.** The
byte column comes out of `nm -m` and `size -m`, so `tools/codegen-compare-cc.f`
`DECIDE` refuses the column outright when `HB-TARGET-MACOS?` is false. On Linux
there is therefore **no reference column by construction, not because `clang` is
missing**: the compiler can be installed and on `PATH` and the column is still
absent, because nothing here reads ELF. A non-Mach-O host has two columns and
one printed line saying why. A missing `clang` is told from a
`clang` that refused by the completion code: POSIX reserves 127 for "command not
found", so 127 means absent and any other nonzero code means the reference does
not build — which throws, with the tool's own diagnosis printed. The same holds
one input at a time: a row may state a pinned input over a buffer the C file
does not carry, and the tally counts how many inputs the reference reached so a
comparison never made cannot read as one made and passed.

## What is not done to clang

The build passes `-O2 -arch arm64 -fno-math-errno` and nothing else that changes
code generation.

* `-fno-math-errno` so a square root is the `fsqrt` instruction the habu word is,
  rather than a libm call with an `errno` test around it. Without it every float
  row would be measuring a call into a shared library.
* **Not** `-ffast-math`. Every recorded double is compared bit for bit against
  the same inputs the other two columns are held to, and reassociating a sum
  changes them. A reference that answered differently would not be a reference.

No twin is hand-tuned and no inlining is forced or forbidden. Where clang
recognises a closed form for a loop, or folds a dead store the engine keeps, it
is allowed to: that difference **is** the measurement. The twins own their own
copies of the pinned data — they are a different program, not a second
compilation of the same one — and what makes that honest is not the memory but
the answers, which are compared on every pinned input.

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
