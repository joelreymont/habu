# Debugging the self-hosted standalone

The standalone is JIT-compiled native ARM64; blind `.`-printing was the recurring
pain. Toolkit:

## `.s` — data-stack inspector (in the standalone)
`forth.fs` defines a `.s` primitive: prints the whole data stack (base..top), one
signed decimal per line, **non-destructively**. Interleave it to "step" through a
word and watch the stack — the practical stepper for standalone Forth. The loop
pointer lives in a DATA cell (`SSCR-CELL`) because the shared printer `g-print9`
clobbers x9..x15. The base is saved at startup into `S0-CELL`.
For build-time underflow, also probe with `depth .`: `.s` walks the saved
base-to-top range and can hide a negative depth after a native emitter corrupts
the build stack.

```
: GO 11 22 33 .s + + . ;   \ .s prints 11/22/33, then GO continues -> 66
```

## `.` — single value (in the standalone)
Pop + print one signed decimal + newline. Use for a specific intermediate.

## `BPW+` / `BPW-` / `BPW.` — watched cells
`src/habu/debug-watch.f` is baked into `bin/hb` before the stepper/debugger. It
publishes a small watch table used by both `step` and compiled-word breakpoints.
Add a cell address with `BPW+`, remove it with `BPW-`, clear all watches with
`BPW-CLEAR`, and list `address value` pairs with `BPW.`. For fixed engine cells:

```
DATAB ENVP-CELL + BPW+
DATAB ARGV-CELL + BPW+
```

## `step` — native token stepper (in the REPL, `bin/hb` on a tty)
`src/habu/stepper.f` is baked into `bin/hb`. `step 5 dup * 3 +` runs the rest
of the line one token at a time, echoing each token and printing the data stack
and watch table after it executes — no `EVALUATE` needed: the REPL hook feeds
the engine one token per call, so the engine's own interpret loop is the
evaluator.

## `BP+` / `BP-` — one-shot breakpoints on compiled words (REPL)
`src/habu/debug.f` (baked into `bin/hb`): `' WORD BP+` plants a `BRK #0` at the
word's entry. Hitting it prints `habu-bp:` + the pc + the data-stack top, then
`habu-bp-lr:` + the **interrupted thread's x30** — the address the word will
return to, which is what names its caller — then `habu-bp-stack:` with each live
data-stack cell and `habu-bp-watch:` with watched address/value pairs, then
restores the original instruction and **resumes** the word; the breakpoint is
one-shot. Feed the `habu-bp-lr:` value to `tools/code-owner.f` (below) to turn it
into a caller's name; that pair is the only way to answer "who called this?" in a
seeded engine, because lldb cannot plant a breakpoint in the JIT region at all. `' WORD BP*` is **persistent** (fires every call — the handler
emulates the entry prologue `sub sp,#16` by adjusting the ucontext sp/pc and
leaves the BRK planted, so no single-step is needed). `N ' WORD BPN` is
persistent but **silent for the first N hits** (skip-count). `BP-` removes;
`BP.` lists. Up to 8 at once. The SIGTRAP handler (`EMIT-TRAPH`) resumes via
`sigreturn` with the trampoline token; code is patched through the `patch32`
prim (RW→store→RX→isync, atomic from JIT-resident code). A full
Forth-predicate conditional would need signal-safe deferred evaluation; the
supported conditional breakpoint mechanism is skip-count (`BPN`).

## gdb/lldb — native stepping boundary
Use the Habu stepper, breakpoints, watch cells, `jitdump`, and `imgdump` first.
Use gdb on Linux and lldb on macOS only when the fault is in startup or emitted
machine code before the Forth debugger is reachable. Inspect data-stack cells,
DATA header cells, and watch-cell addresses before adding print probes. On macOS,
lldb may need the admin password once. AMFI signature cache keys on the
path/cdhash, so a binary that ran fine can be SIGKILLed at a path that previously
held an invalid signature. Write to a fresh path when in doubt.

## Forth disassembler (preferred over external disassemblers)
The native disassembler decodes habu's ARM64 subset to mnemonics. Its decode math
and encoders are written as checked Forth where expressible. Use this to inspect
generated code before falling back to external tools.

```
bin/hb --load src/arch/arm64/disasm.f tools/jitdump.f -- ': S dup * ;' S
bin/hb --load lib/errors.f lib/string.f src/arch/arm64/disasm.f tools/imagedisasm.f -- bin/hb $1000 16
bin/hb --load tools/imgdump.f -- bin/hb
bin/hb --load tools/imgdump.f -- old-hb new-hb
```

## Which word owns this address — `tools/code-owner.f`

A debugger stop, a crash dump and a breakpoint all hand back raw addresses in the
JIT region, where no external symbol table reaches: `nm` and lldb see the loaded
`__text` and nothing else. The dictionary has the answer, because every record
carries its routine's start and length.

```sh
<engine> --load tools/code-owner.f tools/code-owner-main.f -- '$181954'
```

The argument is a **region offset**, not an address — ASLR moves the region every
boot, so an absolute address caught in one process means nothing in the next, and
`region-off=` is what one run can hand another. It prints every record whose span
contains the address (an `EXPORT` alias and a republication both make that more
than one) with the offset into each, or says plainly that no record owns it.
`CODE-OWNER:AT.` takes a live address for use inside a larger probe, and
`CODE-OWNER:AT` answers the count so a caller can tell "no owner" from silence.

It must run inside the engine under study, for the same reason
`tools/snap-heap-owner.f` must: that is the only process where those records
exist. This is what turned an anonymous return address into `owner=PATHZ off=384`
during the merged-engine crash hunt (dot habu-merged-engine-nmigrate-c970bf04).

## Dictionary / xref inspection
Semantic dictionary inspection is a live-image Forth surface, not external text
search. `src/habu/xref.f` is baked into `bin/hb` and exposes `LATEST`,
`XREF-FIND`, `XREF.`, `XREF`, `SEE`, and `WORDS`; use these before source search
when debugging dictionary ownership. `XREF word-name` prints the latest matching
record name, start, length, flags, and wordlist.

## Who owns a persisted DATA cell — `tools/snap-heap-owner.f`

When two builds of the same snapshot image differ, the differing byte offsets say
*where* a stale pointer sits but not *whose* it is, and guessing an owner from
what the cell contains is how this class of bug gets papered over. This tool
answers the question from the dictionary instead. It prints two maps:

- `SNAP-HEAP-OWNER:DUMP` — `<heap offset> <name>` for every word that owns a
  piece of the DP heap. The owner of a drifting offset is the last line whose
  offset is not greater than it. A word is recognised as a heap owner by the one
  fixed shape `create`/`variable` compiles (the four-instruction MOVZ/MOVK x9
  address chain, the push stencil, a return, code length 24), and the address it
  owns is read out of the chain's immediate fields.
- `SNAP-HEAP-OWNER:CODE-MAP` — `<JIT region offset> <code length> <name>` for
  every word that has code, headed by the region base and heap top this run got,
  so a program counter caught by a debugger watchpoint turns into a name.

It has to run inside a process that has the source under investigation loaded and
has not retired its dictionary, because that is the only place the names exist.
The way to get one is to add two lines to `src/habu/snap.f` just above the final
`RETIRE-AND-PERSIST`, run a snapshot build, and take the lines off again:

```
require tools/snap-heap-owner.f
SNAP-HEAP-OWNER:DUMP
```

```sh
HB_TMP=<private-root> bin/hb --load tools/build-fixpoint-refresh.f -- snap > owners.txt
```

The heap map that produced the owner table in dot
`habu-fix-persisted-dangling-a520f7b4` had 1793 owners; pairing it against the
offsets `cmp -l` reports between two images built from one `hb-stdin` and one
`hb-snap-src` named every drifting cell above the engine-reserved band.

For the same reason it cannot be loaded on its own: it reads the dictionary
through `src/habu/xref.f` and the instruction encodings through
`src/habu/habu1.f`, and the snapshot builder inlines both rather than
`require`-ing them, so the tool must not `require` them either.

## Is this heap cell a persisted pointer or a live one — the ASLR intersect

A snapshot image carries the whole DP heap verbatim, so a cell holding an
address the *build* process owned — a `malloc`ed arena base, an execution token
in the build's code region — is wrong the moment the image boots somewhere else.
The restored process is full of perfectly good addresses in the same numeric
range, so "this cell looks like a pointer" separates nothing. What separates them
is where the number comes from: **a persisted pointer was written into the image
and is therefore identical in every run; a live one is produced by the running
process and moves with ASLR.**

So run the same image twice, dump the DP heap from each, and intersect:

```sh
for n in A B; do
  ( (printf '7 .\n'; sleep 30) | ./hb-new >/dev/null 2>&1 ) &
  sleep 3
  pid=$(pgrep -n -f hb-new)
  lldb --no-lldbinit -b -p "$pid" \
    -o "memory read --outfile heap-$n.bin --binary --force 0x44000000000 0x44001100000" \
    -o detach -o quit >/dev/null
done
```

A cell that holds an out-of-band address (above the image, i.e. neither the DP
heap at `DATA-VA` nor this run's code region) **and holds the same value in both
dumps** is persisted, and it is a defect. A cell whose value differs between the
two dumps is this process's own and is fine. The pipe held open by `sleep` is
what gives a booted, quiescent process to attach to; the trivial `7 .` only
proves the engine reached its REPL.

Run the same intersect against an image built from a known-good tree and compare
the two sets **by owner name** (`SNAP-HEAP-OWNER:DUMP`, above): what the suspect
image carries and the good one does not is the regression, and everything in both
is a pre-existing cell the good image already ships green. That comparison is
what named `DEV-A-P` in dot `habu-single-prefix-load-17a8c792` — the arena base
of `src/core/decl-event.f`, `malloc`ed by the build and persisted, so the warm
image's first `ENUM` stored its event at `stale base + DEV-N * DEV-REC` and died
`EXC_BAD_ACCESS` on an unmapped page. Note that it did NOT die on every run: when
ASLR happened to leave that address mapped, the store landed in live memory and
the program "passed". A nondeterministic pass rate is itself a symptom of this
class, not noise to be re-run away.

Two ways this class hides:

- **A double load re-seeds it.** If the build loads the owning source twice, the
  second load re-runs `variable X-P X-BOOT X-P !` and the persisted copy is the
  fresh one. The defect is still there; only the accident is. Removing a
  redundant load is therefore a change that can expose persisted-pointer bugs
  anywhere in the tree, and this intersect is how to check.
- **The owner map does not name it.** `snap-heap-owner` recognises only the
  engine's own fixed x9 address chain, so a cell inside a natively compiled
  `create` shows up attributed to whatever recognised owner lies below it, with a
  large offset. Treat a big offset as "unnamed", not as that word's field.

## Stage0 mirror vs native engine — which engine is actually running

Two independent engines compile the prefix, and a defect can live in one and be
invisible to the other. `src/habu/habu2.f` is the native engine, baked into an
installed `bin/hb`. `bootstrap/cg/forth.fs` is the Gforth-hosted mirror that
builds `hb-stage0` during recovery. The mirror is meant to stay byte-for-byte
equivalent to the native engine, and nothing currently proves that it does.

This matters because `tools/bootstrap.sh` builds `hb-stage0` with the mirror at
line 315 and runs it at line 318. That run is the first time a native binary
loads the prefix, and it happens before any natively built engine exists. So a
mirror-only defect stops the bootstrap outright, while every test that boots a
child engine from an installed `bin/hb` keeps passing — those tests exercise the
native engine and structurally cannot see the mirror.

When a failure appears during `tools/bootstrap.sh` but the matching test suite is
green, suspect this split before suspecting the checker. Identify the engine by
where the failure lands: a diagnostic printed by `hb-stage0` (bootstrap.sh line
318, before any `stage2:` message) is the mirror; the same source failing under
`bin/hb --load` is the native engine.

### Reproducing a mirror-only prefix defect

Patch the working tree and let `tools/bootstrap.sh` drive it. No separate seed
builder is needed — the script already assembles the prefix in the right order,
and the failure surfaces about five seconds in, at the `hb-stage0` run.

The worked example below is the pre-trust deferred-word replay (dot
`habu-fix-stage0-pre-88a4297e`). A `defer` declared before `: TRUST` in
`src/core/checker.f` is copied into the pending table described in
`src/habu/layout.f` (the `PD-*` constants) and replayed by `DRAIN-PRETRUST`. The
replay is what teaches the checker the name, so a later checked `is` on that
deferred word can compare the quotation against the declared effect.

Append a pre-trust deferred word to `src/core/exec-vector.f`, the earliest
prefix file where a `defer` is legal:

    defer ZZ-PRETRUST-XT ( -- n )

and a checked round-trip to the end of `src/core/check-hook.f`, which is the
first file that compiles with the check hook installed:

    : ZZ-PRETRUST-SELFTEST ( -- )
       [: 42 ;] is ZZ-PRETRUST-XT
       ZZ-PRETRUST-XT 42 <> IF s" zz: pre-trust round-trip failed" 76 die THEN ;
    ZZ-PRETRUST-SELFTEST

Then run the recovery launcher and restore the two files afterwards:

    HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth tools/bootstrap.sh

Under a working mirror this completes with `bootstrap OK: bin/hb`. While the
mirror replay is broken it exits 70 in about five seconds with

    hook: non-certified definition: zz-pretrust-selftest at 'is'

The same two patches applied through `test/pre-trust-defer.f`, which boots child
engines from an installed `bin/hb`, pass — that contrast is the evidence that the
defect is in the mirror and not in the checker, the prefix source, or `is`.

### Censusing what the checker actually learned

`is NAME` needs two separate checker rows: the deferred-word row that
`CHECKER-FIND-ACTIVE-DEFER` reports, and the signature row that
`CHECKER-DEFINED?` reports. `DRAIN-PRETRUST` is supposed to produce the first
through `checker-defer` and the second through `trust`. Ask for both by name
immediately after the bare `DRAIN-PRETRUST` token in `src/core/checker.f`:

    s" NAME dfr=" type s" NAME" CHECKER-FIND-ACTIVE-DEFER .
    s" sig="      type s" NAME" CHECKER-DEFINED? .

Use the lower-case spelling; the lookup folds the token. Run the same census
under both engines and compare — an installed `bin/hb` re-reads the prefix from
disk at boot, so patching `src/core/checker.f` and running any
`bin/hb --load <file>` prints the native answer without a rebuild.

On 2026-07-28 that census read `dfr=-1 sig=-1` for every pre-trust deferred word
under the native engine and `dfr=0 sig=0` under the mirror. The conclusion drawn
at the time — that the mirror's replay of `trust` produces no signature row —
was WRONG, and it cost a lane. Both halves of the replay run and both reach the
checker; what differed was WHICH checker they reached. Read the next section
before trusting a census: a recovery engine loads `src/core/checker.f` twice in
one process, so a census printed right after `DRAIN-PRETRUST` answers about
whichever load is running, and the two loads give opposite answers.

### Reading the replay from inside the engine

Instrumenting `src/core/checker.f` changes what `test/bootstrap-wide-memory.fs`
measures, so `tools/bootstrap.sh` then stops in its first gate with `bootstrap
wide memory mismatch` and never reaches the stage0 run. Instrumented runs must
therefore skip the launcher and build the seed directly. The seed is exactly the
file `tools/bootstrap.sh` writes to `$HB_TMP/stage2-src`, used as written —
`emit_src` gives every consumer the same text, boot-hide prologue included, and
that prologue is load-bearing: strip it and the boot dies at exit 70 before you
see any of your instrumentation (see below). Build and boot it with:

    HABU_TARGET=<target> gforth -e 'require test/nf.fs s" <seed>" slurp-file s" <out>" FORTH-BUILD-EXE bye'
    HB_TMP=<dir> <out> -- <dir>

A boot that reaches `stage2: cannot open source` (exit 74) got through the whole
prefix; that message is success for this purpose.

### The recovery engine reads the prefix twice

This is the fact that made the 2026-07-28 census misleading, so keep it in mind
for any probe placed in the boot prefix. The emitted engine reads every prefix
file from disk when it starts — `PFX-LOAD-CHECKER-FILES` and its siblings in
`bootstrap/cg/forth.fs` emit `LSRCRD` calls on baked path strings — and then
interprets its baked program, which for a `FORTH-BUILD-EXE` binary is the whole
prefix again plus a driver. So every top-level action in `src/core/checker.f`
happens twice, in two different checker instances. Two markers tell the loads
apart in a trace: only the startup load runs `src/core/include.f`, and only the
baked program runs `src/habu/habu1.f`.

The second load must not inherit the first load's words. That is the job of the
boot-hide prologue `emit_boot_hide` in `tools/bootstrap.sh`, which hides the
startup load's dictionary and clears its recorded effects, and it is why
instrumented seeds built by hand (above) drop it deliberately. When it is
missing, `trust` and `checker-defer` from the startup load are still resolvable
while `checker.f` is being re-read, so `C-PRETRUST-READY?` says "ready" and
every defer declared before `: TRUST` publishes into the checker that is being
replaced; nothing is captured, the drain replays nothing, and the first checked
`is` on such a defer fails with `hook: non-certified definition: ... at 'is'`
and exit 70 (dot habu-fix-stage0-pre-88a4297e).

To see this directly, put the engine-side probe and the checker-side probe on
the SAME file descriptor so their order is evidence: write the slot name from
inside the `BDRAINPRETRUST` loop to fd 1, and `type` the name at the head of
`: TRUST` and `: CHECKER-DEFER`. Interleaved output of the form
`[NAME><tr:NAME><cd:NAME>]` proves the replay reached the checker; a second
`is NAME` later in the same stream answering differently from the first proves
you are looking at two loads, not at a broken replay.


The pending table and its replay are assembly in both engines, so ordinary
`type`/`.` probes cannot reach them. Two techniques cover it without
print-bisecting:

- Instrument the checker end in Habu. Add a `type` of the name to `: TRUST` and
  to `: CHECKER-DEFER` in `src/core/checker.f`, then bracket the bare
  `DRAIN-PRETRUST` token with markers. If the markers print with nothing between
  them, the replay never reached the checker.
- Instrument the engine end by making an existing fail-closed exit fire where you
  want a probe. Calling `C-PD-DIE-FULL` at the top of `C-PD-CAPTURE` proves the
  capture branch was taken and names the deferred word; the same call inside the
  `BDRAINPRETRUST` loop body proves the table was non-empty at replay time. Both
  print the current token and exit 72, so they need no new string labels.

Copy `bootstrap/` and `test/nf.fs` into a scratch directory before instrumenting
the mirror, and point Gforth at the copy, so the repository tree stays clean.

## External disassembly — last resort
Use external disassemblers only when the native disassembler lacks an encoding.
On Linux, `objdump -d` or `readelf -l` can inspect ELF text and load segments.
On macOS, `otool -tv` can inspect `__text`; verify page hashes against the
embedded CodeDirectory when signature behavior is involved.

## Source arenas

Three independent source arenas share one capacity policy but have different
contents and failure boundaries:

- `IBUFSZ` holds the cold source prefix and the later program input in the
  generated engine. Its effective maximum input is below `IBUFSZ` because the
  prefix is already resident and the reader reserves an EOF probe. Discover the
  boundary with bounded `--build` probes against the freshly built candidate so
  the measurement uses `LCOLDPFXB`; never assume `IBUFSZ+1` is the first failing
  file. Overflow exits 74 with `hb: source prefix buffer full`.
- `S2-SOURCE-CAP` is the anonymous mapping used by `src/habu/stage2.f` to read
  the generated fixpoint compiler source. It is not the engine input arena. A
  candidate-backed regression proves cap-minus-one succeeds and exact-cap exits
  74 with `stage2: source exceeds buffer`.
- `MK-SOURCE-CAP` is the dictionary allocation used by `src/habu/maker.f` to
  read the generated AOT/REPL maker source. It is not the stage2 mapping. Its
  candidate-backed reader regression proves the same adjacent boundary with the
  exact `maker: source exceeds buffer` diagnostic.

`SOURCE-HEADROOM-PCT` requires at least 25 percent above the live composite,
then `SOURCE-ARENA-CAP` is the smallest power of two meeting that requirement.
The 2026-07-15 owner-persistence merge measured a 1,687,332-byte live composite;
25 percent headroom required 2,109,165 bytes and selected 4 MiB. The 2026-08-17
seeded-signature merge measured 3,421,904 bytes (a 1,422,190-byte cold prefix
plus a 1,999,714-byte stage2 source); 25 percent headroom required 4,277,380
bytes and selected 8 MiB. Measurements are selection history, not enforcement
constants.
The fixpoint regression regenerates the stage2 source, derives cold-prefix
occupancy from the candidate's probed boundary minus the required EOF-read byte,
and enforces the minimal shared power-of-two from those live sizes. The hb-build
regression generates both REPL and AOT `hb-maker-src` inputs and proves their live
maximum retains the required headroom under that shared power of two. Native
layout and Gforth recovery carry matching owner tokens; stage2 and maker alias
that owner rather than carrying independent numeric ceilings.

## A child-process fixture disagrees with itself — `tools/launch-context.f`

A fixture that spawns a child and asserts its exit code reports a bare number
when it fails (`expected 73 got 70`), which is what makes this class look
environmental: the child printed the reason and the fixture threw it away. Get
the reason before theorising about the launcher.

- `lib/test/spawn-report.f` is the reporter. `SPAWN-REPORT:CHILD
  ( ptr u8 n n n ptr u8 n ptr u8 n -- )` takes a label, the wanted and the got
  rc, and the captured stdout/stderr, and prints all of it plus the launch
  context. Wire every child-rc assertion in a fixture through a helper that
  calls it on a mismatch — `test/pre-trust-defer.f` `CHILD-RC` is the pattern.
  The exit-70-vs-73 disagreement that stood for a day was one line of the
  child's own stderr.
- `bin/hb --load tools/launch-context.f` prints only the context, so the same
  report can be taken under different launchers and diffed. Every line starts
  `ctx `: pid, script argv, whether `bin/hb` is reachable from the process's cwd
  and from its inherited `PWD` (a stale `PWD` shows up as a yes/no split), fds
  0/1/2 with open state, status flags and tty-ness, and the environment.

```sh
bin/hb --load tools/launch-context.f | grep '^ctx ' | sort > /tmp/pipe.txt
script -q /tmp/tty.log bin/hb --load tools/launch-context.f >/dev/null
grep '^ctx ' /tmp/tty.log | sort > /tmp/tty.txt
diff /tmp/pipe.txt /tmp/tty.txt
```

tty-ness is read with the host's own terminal-attributes ioctl, selected by
`HB-TARGET-MACOS?`/`HB-TARGET-LINUX?`. The two hosts' request numbers are not
interchangeable — issuing Linux `TCGETS` on macOS kills the process (exit 83) —
so an unrecognised host throws `E-PROC-HOST` instead of trying both.

Before concluding "environment", check the cheaper explanations the same way
this class was mis-filed once already: the suite may not be selected by the
slice that looked green (`SUITE-RUN?` in `test/gate-stdlib-lib.f`), and a
fixture that asserts a specific exit code may simply be asserting a code the
tree stopped producing.

## Standalone gotchas a stepper catches fast
- A 2nd `{: :}` locals group mis-reads its slot (use a variable instead).
- Declaring locals inside `IF`/loop corrupts the frame.
- Unchecked native emitters can be visibly balanced but still corrupt the build
  stack through saved-register/frame mistakes; bracket phase calls with
  `depth .`, then factor the offending raw emitter and add a source-shape gate.
- Plain `DO` is do-while (`0 0 DO` runs once); guard zero-trip loops.
- Undefined words must fail closed through the checked load path. If a runtime
  path reaches an unknown word without diagnostics, treat that as a
  checker/compiler RCA before editing downstream code.
- The dictionary search returns the newest definition; use the checker and
  shadow lint when a new word appears to change built-in behavior.

## Native codegen refusals — what the pass actually decided

A migration that throws gives one number and no operation. Two dumpers turn
that number back into the decisions behind it. Both compile the source through
the production migration entry (`NMIGRATE:MEASURE-HELD`) and publish nothing,
so a body can be dumped as many times as it takes.

- `tools/codegen-alloc-dump.f` — what the REGISTER ALLOCATOR decided: the walk
  generation, the spill slots, every plan row (store/move/remat/reload) and
  every value it gave no register to. Start here for `E-A64RAV-REGISTER` and
  anything that smells like an eviction.
- `tools/codegen-verify-dump.f` — what the RESIDENCY VERIFIER judged: the
  machine module block by block, each operation's shape, the data-stack slot it
  moves, and the use count the verifier itself computed for each result. It also
  names which of the three findings behind `E-A64RAV-DKEEP` fired and at which
  block and position — that code covers a load into a slot that already holds a
  named value, a load whose result nothing reads, and a store of the value the
  slot already holds, and the three have different causes. A `dload` printed
  with `u0` in front of an elided store is the planning pass and the emission
  disagreeing about the map.

Drive either with a one-line second file, e.g.

```
bin/hb --load tools/codegen-verify-dump.f /tmp/dump-run.f
```

where `/tmp/dump-run.f` is a single `VERIFY-DUMP:REPORT` call carrying the
source text and nothing else — the arity is the checker's and the register
pool is the machine's, so neither is a thing the caller can get wrong.

## Performance measurement tools (tools/perf/)

- **"Where does boot time go" for a protection change — measure the real
  workload; there is no tool row here on purpose.** Two cheaper instruments
  were tried and both refuted on the same landing (2026-08-14), so neither
  gets a shortcut in this file. `sample(1)` bucketed by mprotect stub
  under-reports syscall time about 5x (`tools/perf/protcost.py` read 12.5ms
  where wall/sys said 67-80ms; the tool was deleted rather than left as a
  wrong number with a trustworthy interface). Replaying the syscall
  sequence against a fresh bare mapping under-predicts about 4x in the
  other direction, because nothing executes inside the replayed region and
  the real cost is not the syscall at all — a wide RW→RX→RW flip drops the
  PTEs of the JIT code the engine is executing, which showed up as 93,691
  minor faults per boot. Price a protection change by running the actual
  workload and reading wall/sys time and the minor-fault count together
  (`/usr/bin/time -l` on macOS); attribute it by moving the fault count,
  not by attributing samples to a stub.
- `tools/perf/boot-census-watcher.c` + `boot-census-analyze.py` — follow a
  command's whole process tree via kqueue `EVFILT_PROC` and classify every
  child fork-vs-exec by image path and argv (fork children keep the
  parent's argv). Built to count the gate's cold engine boots for the
  Stage B refutation; cross-checked exactly against `lib/process.f`
  PROCESS-TRACE at the top level. Start here for any "how many engines
  does this run actually start" question.
