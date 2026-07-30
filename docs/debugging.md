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
prints `habu-bp-stack:` with each live data-stack cell and `habu-bp-watch:` with
watched address/value pairs, then restores the original instruction and
**resumes** the word; the breakpoint is
one-shot. `' WORD BP*` is **persistent** (fires every call — the handler
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
`SNAP-RETIRE-GO`, run a snapshot build, and take the lines off again:

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
25 percent headroom required 2,109,165 bytes and selected 4 MiB. Measurements
are selection history, not enforcement constants.
The fixpoint regression regenerates the stage2 source, derives cold-prefix
occupancy from the candidate's probed boundary minus the required EOF-read byte,
and enforces the minimal shared power-of-two from those live sizes. The hb-build
regression generates both REPL and AOT `hb-maker-src` inputs and proves their live
maximum retains the required headroom under that shared power of two. Native
layout and Gforth recovery carry matching owner tokens; stage2 and maker alias
that owner rather than carrying independent numeric ceilings.

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
