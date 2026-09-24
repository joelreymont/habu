# Debugging the self-hosted standalone

The standalone is JIT-compiled native ARM64; blind `.`-printing was the recurring
pain. Toolkit:

## Where a load refusal happened — read the message

A refusal the engine raises while reading source names the file and the line:

```
hb: bad string literal at /home/j/Work/habu/test/fixtures/x.f:12
```

No column. `<path>` is the resolved, canonical path of the **innermost open**
file, so a nested `require` names the file that actually holds the offending
token, not the entry the command line asked for. `<line>` counts newlines from
the start of the buffer being evaluated: an include evaluates the whole file as
one buffer, so that is the file line.

Two cases print the message and a newline with **no** ` at …`, because there is
no file to name: source on stdin or at the tty REPL, and the engine's own boot
prefix. A string a harness `evaluate`s *inside* a file is the one case where the
two halves come from different places: the path is the file's, the line counts
within the evaluated string.

One place prints this — the `LCOMPILEDIE` tail in `src/habu/habu2.f`, from the
three cells `src/habu/layout.f` reserves as `SRCLOC:PATH-CELL` /
`PATHLEN-CELL` / `INB-CELL`. Every die site that branches there writes its
message with no trailing newline and the tail ends the line.

**What is located, and what is not.** Located: every refusal routed through
`LCOMPILEDIE` — bad string literal, counted string too long, data space out of
range, dictionary/code space full, definition body text full, BEGIN nesting
full, nested quotation, duplicate definition, `does>` in a checker-rejected
body, malformed stack signature, `;]` with no open quotation, `does>` with
locals active, a local referenced inside a quotation, locals opener inside a
quotation, `:`/`cast:`/`defer`/`is` missing a name, `is` target not found or not
deferred, `package`/`export` misuse, the whole `using` family, and the
`construct`/`match` operand refusals (`hb: construct: unknown family: NOPE at
<path>:<line>`).

Still unlocated, and why: the interpret-level diagnostics share a **different**
tail (`LDIAGRET`) — `hb: undefined: X`, `hb: interpret-mode layout value`,
`hb: internal engine word`, `hb: interpret stack underdepth`,
`hb: control-flow closer without opener`, `hb: control-flow nesting too deep`,
`hb: local name over 16 bytes`, `hb: more than 64 locals in one definition`.
So do the refusals that `exit_group` without any tail: the boot source errors
(`hb: source prefix buffer full`, `hb: cannot read source`), the CLI ones
(`hb: unknown flag`, `hb: cannot open`), `hb: uncaught throw code N`,
`hb: catch frame corrupt`, the snapshot, AOT, protected-WID, lowering,
address-cell and mmap families, and `hb: repl line over`.
For those the checker's own diagnostics (which carry `<path>:<line>` of their
own) or a bisect are still the way in.

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

## Loading the debugger — `require src/habu/debug.f`
The engine bakes the compiler, the JIT and the REPL and nothing else
(`src/habu/native-runtime.f`), so the debugger arrives on demand: one
`require src/habu/debug.f` loads the breakpoints, the token stepper and the
shared watch cells over the baked REPL, in a session or at the head of a
program, and costs about 14 ms. Every `BPW*`, `step` and `BP*` example below
assumes that line ran first.

## `BPW+` / `BPW-` / `BPW.` — watched cells
`src/habu/debug-watch.f` loads before the stepper/debugger. It publishes a small
watch table used by both `step` and compiled-word breakpoints.
Add a cell address with `BPW+`, remove it with `BPW-`, clear all watches with
`BPW-CLEAR`, and list `address value` pairs with `BPW.`. For fixed engine cells:

```
DATAB ENVP-CELL + BPW+
DATAB ARGV-CELL + BPW+
```

## `step` — native token stepper (in the REPL, `bin/hb` on a tty)
`src/habu/stepper.f` installs its own REPL read hook. `step 5 dup * 3 +` runs
the rest of the line one token at a time, echoing each token and printing the
data stack and watch table after it executes — no `EVALUATE` needed: the REPL
hook feeds the engine one token per call, so the engine's own interpret loop is
the evaluator.

## `BP+` / `BP-` — one-shot breakpoints on compiled words (REPL)
`src/habu/debug.f` (the require above): `' WORD BP+` plants a `BRK #0` at the
word's entry. Hitting it prints `habu-bp:` + the pc + the data-stack top, then
`habu-bp-lr:` + the **interrupted thread's x30** — the address the word will
return to, which is what names its caller — then `habu-bp-stack:` with each live
data-stack cell and `habu-bp-watch:` with watched address/value pairs, then
restores the original instruction and **resumes** the word; the breakpoint is
one-shot. Feed the `habu-bp-lr:` value to `tools/code-owner.f` (below) to turn it
into a caller's name; that pair is the only way to answer "who called this?" in a
seeded engine, because lldb cannot plant a breakpoint in the JIT region at all. `' WORD BP*` is **persistent** (fires every call — the handler
emulates the word's entry instruction and leaves the BRK planted, so no
single-step is needed). A tier-0 word entry is one of exactly two instructions:
`str x30,[sp,#-16]!` in a word that calls, or a `nop` in one that does not
(`EM-COMPILE-RET` rewrites the slot at `;`). The handler reads the instruction
the BRK replaced out of the breakpoint slot to tell them apart: a nop needs only
`pc += 4`, and the save also gets `sp -= 16` and the interrupted `x30` written at
the new `sp`. That store lands in the sixteen bytes the kernel's signal frame
keeps for its `frame_record {fp, lr}`; nothing reads those back (`sigreturn`
restores from `uc_mcontext` lower in the frame, and the handler addresses its
locals off `sp`, never off `x29`), and the resumed word loads the value with
`ldr x30,[sp],#16` while the frame is still there. There is no other address to
write: that one IS the slot the word's epilogue reads. `N ' WORD BPN` is
persistent but **silent for the first N hits** (skip-count). `BP-` removes;
`BP.` lists. Up to 8 at once. The SIGTRAP handler (`EMIT-TRAPH`) resumes via
`sigreturn` with the trampoline token; code is patched through the `patch32`
prim (RW→store→RX→isync, atomic from JIT-resident code). A full
Forth-predicate conditional would need signal-safe deferred evaluation; the
supported conditional breakpoint mechanism is skip-count (`BPN`).

Targets must be instruction-aligned addresses in the live compiled code region.
Engine-text primitives such as `atomic-cas` are refused with `E-BP-TARGET`
before changing the breakpoint table or page permissions.

`cp@` is stable only inside a compiled word: the interpreter compiles each
top-level line into a transient buffer at `cp@`, so a top-level `cp@ patch32`
clobbers the executing line (SIGILL). Write a runtime stub at `cp@` from inside
`: WORD … ;`. To verify emitted primitive bytes without running them, compute
the exact ARM64 encodings and search the on-disk `bin/hb` for the contiguous
stream — ASLR slides the xt, file bytes do not move.

An uncaught throw in a `--load` or spawned child exits with the throw code's
low eight bits and prints nothing: exit 56 is `E-PROC-TRUNCATED` (-2504), 104
is `E-STR-BOUNDS` (-2200). Add multiples of 256 until a known `E-*` appears
before hunting for the site; a one-byte diagnostic and a clean exit means a raw
engine capacity path (`exit_group`), not a throw.

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

For a baked Mach-O image, `imgdump` reports the preferred virtual addresses
recorded in its `__text` section. Remove the process's ASLR slide from a live
PC before passing it to `--pc`. Snapshot records use their persisted canonical
coordinates. `tools/engine-size.f` reads Mach-O load commands and accounts for
the GOT, chained fixups, code signature and padding as well as the payload.

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
contains the address (an `EXPORT` alias or `;does` companion can make that more
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
  `DKIND:ADDR` stamp `create`/`variable` publishes; the address it owns is read
  with the shared, bounded `SNAP-RELOC` carrier decoder. A word that merely uses
  that address is not an owner.
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

It reads the live dictionary through the engine's `src/habu/xref.f` surface and
loads the pure grammar from `src/habu/address-carrier.f`; image inspection does
not need the capture buffers or assembler state in `aot-decl.f`.

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
- **The owner map does not name it.** `snap-heap-owner` lists live records still
  stamped `DKIND:ADDR`; `does>` clears that stamp. An unnamed cell may therefore
  fall far above the preceding owner. Treat a big offset as "unnamed", not as
  that word's field.

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

Three independent source arenas share one capacity but have different
contents and failure boundaries:

- `IBUFSZ` holds the cold source prefix and the later program input in the
  generated engine. Its effective maximum input is below `IBUFSZ` because the
  prefix is already resident and the reader reserves an EOF probe. Discover the
  boundary with bounded `--build` probes against the freshly built candidate so
  the measurement uses `LCOLDPFXB`; never assume `IBUFSZ+1` is the first failing
  file. Overflow exits 74 with `hb: source prefix buffer full`. The prefix rows
  come in through `LSRCRDP`, which drops each file's comment and blank lines
  (`EMIT-SOURCE-READ-PREFIX`), so the resident prefix is about 41 percent
  smaller than the files on disk; `LSRCRD` still reads argv files and the
  `--build` payload byte for byte, so only the prefix term moved. A seeded
  engine emits no cold prefix at all, so the installed `bin/hb` starts the
  program at the base of the arena.
- `S2-SOURCE-CAP` is the anonymous mapping used by `src/habu/stage2.f` to read
  the generated fixpoint compiler source. It is not the engine input arena. A
  candidate-backed regression proves cap-minus-one succeeds and exact-cap exits
  74 with `stage2: source exceeds buffer`.
- `MK-SOURCE-CAP` is the dictionary allocation used by `src/habu/maker.f` to
  read the generated AOT/REPL maker source. It is not the stage2 mapping. Its
  candidate-backed reader regression proves the same adjacent boundary with the
  exact `maker: source exceeds buffer` diagnostic.

`SOURCE-ARENA-CAP` is the shared capacity owner. Native layout and Gforth
recovery carry matching owner tokens; stage2 and maker alias that owner rather
than carrying independent numeric ceilings.

The three ceilings a PROGRAM reaches — one definition's captured body text
(`BODYBUF-CAP`, rc 71), one REPL line (`LLINE-MAX`, refused at the prompt) and
`begin` nesting per definition (`JIT-SNAP:FRAMES`, rc 75) — are not arenas and
are documented where the author of the program will look, under "Engine limits
ordinary source reaches" in [forth.md](forth.md). Each states its ceiling and
the count it saw on fd 2, so a refusal is read, not bisected.

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
this class was mis-filed once already: the test may not be registered in
`test/gate-stdlib-cases.f`, and a fixture that asserts a specific exit code may
simply be asserting a code the tree stopped producing.

## Stack diagnostics — which of the three you are looking at
Every VM stack (the boot data stack, a task's own data/return/loop stacks, a
run-in-stack callback's stack) is a mapping with an inaccessible page on each
side (`STACK-ABI:PAGE-BYTES`, `src/habu/rt.f` EMIT-MAP). Compiled code carries
no bounds check at all any more — capacity is enforced by the MMU, not by a
check at every push and pop — so three independent mechanisms answer "why did
this die", and they are not interchangeable:
- `E-UNDERFLOW: <token>` (exit 70) is the data stack read below its base while
  `<token>` was being interpreted, however deep the read happened: the
  interpreter's depth checks catch it before a primitive runs. Inside
  `evaluate` it is a catchable RC-REJECT throw; in the REPL the line recovers.
  This is the one check still anywhere near the per-token path, and it only
  ever fires from the interpreter — a stripped application has no interpreter
  to name the token, so the same underflow there is a guard-page fault
  instead (below).
- `hb: stack bounds exceeded (data)` / `(return)` / `(loop)` (exit 102,
  `ENGINE-ERROR:STACK-BOUNDS`) is a guard-page fault: a push past the capacity
  or a read below the base takes SIGSEGV/SIGBUS, and `src/habu/crash.f` reads
  the faulting address out of the signal context and classifies which of the
  three named stacks it landed in — the parenthetical names the stack, not
  the operation. This is what a per-transfer bounds check used to report
  generically; the message and exit code are unchanged from before guard
  pages, only the `(name)` suffix is new, so match it as a prefix
  (`hb: stack bounds exceeded`) rather than the whole line if the specific
  stack does not matter to the assertion.
- `E-STACK-UNGUARDED` (-3802, `lib/errors.f`; `STACK-ABI:E-STACK-UNGUARDED` spells
  the same number for the engine emitters, and `test/stack-guard.f` proves
  the two agree) is run-in-stack's own admission check
  (`src/habu/habu1.f` BRUNSTACK `GUARDED-EXTENT?`), thrown *before* the
  callback ever runs and before any stack switch happens — catchable, not a
  crash. It is a STRUCTURAL test of the extent, not a provenance test: it
  knows nothing about where the extent came from and never asks whether
  `lib/memory.f` `MEM-ALLOC-GUARDED` made it. It tests six things in a fixed
  order, and the first that fails is the one that decides — base non-zero,
  capacity non-zero, base a whole `STACK-ABI:PAGE-BYTES` multiple, capacity a
  whole `STACK-ABI:PAGE-BYTES` multiple, base + capacity not wrapping, base
  outside the DATA region. The DATA-region clause is what makes this a proof
  rather than a guess, because every `create`, `allot`, `,` and `buffer`
  address lies inside that region — but it is the LAST clause, so an ordinary
  create/allot buffer is normally refused earlier, on base alignment, and a
  case that means to exercise the region clause has to hand over an address
  that is page-aligned to begin with (`test/stack-guard.f`
  DATA-REGION-REFUSAL uses `data-base` itself). In practice
  `MEM-ALLOC-GUARDED` is the only thing that returns an extent all six
  clauses accept. Because a guarded mapping only comes in whole-page sizes,
  there is no way to hand run-in-stack a "slightly too small" guarded stack
  any more: a request is either refused up front with `E-STACK-UNGUARDED`, or
  it runs on a full page and any real overflow is a guard-page fault instead.
  Nothing malformed reaches the older descriptor check behind this one:
  `GUARDED-EXTENT?` runs strictly before it at the same switch and, for a
  freshly entered stack (used bytes = 0), subsumes it so completely that the
  entry `STACK-GUARD:CHECK-CURSOR` call is gone — the check on the way back
  OUT, where the callback may have moved the cursor, stays.
  `test/engine-stack-lifecycle.f` UNGUARDED-UNCAUGHT shows the uncaught shape
  (exit 67, `hb: uncaught throw code -3802`, and an empty stdout because the
  callback never ran).
- A tier-1 top-row warning does not change which of these three a given
  program hits, only that it also warns once first. `' FOO2 execute` on an empty
  stack (`: FOO2 ( n -- n n ) dup ;`) warns once and then still runs FOO2:
  since FOO2 is compiled code invoked through `execute`, not a token the
  interpreter reads directly, its `dup` reading below the base faults the
  data stack's guard page — `hb: stack bounds exceeded (data)`, rc 102 — not
  `E-UNDERFLOW`. A bare `drop` on an empty stack, entered directly at the top
  level or through `evaluate`, still ends in `E-UNDERFLOW: drop` rc 70,
  because the interpreter's own depth floor sees that one before any
  primitive runs (see `test/xt-effect-test.f` XE-TIER1,
  `test/top-row-warn-test.f` TW-POSITIVES, and
  `test/runtime-regression-test.f` for the unchanged interpreted case).

`test/stack-guard.f` has worked cases for all three: filling the boot stack
short of its page by a margin versus filling the whole page, unbounded
data/return/loop recursion, and one case per `GUARDED-EXTENT?` clause — each
changing a single field of an extent the same fixture shows being accepted, so
the clause named in the label is the one that decided. `test/engine-stack-wide.f`
and `test/engine-stack-jit.f` add the capacity boundary of individual
transfers: each wraps the transfer in a ratchet that grows the stack by one
cell per recursion level on a `MEM-ALLOC-GUARDED` stack, so the transfer is
what fills the stack and, being wider than the ratchet, its own write is what
first crosses the guard page.

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
  parent's argv). Start here for any "how many engines does this run actually
  start" question.

## Sampling profiler

`n prof-on` arms a SIGALRM interval timer and counts each tick against the
dictionary word whose code holds the interrupted pc, then walks the interrupted
machine stack for that word's callers. The surface:

| word | effect | what it does |
| --- | --- | --- |
| `n prof-on` | `( n -- )` | build the pc index, clear the counters, start the clock. `n` is a sample limit: at the `n`-th sample the profiler prints the text report and exits 99. `0` samples until `prof-off`. |
| `prof-off` | `( -- )` | stop the clock. The handler stays installed and every counter keeps its value, so the phase just measured can be reported afterwards. |
| `prof-report` | `( -- )` | print the text report. |
| `prof-json` | `( -- )` | print the same walk as one JSON object. |
| `prof-reset` | `( -- )` | clear every counter and keep the index, so a second phase costs no rebuild. |
| `n prof-rate` | `( n -- )` | set the sampling interval in microseconds for the *next* `prof-on` (default 1000). Writing a rate while no handler is installed would hand the process a SIGALRM it cannot take, so it never re-arms the running clock. |
| `n prof-row` | `( n -- )` | print the row for one dictionary record, whatever its rank, with its callers. A phase word takes no exclusive samples at all, so no ranking will ever show it; this is how you read one. The record index comes from the caller, where `XREF` already answers a name. |
| `pc prof-pc>rec` | `( n -- n )` | the record index the armed index gives that pc, or -1. This is the handler's own search, which is what `test/prof-index.f` compares against an exhaustive dictionary scan. |

A report never samples itself: it stops the clock for the walk and starts it
again if `prof-off` has not already stopped it, so the phase loses at most one
interval. Without that the header's own identity would be false by one, because
the report owns x20 and its own ticks read as a foreign context.

### Report format

The first line is the accounting, and every field is named:

```
profiler samples 92000 words 80215 other 291 new 0 defer 11492 spill 0 foreign 2 frames 764625 dropped 0 indexed 15676 usec 1000 attributed 1873
```

- `samples` — ticks delivered.
  `words + other + new + defer + spill + foreign == samples` exactly, and a
  regression in `test/gate-debug-lib.f` reads those named fields and asserts it.
- `words` — ticks attributed to a dictionary word, the sum of every row's
  exclusive column.
- `other` — ticks in Habu code that belongs to no word: the main loop, engine
  helpers, the gaps between spans.
- `new` — deferred ticks the sync could not name at all: their pc belongs to no
  live record. After a sync this is normally 0.
- `defer` — ticks in code compiled *after* `prof-on` built the index, still
  waiting to be named. `prof-report` and `prof-json` name them (see **Code
  compiled after prof-on** below) and leave this at 0; only the automatic report
  at a `prof-on` limit, which runs inside the signal handler and cannot rebuild
  the index, prints a non-zero `defer`.
- `spill` — deferred ticks the buffer had no room for. It holds `$40000`
  samples, about eighteen self-builds' worth; a spill is reported, never dropped
  in silence.
- `foreign` — ticks whose context does not hold the engine's DATA and dictionary
  base registers, which is what a foreign callee reached through the FFI (libc,
  libzip, the CUDA driver) leaves behind. It is a lower bound on foreign time: a
  foreign leaf that keeps those two registers intact is walked like Habu code and
  lands in `other`.
- `frames` — caller frames the stack walk attributed, over every sample.
- `dropped` — caller edges the edge table could not key because its probe window
  was full. They are reported, never merged into another row.
- `indexed` — index entries, and `usec` the interval actually sampled at.
- `attributed` — how many words took a sample of either kind. The text report
  shows the top 24 of them per section, so this is what says how much it left
  out; `prof-json` carries all of them.

Then two sections, each up to 24 rows with their top 5 callers: first ordered by
exclusive count, then, under a `by inclusive` line, ordered by inclusive count.
The second section is the one that shows a phase word — a compiler pass, a
verifier entry — which does no work of its own and is invisible in the first.

```
    9257  10.0   11709  12.7  IR-ARENA:ACEIL!
         <-     1564  16.8 IR-OP:OFF-OPN
         <-     1169  12.6 IR-SCHEMA:UNUSED
```

The row is `exclusive`, its percent of `samples`, `inclusive`, its percent of
`samples`, then the package-qualified word. Exclusive counts the ticks whose pc
was inside that word; inclusive counts the ticks where the word was anywhere on
the sampled stack. A caller line is `<- count percent-of-this-row's-exclusive
caller`. The caller lines are the top few, not every caller, so they need not sum
to the row. `(unknown)` is a real caller row: it is the share of the word's
samples whose caller the walk could not establish, kept explicit rather than
dropped. Rows are selected by repeated maximum rather than sorted, so the
counters are left exactly as they were and a second `prof-report` says the same
thing.

`prof-json` prints **every attributed word**, not a top-N, and every caller edge:

```
{"samples":N,...,"attributed":M,
 "rows":[{"word":"PKG:W","excl":N,"incl":N}, ...],
 "edges":[{"word":"PKG:W","caller":"PKG:C","n":N}, ...]}
```

The edges are a flat array rather than nested under each row on purpose:
nesting them would mean one pass over the edge table per row, and this walk
covers every row. Both arrays are bounded by the arena — at most one row per
index entry and one edge per table slot — so the report cannot outgrow it and
nothing is truncated. The header is the same one the text report prints, from
the same walk, so the two cannot disagree about what they counted.

Inclusive counts a word **once per sample**. The conservative walk can see one
word twice in a single stack — a recursive call, or a stale spill slot still
holding a return address into it — and an inclusive count that ran past the
sample total would say a word took longer than the program did. The handler
stamps each record with the sample serial, so the second sighting costs a load
and a compare and changes nothing.

### Code compiled after prof-on

`prof-on` builds the pc index once, so a word the profiled phase *defines* has no
entry to be found by. For a compile — the self-build above, or any program that
`require`s its own sources — that is most of the profile: a 92-second self-build
put 11,492 of 92,000 samples in code that did not exist when sampling started.

The handler cannot fix this itself: naming those words means reading the
dictionary, and the interrupted thread may be in the middle of adding a record to
it. So the handler keeps the raw pc and the interrupted x30 for such a tick — two
cells, no walk, no allocation — and `prof-report` / `prof-json` do the work, with
the clock stopped: fold the index entries' inclusive counts into the per-record
array, rebuild the index from the dictionary as it now stands, and replay every
deferred sample through it. That is the first moment at which every word the
phase compiled exists.

Two consequences worth knowing:

- The automatic report at a `prof-on` limit runs in the signal handler, so it
  does *not* sync. Its deferred samples stay in the `defer` column. A program
  that can reach a `prof-off` of its own should use one.
- A deferred sample carries only x30, not the stack scan, so its caller is
  whatever x30 pointed at. When the word had already called something that
  returned, x30 points back inside the word itself and the caller is reported as
  `(unknown)` rather than guessed.

### Caller attribution

Emitted words carry no frame pointer — the prologue is `sub sp,sp,#16` +
`str x30,[sp]` — so there is no chain to follow and return addresses are
recognised by their value: a stack cell that lands inside the index's code range
is taken for a return address. That is the standard frame-pointer-less technique
and it is a *superset* of the true chain: an uninitialised spill slot still
holding a code address from a returned call adds a frame. The immediate caller is
usually in x30 rather than on the stack, because a leaf entered by `bl` never
stores it. The scan never leaves SP's own 4 KiB block, since one cell further
could be the unmapped page above a thread stack and a SIGSEGV inside a SIGALRM
handler is not a diagnosis anybody can use.

The x29 chain was not built: it would cost every emitted word in the default
build two more instructions and a register.

### Cost

Measured against the same workload run with and without the clock, comparing user
CPU time across separate processes — wall time on a machine carrying other work
is far too noisy for a signal this small. At `20 prof-rate` (50 kHz) over
1,054,074 and 1,062,100 samples the profiled runs cost +0.98 s and +1.13 s of
user CPU against a 19.7 s baseline, i.e. **about 1 us per tick**, inside the 2 us
budget. At the 1 kHz default that is a tenth of a percent of wall time and sits
below the noise floor of a loaded machine. The handler allocates
nothing and takes nothing from the interrupted registers; its state lives in the
profiler band at the top of the DATA region and in an arena mapped once per
process, so it works inside a stripped image.

### Profiling the build

`tools/build-profile.f` runs the engine self-build under this profiler and
writes both reports to files:

```
bin/hb --load tools/build-profile.f -- /tmp/engine /tmp/build.txt /tmp/build.json
```

This is the method for "what does the build spend its time on". `strace -c` and
`perf record` cannot answer it on their own: neither can name a word the engine
compiled for itself, because it is all one anonymous mapping. The build's own
phase timers still go to stdout, so they sit beside the profile.

For the same source tree and host, its product is byte-identical to
`tools/native-build.f`. Capture stores DATA addresses in window coordinates,
and symbol-table growth clears retired storage, so the profiler's own DATA
allocations do not enter the artifact.

A caller line's share is of the row's own denominator: the exclusive count in the
flat section, where the edges come from, and the inclusive count in the inclusive
section, where it says how much of the row the known edges cover.

### Cross-checking with external perf

`perf record -g` keeps exact call chains where the profiler's conservative walk
can only approximate one, but it prints every Habu word as `[unknown]`: the
engine's code lives in one anonymous mapping perf has no symbols for.
`tools/perf-map.f` puts the names back:

```
perf record -g bin/hb --load prog.f
perf script | bin/hb --load tools/perf-map.f
```

Each call-chain line's address column becomes a package-qualified word plus a
byte offset — `(PROT-SPAN)+344` — resolved through `prof-pc>rec`, which is the
handler's own search, so the two profilers cannot disagree about which word owns
an address. Only the address column of an indented call-chain line is rewritten;
the sample header's pid and period are decimal runs that also read as hex and are
left alone. Addresses the profiled program compiled for itself, and everything in
a shared library, resolve to nothing and stay hex.

### Limits

- A profiled program that spawns and captures a child used to die, and no longer
  does. `poll(2)` is never restarted by `SA_RESTART`, so every tick that lands in
  it returns EINTR; the `poll` primitive used to collapse that to -1, so
  `lib/process.f` could not tell it from a real failure and raised
  `E-PROC-OUTPUT` — which stopped a `tools/native-build.f` run under `prof-on` at
  its final child-spawn phase. The primitive now returns `-errno` and the capture
  loop restarts on `-EINTR` against its existing deadline, so a profiled
  self-build runs end to end and needs no sample-limit workaround. A `prof-on`
  limit still reports and exits 99 at that many samples, so give a whole
  self-build a limit it cannot reach (or `0`) and call `prof-report` yourself.
- The handler runs on an alternate stack registered for the thread that called
  `prof-on`; a tick delivered to a `lib/task.f` thread runs on that thread's own
  stack.
- When the `n`-th sample is foreign the report waits for the next Habu sample, so
  a program that exits or stays blocked in a foreign call from that point on
  never reports; call `prof-report` yourself in that case.
