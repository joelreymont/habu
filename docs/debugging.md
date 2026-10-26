# Debugging the self-hosted standalone

Tools and procedures for diagnosing native engine failures, from a load
refusal to a profile.

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

An undefined word, `E-UNDEFINED: X at <path>:<line>`, is located like any
compile refusal on both loops. A loaded file that leaves cells prints
`E-EVAL-RESIDUE: stack depth N at <path>`, naming the whole file and so no
line, once at the innermost file, before the throw it raises (uncaught, that
throw ends the load). The other interpret-level diagnostics
(`hb: interpret-mode layout value: X` and
its siblings) and the refusals that exit without the compile tail (boot
source, CLI, uncaught throw, snapshot and AOT families) print no location.
Neither do the checker's diagnostics: they name the definition and the token
(`habu: in g: at '2' expected: n actual: n n`, and a tier-1 undefined word in
a checked body as `E-UNDEFINED habu: in f: undefined word 'x'`) but no file or line. For
all of these, search the source for the named definition, or bisect.

## Reading an exit status

An uncaught throw in a `--load` or spawned child, or in an arm64 stripped (AOT)
application, runs the process-exit hook and then exits by its code. An x86-64
stripped application's uncaught throw does not yet run the hook or print the
report. A code from 1 to 255 is the exit status and prints nothing: `42 throw`
exits 42. Any other code prints `hb: uncaught throw code N` on stderr and exits
67, so read the `E-*` from the message, not from the status. Code 0 differs by
entry: under `--load` the loader rethrows only a nonzero code, so `0 throw`
ends that file's load and the process exits 0, while a plain `hb file.f` or
source on stdin prints `hb: uncaught throw code 0` and exits 67. A refusal the
checker rendered before throwing its code prints the same line after its
diagnostic and exits 70, the status of every refusal; a stripped application
carries no checker, so it never takes that exit. A one-byte diagnostic and a
clean exit means a raw engine capacity path, not a throw.

## `.s` and `.` — stack inspection

`.s` prints the whole data stack (base..top), one signed decimal per line,
**non-destructively**. Interleave it to "step" through a word and watch the
stack:

```
: GO 11 22 33 .s + + . ;   \ .s prints 11/22/33, then GO continues -> 66
```

For build-time underflow, also probe with `depth .`: `.s` walks the saved
base-to-top range and can hide a negative depth after a native emitter corrupts
the build stack. `.` pops and prints one value, for a specific intermediate.

## The debugger: watched cells, `step`, breakpoints

The engine bakes the debugger as package DEBUG (`src/habu/debug.f` lists its
commands). `using DEBUG`, once in a session or at the head of a program, opens
its commands for bare use; a one-off call qualifies the name instead, as in
`' WORD DEBUG:BP+`. Every example below assumes `using DEBUG` ran.

**Watched cells.** `BPW+` adds a cell address, `BPW-` removes it, `BPW-CLEAR`
clears all, `BPW.` lists `address value` pairs. Both `step` and breakpoints
print the watch table. For fixed engine cells:

```
DATAB ENVP-CELL + BPW+
DATAB ARGV-CELL + BPW+
```

**`step`** (in the REPL, `bin/hb` on a tty): `step 5 dup * 3 +` runs the rest
of the line one token at a time, echoing each token and printing the data stack
and watch table after it executes.

**Breakpoints on compiled words.** `' WORD BP+` is one-shot, `' WORD BP*`
fires on every call, and `N ' WORD BPN` fires on every call after the first N
(the supported conditional breakpoint). `BP-` removes, `BP.` lists. A hit
prints `habu-bp:` with the pc and stack top, `habu-bp-lr:` with the address the
word will return to, then the live data stack and watched cells, and resumes
the word. Feed the `habu-bp-lr:` value to `tools/code-owner.f` (below) to name
the caller: in a seeded engine that pair is the only way to answer "who called
this?", because lldb cannot plant a breakpoint in the JIT region at all.
Each word takes the target as an xt, which must be an instruction-aligned entry
in the live compiled code region; engine-text primitives are refused.

`cp@` is stable only inside a compiled word: the interpreter compiles each
top-level line into a transient buffer at `cp@`, so a top-level `cp@ patch32`
clobbers the executing line (SIGILL). Write a runtime stub at `cp@` from inside
`: WORD … ;`. To verify emitted primitive bytes without running them, compute
the exact ARM64 encodings and search the on-disk `bin/hb` for the contiguous
stream — ASLR slides the xt, file bytes do not move.

## gdb/lldb — native stepping boundary

Use the Habu stepper, breakpoints, watch cells, `jitdump`, and `imgdump` first.
Use gdb on Linux and lldb on macOS only when the fault is in startup or emitted
machine code before the Forth debugger is reachable. Inspect data-stack cells,
DATA header cells, and watch-cell addresses before adding print probes. On macOS,
lldb may need the admin password once. AMFI signature cache keys on the
path/cdhash, so a binary that ran fine can be SIGKILLed at a path that previously
held an invalid signature. Write to a fresh path when in doubt.

A fault the crash handler does not name prints the `habu-crash regs` dump on
fd 2 and then ends by its own signal, delivered again at the faulting pc
(`src/habu/crash.f` C-CRASH-RERAISE, `src/habu/boot-x64.f` DUMP,): the parent
sees the signal (a shell shows 128 + n, 139 for SIGSEGV), and a core or a
debugger shows the faulting pc, sp and caller frames, not the handler's. A core
is written only when RLIMIT_CORE allows it: `ulimit -c unlimited` before the
run. Linux writes it where `/proc/sys/kernel/core_pattern` says
(`coredumpctl` when that is systemd-coredump). macOS writes it to
`sysctl kern.corefile` (`/cores/core.%P`), and `/cores` is root-owned mode 755,
so a core needs it made writable by an administrator first. Without a core,
run the child under lldb with
`settings set platform.plugin.darwin.ignored-exceptions EXC_BAD_ACCESS` and
`process handle SIGSEGV -s true -p true` before `process launch`; without the
setting lldb catches the Mach exception itself and `continue` discards it, so
the handler never runs. The first stop (`signal SIGSEGV`) is the fault before
the handler runs; after `continue` the handler prints the dump, and the
re-delivered SIGSEGV stops at the same pc and sp, where `bt` or the return
address at `[sp]` shows the callers.

## Forth disassembler (preferred over external disassemblers)

Use the in-process dumps to inspect generated code before falling back to
external tools:

```
bin/hb --load tools/jitdump.f -- ': S ( n -- n ) dup * ;' S
bin/hb --load tools/imagedisasm.f -- bin/hb $1000 16
bin/hb --load tools/imgdump.f -- bin/hb
bin/hb --load tools/imgdump.f -- old-hb new-hb
```

For a baked Mach-O image, `imgdump` reports preferred virtual addresses:
remove the process's ASLR slide from a live PC before passing it to `--pc`.

## Which word owns this address — `tools/code-owner.f`

A debugger stop, a crash dump and a breakpoint all hand back raw addresses in the
JIT region, where no external symbol table reaches. The dictionary has the
answer:

```sh
<engine> --load tools/code-owner-main.f -- '$181954'
```

The argument is a **region offset**, not an address: ASLR moves the region
every boot, and `region-off=` is what one run can hand another. Run it inside
the engine under study, the only process where those records exist.

## Dictionary / xref inspection

Semantic dictionary inspection is a live-image Forth surface, not external text
search. `src/habu/xref.f` is baked into `bin/hb`; use `XREF`, `SEE` and
`WORDS` before source search when debugging dictionary ownership.

## Who owns a persisted DATA cell — `tools/snap-heap-owner.f`

When two builds of the same snapshot image differ, the differing byte offsets say
*where* a stale pointer sits but not *whose* it is, and guessing an owner from
what the cell contains is how this class of bug gets papered over. This tool
answers from the dictionary: `SNAP-HEAP-OWNER:DUMP` maps heap offsets to owner
words, `SNAP-HEAP-OWNER:CODE-MAP` maps code offsets to words.

It has to run inside a process that has the source under investigation loaded.
A snapshot build leaves that process's two inputs in `HB_TMP`: the native
engine `hb-native` and the snapshot source `hb-snap-src`, whose last line calls
`APP-IMAGE:SAVE`. Run that engine on the same source with the dump in place of
the save:

```sh
HB_TMP=<private-root> bin/hb --load tools/build-fixpoint-refresh.f -- snap
{ grep -v 'APP-IMAGE:SAVE' <private-root>/hb-snap-src
  printf '%s\n' 'require tools/snap-heap-owner.f' 'SNAP-HEAP-OWNER:DUMP'
} | HB_TMP=<private-root> <private-root>/hb-native > owners.txt
```

Pair the heap map against the offsets `cmp -l` reports between two images
built from one `hb-stdin` and one `hb-snap-src` to name every drifting cell
above the engine-reserved band.

## Is this heap cell a persisted pointer or a live one — the ASLR intersect

A snapshot image carries every cell of the DP heap, so a cell holding an
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

A cell that holds an out-of-band address (neither the DP heap at `DATA-VA` nor
this run's code region) **and holds the same value in both dumps** is
persisted, and it is a defect. A cell whose value differs between the two dumps
is this process's own and is fine. The pipe held open by `sleep` is what gives
a booted, quiescent process to attach to; the trivial `7 .` only proves the
engine reached its REPL.

Run the same intersect against an image built from a known-good tree and compare
the two sets **by owner name** (`SNAP-HEAP-OWNER:DUMP`, above): what the suspect
image carries and the good one does not is the regression, and everything in both
is a pre-existing cell the good image already ships green. A persisted
`malloc`ed arena base makes the warm image's first store through it die
`EXC_BAD_ACCESS` on an unmapped page, but not on every run: when ASLR leaves
that address mapped, the store lands in live memory and the program "passes".
A nondeterministic pass rate is itself a symptom of this class, not noise to be
re-run away.

Two ways this class hides:

- **A double load re-seeds it.** If the build loads the owning source twice, the
  second load re-runs `variable X-P X-BOOT X-P !` and the persisted copy is the
  fresh one. The defect is still there; only the accident is. Removing a
  redundant load is therefore a change that can expose persisted-pointer bugs
  anywhere in the tree, and this intersect is how to check.
- **The owner map does not name it.** The owner map lists only live `create`
  and `variable` records; `does>` clears their stamp. An unnamed cell may
  therefore fall far above the preceding owner. Treat a big offset as
  "unnamed", not as that word's field.

## Stage0 mirror vs native engine — which engine is actually running

Two independent engines compile the prefix, and a defect can live in one and be
invisible to the other. `src/habu/habu2.f` is the native engine, baked into an
installed `bin/hb`. `bootstrap/cg/forth.fs` is the Gforth-hosted mirror that
builds `hb-stage0` during recovery. The mirror is meant to stay byte-for-byte
equivalent to the native engine, and nothing currently proves that it does.

`tools/bootstrap.sh` builds `hb-stage0` with the mirror and then runs it. That
run is the first time a native binary loads the prefix, and it happens before
any natively built engine exists. So a mirror-only defect stops the bootstrap
outright, while every test that boots a child engine from an installed
`bin/hb` keeps passing: those tests exercise the native engine and structurally
cannot see the mirror.

When a failure appears during `tools/bootstrap.sh` but the matching test suite is
green, suspect this split before suspecting the checker. Identify the engine by
where the failure lands: a diagnostic printed by the `hb-stage0` run, before
any `stage2:` message, is the mirror; the same source failing under
`bin/hb --load` is the native engine.

### Reproducing a mirror-only prefix defect

Patch the working tree and let `tools/bootstrap.sh` drive it. No separate seed
builder is needed — the script already assembles the prefix in the right order,
and the failure surfaces within seconds, at the `hb-stage0` run. Add the
smallest checked probe that exercises the suspect path at the earliest prefix
file where it is legal, run the recovery launcher, and restore the files
afterwards:

    HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth tools/bootstrap.sh

An installed `bin/hb` re-reads the prefix from disk at boot, so the same patch
under any `bin/hb --load <file>` prints the native engine's answer without a
rebuild. Compare the two.

### Building the seed directly

Instrumenting `src/core/checker.f` changes what `test/bootstrap-wide-memory.fs`
measures, so `tools/bootstrap.sh` then stops in its first gate and never
reaches the stage0 run. Instrumented runs must therefore skip the launcher and
build the seed directly. The seed is exactly the file `tools/bootstrap.sh`
writes to `$HB_TMP/stage2-src`, used as written: its boot-hide prologue is
load-bearing (below). Build and boot it with:

    HABU_TARGET=<target> gforth -e 'require test/nf.fs s" <seed>" slurp-file s" <out>" FORTH-BUILD-EXE bye'
    HB_TMP=<dir> <out> -- <dir>

A boot that reaches `stage2: cannot open source` (exit 74) got through the whole
prefix; that message is success for this purpose. Copy `bootstrap/` and
`test/nf.fs` into a scratch directory before instrumenting the mirror, and
point Gforth at the copy, so the repository tree stays clean.

### The recovery engine reads the prefix twice

Keep this in mind for any probe placed in the boot prefix. The emitted engine
reads every prefix file from disk when it starts, then interprets its baked
program, which for a `FORTH-BUILD-EXE` binary is the whole prefix again plus a
driver. So every top-level action in a prefix file happens twice, in two
different checker instances, and a census printed from the prefix answers about
whichever load is running. Two markers tell the loads apart in a trace: only
the startup load runs `src/core/include.f`, and only the baked program runs
`src/habu/habu1.f`.

The second load must not inherit the first load's words: the boot-hide prologue
(`tools/bootstrap.sh`, through `src/habu/hide.f`) hides the startup load's
dictionary and clears its recorded effects. Without it, checker words from the
startup load stay resolvable while `checker.f` is re-read, so declarations
publish into the checker that is being replaced, and a later checked use fails
with `hook: non-certified definition: ...` and exit 70.

The pending tables and their replay are assembly in both engines, so ordinary
`type`/`.` probes cannot reach them. Two techniques cover it without
print-bisecting:

- Instrument the checker end in Habu: `type` the name at the head of the
  checker words the replay calls, and bracket the replay with markers. If the
  markers print with nothing between them, the replay never reached the
  checker. Put the engine-side and checker-side probes on the same file
  descriptor so their order is evidence; a second answer for the same name
  later in the stream shows two loads, not a broken replay.
- Instrument the engine end by making an existing fail-closed exit fire where
  you want a probe: it prints the current token and exits, so it needs no new
  string labels.

## External disassembly — last resort

Use external disassemblers only when the native disassembler lacks an encoding.
On Linux, `objdump -d` or `readelf -l` can inspect ELF text and load segments.
On macOS, `otool -tv` can inspect `__text`; verify page hashes against the
embedded CodeDirectory when signature behavior is involved.

## A child-process fixture disagrees with itself — `tools/launch-context.f`

A fixture that spawns a child and asserts its exit code reports a bare number
when it fails (`expected 73 got 70`), which is what makes this class look
environmental: the child printed the reason and the fixture threw it away. Get
the reason before theorising about the launcher.

- Wire every child-rc assertion in a fixture through a helper that calls
  `SPAWN-REPORT:CHILD` (`src/lib/test/spawn-report.f`) on a mismatch: it prints
  the wanted and got rc, the captured stdout/stderr and the launch context.
- `bin/hb --load tools/launch-context.f` prints only the context, so the same
  report can be taken under different launchers and diffed. Every line starts
  `ctx `; a stale `PWD` shows up as a yes/no split between cwd and `PWD`.

```sh
bin/hb --load tools/launch-context.f | grep '^ctx ' | sort > /tmp/pipe.txt
script -q /tmp/tty.log bin/hb --load tools/launch-context.f >/dev/null
grep '^ctx ' /tmp/tty.log | sort > /tmp/tty.txt
diff /tmp/pipe.txt /tmp/tty.txt
```

Before concluding "environment", check the cheaper explanations: the test may
not be registered in `test/gate-stdlib-cases.f`, and a fixture that asserts a
specific exit code may simply be asserting a code the tree stopped producing.

## Stack diagnostics — which one you are looking at

Every VM stack is a guarded mapping and compiled code carries no bounds check,
so the message names the mechanism that caught the fault
(`src/habu/stack-abi.f` and `src/habu/crash.f` hold the rules):

- `hb: interpret stack underdepth: <token>` (exit 70): the interpreter refused
  a token whose record states more inputs than the stack holds, or compiled
  code reached through `execute`, `catch`, `finally` or a quotation read below
  the data stack's base. Catchable; the REPL recovers the line.
- `E-UNDERFLOW: <token>` (exit 70): a word ran and left the stack below its
  base because its body consumes more than its record states: a word defined
  with checking off, or a `TRUSTED:` body that drops more than it declares.
- `hb: stack bounds exceeded (data|return|loop)` (exit 102): an overflow, a
  jump into a guard page, or a below-base access where no throw entry exists
  (a task's own stack, a stripped AOT image, the x86-64 engine). Match the
  prefix `hb: stack bounds exceeded` unless the stack matters.
- `hb: stack bounds exceeded (machine)` (exit 102): the OS thread's own stack
  overflowed, typically unbounded recursion; `catch` does not receive it.
- `habu-crash regs [...]` and one hex value per line: any other fault
  (SIGSEGV, SIGBUS, SIGILL, SIGTRAP, SIGFPE). After the dump the process dies
  by that signal at the faulting pc, with a core when RLIMIT_CORE allows (see
  "gdb/lldb" above).
- `E-STACK-UNGUARDED`: `run-in-stack` refused an extent that is not a guarded
  mapping, before the callback ran. Allocate the stack with
  `MEM:ALLOC-GUARDED`.

A tier-1 top-row warning does not change which of these a program hits; it
only warns once first. `test/stack-guard.f`, `test/engine-stack-machine.f` and
`test/engine-stack-wide.f` hold worked cases.

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

## Sampling profiler

`n prof-on` samples the running program and attributes each tick to the word
whose code holds the interrupted pc, with its callers (`src/habu/prof.f` states
the surface and the report format). The usual phase measurement:

```
1000 prof-rate  0 prof-on  <work>  prof-off  prof-report
```

- `prof-off` stops the clock and keeps every counter, so the phase just
  measured can be reported afterwards; `prof-reset` clears the counters for a
  second phase without rebuilding the index.
- `prof-report` prints the top rows twice: by exclusive count, then under
  `by inclusive`. The second section shows a phase word, such as a compiler
  pass, that does no work of its own. `prof-json` prints every attributed word
  and caller edge.
- `n prof-row` prints one word's row whatever its rank.
- A non-zero `prof-on` limit reports from inside the signal handler and exits
  99 at that many samples. Words compiled after `prof-on` stay in the `defer`
  column of that report; a program that can reach its own `prof-off` should
  use one and call `prof-report`.

### Profiling the build

`tools/build-profile.f` runs the engine self-build under the profiler and writes
both reports to files:

```
bin/hb --load tools/build-profile.f -- /tmp/engine /tmp/build.txt /tmp/build.json
```

This is the method for "what does the build spend its time on". `strace -c` and
`perf record` cannot answer it on their own: neither can name a word the engine
compiled for itself, because it is all one anonymous mapping.

### Cross-checking with external perf

`perf record -g` keeps exact call chains where the profiler's conservative walk
can only approximate one, but it prints every Habu word as `[unknown]`.
`tools/perf-map.f` puts the names back through the profiler's own search:

```
perf record -g bin/hb --load prog.f
perf script | bin/hb --load tools/perf-map.f
```

### Limits

- A profiled program may spawn and capture children; the capture loop restarts
  its `poll` on EINTR against its existing deadline.
- A tick delivered to a thread C started that is not inside a callback runs on
  that thread's own stack.
- When the limit's last sample is foreign the report waits for the next Habu
  sample, so a program that exits or stays blocked in a foreign call from that
  point on never reports; call `prof-report` yourself.
