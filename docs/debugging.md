# Debugging the self-hosted standalone

Tools and procedures for diagnosing native engine failures, from a load
refusal to a profile.

When checked code misbehaves, reduce the failure, find which layer is wrong
(declaration, checker, compiler or runtime) and fix that layer.

## Where a load refusal happened — read the message

A refusal the engine raises while reading source names the file and the line:

```
E-UNTERMINATED-STRING habu: /home/j/Work/habu/test/fixtures/x.f:12: bad string literal
```

No column. `<path>` is the resolved, canonical path of the **innermost open**
file, so a nested `require` names the file that actually holds the offending
token, not the entry the command line asked for. `<line>` counts newlines from
the start of the file.

A refusal with no include frame open names no place, `E-<class> habu:
<prose>`, because there is no file to name: the root source stream's own
text (src/habu/main.f `ROUTED`: stdin when `hb` has no argument, tty or piped,
or the `--load` and `--build` streams themselves) and the engine's own boot
prefix. The program file of a plain `hb <file>` is an open file and is named. A
string `evaluate`d *inside* a file keeps the file line only while it is a slice
of the file's own bytes (`parse-name X evaluate`). Any other evaluated string
names the file alone (`<path>: ` after `habu: `) for a refusal in it, and so
do the native compiler's `ncomp:` and `E-UNDEFINED` lines that follow that
refusal (src/core/checker.f SRCLOC:FILE-BUFFER?).

An undefined word, `E-UNDEFINED habu: <path>:<line>: undefined word 'X'`, is located like any
compile refusal on both loops. A loaded file that leaves cells prints
`E-EVAL-RESIDUE: stack depth N at <path>`, naming the whole file and so no line,
once at the innermost file, before the throw it raises (uncaught, that throw
ends the load). The other interpret-level refusals
(`E-LAYOUT-VALUE habu: <path>:<line>: interpret-mode layout value: X` and its
siblings) are located the same way. The checker's refusal in a body names
the definition and the token
(`habu: in g: at '2' expected: n actual: n n ; suggestion: Remove an extra producer or drop the surplus value.`);
the native line after it places the token
(`ncomp: cannot compile G at 2 in <path>:<line>`), and an undefined word adds
`E-UNDEFINED habu: <path>:<line>: undefined word 'x'`. A top-level refusal names the file and line
in its own line (`habu: <path>:<line>: at '!' …`), the file alone for a token of
an evaluated string that is not the file's own bytes, and no place outside a
file; a tool's label replaces the path of the file it labels. For those that
print no location, search the source for the named definition, or bisect.

## Reading an exit status

An uncaught throw in a `--load` or spawned child, or in an arm64 stripped (AOT)
application, runs the process-exit hook and then exits by its code. An x86-64
stripped application's uncaught throw does not yet run the hook or print the
report. A code from 1 to 255 is the exit status and prints nothing: `42 throw`
exits 42. Any other nonzero code prints `hb: uncaught throw code N` on stderr
and exits 67, so read the `E-*` from the message, not from the status. Under
`--load` an uncaught `0 throw` reads as success: it ends its file silently and
the run goes on; a stripped application prints `hb: uncaught throw code 0` and
exits 67. A refusal the checker rendered before throwing its code prints the same line after its
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

A probe in `src/habu/linker.f` shows only in a rebuilt engine:
`tools/hb-build.f` runs the linker inside `bin/hb`, not the source file.

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
(the supported conditional breakpoint). `BP-` removes, `BP.` lists.
Breakpoints exist on the ARM64 engines only: the x86-64 engine has no
breakpoint handler and bakes DEBUG without them. A hit
prints `habu-bp:` with the pc and stack top, `habu-bp-lr:` with the address the
word will return to, then the live data stack and watched cells, and resumes
the word. Feed the `habu-bp-lr:` value to `tools/code-owner.f` (below) to name
the caller: in a seeded engine that pair is the only way to answer "who called
this?", because lldb cannot plant a breakpoint in the compiled code region at all.
Each word takes the target as an xt, which must be an instruction-aligned entry
in the live compiled code region; engine-text primitives are refused.

`cp@` is stable only inside a compiled word: the interpreter compiles each
top-level line into a transient buffer at `cp@`, so a top-level `cp@ patch32`
clobbers the executing line (SIGILL). Write a runtime stub at `cp@` from inside
`: WORD … ;`. To verify emitted primitive bytes without running them, compute
the exact ARM64 encodings and search the on-disk `bin/hb` for the contiguous
stream — ASLR slides the xt, file bytes do not move.

## gdb/lldb — native stepping boundary

Use the Habu stepper, breakpoints, watch cells, `code-dump`, and `imgdump` first.
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
bin/hb --load tools/code-dump.f -- ': S ( n -- n ) dup * ;' S
bin/hb --load tools/imagedisasm.f -- bin/hb $1000 16
bin/hb --load tools/imgdump.f -- bin/hb
bin/hb --load tools/imgdump.f -- old-hb new-hb
```

For a baked Mach-O image, `imgdump` reports preferred virtual addresses:
remove the process's ASLR slide from a live PC before passing it to `--pc`.

## Which word owns this address — `tools/code-owner.f`

A debugger stop, a crash dump and a breakpoint all hand back raw addresses in the
compiled code region, where no external symbol table reaches. The dictionary has the
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

A kernel exit line that ends in `lr <lr>` reads
`<class> habu: <text>[ <value>] lr <lr>`, the value and `<lr>` in 16 hex
digits. `<lr>` is the return address the refusing primitive or guard was
called with, or the interrupted code's link register when a signal handler
exits.

Every VM stack is a guarded mapping and compiled code carries no bounds check,
so the message names the mechanism that caught the fault
(`src/habu/stack-abi.f` and `src/habu/crash.f` hold the rules):

- `E-UNDERFLOW habu: interpret stack underdepth: <token>` (exit 70; in a file
  the place comes before the prose: `<path>:<line>: `, or `<path>: ` in an
  evaluated string):
  the interpreter refused a token whose record states more inputs than the
  stack holds, or compiled code reached through `execute`, `catch`, `finally`
  or a quotation read below the data stack's base. Catchable; the REPL recovers the line.
- `E-UNDERFLOW habu: <path>:<line>: stack underflow at '<token>'` (exit 70): a word ran and left the stack below its
  base because its body consumes more than its record states: a word defined
  with checking off, or a `TRUSTED:` body that drops more than it declares.
- `E-STACK-BOUNDS habu: stack bounds exceeded (data|return|loop) lr <lr>`
  (exit 102): an overflow or a below-base access where no throw entry exists
  (a task's own stack, a stripped AOT image).
  Match the prefix `E-STACK-BOUNDS` unless the stack matters.
- `E-STACK-BOUNDS habu: stack bounds exceeded (machine) lr <lr>` (exit 102):
  the OS thread's own stack overflowed, typically unbounded recursion; `catch`
  does not receive it.
- `E-MEMORY-FAULT habu: memory fault at <address> pc <pc>` (exit 110): Habu
  code read or wrote unmapped or unwritable memory. It is thrown on the
  faulting thread, so `catch` receives 110 and a task ends with it; the line is
  written even when a catch takes the throw.
- `habu-crash regs [...]` and one hex value per line: any other fault: a
  SIGSEGV or SIGBUS in C code or a signal handler, a jump to an address outside
  Habu code or into a guard page, a second fault while one is delivered, and
  SIGILL, SIGTRAP and SIGFPE. After the dump the process dies by that signal
  at the faulting pc, with a core when RLIMIT_CORE allows (see "gdb/lldb"
  above).
- `E-STACK-UNGUARDED`: `run-in-stack` refused an extent that is not a guarded
  mapping, before the callback ran. Allocate the stack with
  `MEM:ALLOC-GUARDED`.

`test/stack-guard.f`, `test/engine-stack-machine.f` and
`test/engine-stack-wide.f` hold worked cases.

## Standalone gotchas a stepper catches fast

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
the surface and the report format). A word whose last call compiles as a tail
call is gone from the stack while its callee runs, so the
callee's samples show `(unknown)` or an outer caller instead of it. The usual
phase measurement:

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
- On arm64 a sample whose stack pointer lies on the main thread's stack scans
  every cell from it up to the stack's top, so every word with a frame there
  counts inclusively. Any other sample, and every sample on x86-64, scans at
  most `PROF-WALK-CELLS` cells and none past its 4 KiB block
  (`src/habu/prof.f` C-PROF-WALK). In a warm arm64 `--engine` build profile,
  `LINKER:WITH` took 19,660 inclusive samples of 19,663, none of them foreign.
- On arm64 a sample inside C (an FFI call, libSystem's `fsync`) is charged to
  the nearest Habu return address: the interrupted return register's, else the
  first code address the scan up from the stack pointer finds, to the stack's
  top on the main stack and within its 4 KiB block elsewhere. The scan is conservative, so a stale code-shaped cell left on
  the stack can take the sample. Its callers are walked from there. The
  header's `foreign charged to callers` says how many; `foreign` counts only
  the foreign samples charged to no word. On x86-64 every sample inside C
  counts in `foreign`, and the header has no `foreign charged to callers`.
- `n prof-row` prints one word's row whatever its rank.
- On arm64 only the thread that ran `prof-on` is sampled: a tick the kernel
  delivers to another thread is sent on to it, so a task's work shows only in
  a profile that task started. On x86-64 a tick counts on whichever thread it
  lands.
- On arm64 a sample counts the sampling intervals elapsed since the last one,
  so time blocked in a syscall (an fsync, a file read) is charged where the
  sample that ends it lands, though the ticks of that wait arrive as one
  signal. On x86-64 each signal counts one sample.
- A non-zero `prof-on` limit reports from inside the signal handler and exits
  99 when the sample total reaches or passes the limit: on arm64 the sample
  that reaches it may count several intervals. Words compiled after `prof-on` stay in the `defer`
  column of that report; a program that can reach its own `prof-off` should
  use one and call `prof-report`.

### Profiling the build

`tools/build-profile.f` runs hb's build under `--engine`, or a program's with a
source, for the host or `--target name` under the profiler and writes both
reports to files:

```
bin/hb --load tools/build-profile.f -- --engine /tmp/engine /tmp/build.txt /tmp/build.json
bin/hb --load tools/build-profile.f -- --target wasm app.f /tmp/app.wasm /tmp/app.txt /tmp/app.json
```

It samples every 100 us, so a warm build of 1.3 s gives about 13,000 samples
and takes a few percent longer. Read a row as a ranking only above about 100
samples, and repeat the run to compare a smaller one.

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
  sample that weighs an interval, so a program that exits or stays blocked in a foreign call from that
  point on never reports; call `prof-report` yourself.

## Timing a change

A before-and-after timing runs both sides on one idle host (spark, Linux
arm64), never the shared Mac, with one engine, interleaved in one session for
several rounds each, and compares instructions retired. Separate sessions
drift: zz's run phase moved 40M instructions (0.3%) between two sessions hours
apart with unchanged code, more than many single changes.
