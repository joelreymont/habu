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
bin/hb --load src/os/linux/layout.f src/habu/layout.f tools/imgdump.f -- bin/hb
bin/hb --load src/os/linux/layout.f src/habu/layout.f tools/imgdump.f -- old-hb new-hb
```

## External disassembly — last resort
Use external disassemblers only when the native disassembler lacks an encoding.
On Linux, `objdump -d` or `readelf -l` can inspect ELF text and load segments.
On macOS, `otool -tv` can inspect `__text`; verify page hashes against the
embedded CodeDirectory when signature behavior is involved.

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
