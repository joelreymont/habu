# Debugging the self-hosted standalone

The standalone is JIT-compiled native ARM64; blind `.`-printing was the recurring
pain. Toolkit:

## `.s` — data-stack inspector (in the standalone)
`forth.fs` defines a `.s` primitive: prints the whole data stack (base..top), one
signed decimal per line, **non-destructively**. Interleave it to "step" through a
word and watch the stack — the practical stepper for standalone Forth. The loop
pointer lives in a DATA cell (`SSCR-CELL`) because the shared printer `g-print9`
clobbers x9..x15. The base is saved at startup into `S0-CELL`.

```
: GO 11 22 33 .s + + . ;   \ .s prints 11/22/33, then GO continues -> 66
```

## `.` — single value (in the standalone)
Pop + print one signed decimal + newline. Use for a specific intermediate.

## `step` — native token stepper (in the REPL, `bin/hb` on a tty)
`src/habu/stepper.f` is baked into `bin/hb`. `step 5 dup * 3 +` runs the rest
of the line one token at a time, echoing each token and printing the data stack
after it executes — no `EVALUATE` needed: the REPL hook feeds the engine one
token per call, so the engine's own interpret loop is the evaluator. The
gforth-host `STEP` (`bootstrap/cg/stepper.fs`, below) is the bootstrap-tier
equivalent.

## `BP+` / `BP-` — one-shot breakpoints on compiled words (REPL)
`src/habu/debug.f` (baked into `bin/hb`): `' WORD BP+` plants a `BRK #0` at the
word's entry. Hitting it prints `habu-bp:` + the pc + the data-stack top, then
restores the original instruction and **resumes** the word; the breakpoint is
one-shot. `' WORD BP*` is **persistent** (fires every call — the handler
emulates the entry prologue `sub sp,#16` by adjusting the ucontext sp/pc and
leaves the BRK planted, so no single-step is needed). `N ' WORD BPN` is
persistent but **silent for the first N hits** (skip-count). `BP-` removes;
`BP.` lists. Up to 8 at once. The SIGTRAP handler (`EMIT-TRAPH`) resumes via
`sigreturn` with the trampoline token; code is patched through the `patch32`
prim (RW→store→RX→isync, atomic from JIT-resident code). A full
Forth-predicate conditional would need signal-safe deferred evaluation; the
supported conditional breakpoint mechanism is skip-count (`BPN`).

## lldb — native stepping (habu-built binaries)
lldb works on habu/standalone binaries (needs the admin password once). Reveals
load-time vs runtime kills. NB: an AMFI **signature cache** keys on the path/cdhash —
a binary that ran fine can be SIGKILLed at a path that previously held an invalid
signature. Write to a fresh path when in doubt.

## STEP — single-step debugger (gforth host)
`bootstrap/cg/stepper.fs`: `s" 5 dup * 3 +" STEP` evaluates one token at a time, printing
the token and the data stack after each step, leaving the result. The "stepper".

## Forth disassembler (preferred over python/otool)
`bootstrap/cg/disasm.fs`: `DISASM ( addr nwords -- )` decodes habu's ARM64 subset to
mnemonics. Its decode math (`disasm-core.fs`) and the encoders (`asm-checked.fs`)
are written as CHECKED typed Forth — habu certifies them (CHECK-CODE=0). Use this to
inspect generated code; it found the walk-dispatcher bug (undefined `STR=`) instantly.

## otool / python — last resort
`otool -tv <bin>` or decode `__text` (offset 0x1000) as little-endian u32 in python.
Verify page hashes vs the embedded CodeDirectory the same way.

## Standalone gotchas a stepper catches fast
- A 2nd `{: :}` locals group mis-reads its slot (use a variable instead).
- Declaring locals inside `IF`/loop corrupts the frame.
- Plain `DO` is do-while (`0 0 DO` runs once); guard zero-trip loops.
- An UNDEFINED word compiles to a silent no-op (e.g. `0<` isn't a prim — `dup 0<`
  silently becomes `dup IF`). Watch for words not in `emit-prims`.
- The dictionary search returns the newest definition (fixed); decimal-only number
  parser; no `move`/`fill`/`emit`/`+!`/`0<`.
