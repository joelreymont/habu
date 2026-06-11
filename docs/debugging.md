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

## lldb — native stepping (caf-built binaries)
lldb works on caf/standalone binaries (needs the admin password once). Reveals
load-time vs runtime kills. NB: an AMFI **signature cache** keys on the path/cdhash —
a binary that ran fine can be SIGKILLed at a path that previously held an invalid
signature. Write to a fresh path when in doubt.

## otool / python — disassembly
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
