# Trusted base — the `TRUST` manifest

`TRUST` is the checker's escape hatch. `s" name" s" effect" TRUST` declares a
word's stack effect *without* checking its body, so callers are verified against
the declared signature but the body is taken on faith. Every trusted word is part
of the **trusted base**: a soundness cliff. If a declared effect is wrong, the
checker will happily certify programs built on a lie, and the error surfaces far
from its cause. Trust only what *cannot* be inferred — host primitives, raw code
emitters, `CREATE`/`DOES>`, and recursion — and keep the set as small as possible.

Why these words defeat inference: they are the **engine emitters**. Their bodies
are sequences of raw ARM64 encodings (`LBL`, `BL,`, `ADR,`, `STR,`, …) and
`xt execute` calls through token-passed handlers. The checker reasons about Forth
stack effects, not about machine-code labels or indirectly-executed handler xts,
so it cannot derive an effect for them. Their correctness is pinned instead by:

- **Native self-rebuild tests** — the standalone emitter rebuilds the engine from
  current source and compares the next stage against the previous native stage.
  Any drift fails.
- **Behavioral tests** — the emitted engine is run on real programs and its stdout
  is compared against the expected result.

`Last audited` is the date a human last confirmed the declared effect matches the
body. Re-audit when a row's body or effect string changes.

`tools/trust-lint.f` repository mode enforces this manifest for every `TRUST`
site in the default scanned roots, `src/` and `lib/`, and every row must cite a
test. `tools/check.f --source-list` also invokes `trust-lint source-only` on each
named input path, so source-list certified tool, test, and benchmark sources use
the same manifest rows below. Rows outside `src/` and `lib/` are validated when
that source is explicitly certified; they are not stale-checked by the default
`src/`/`lib/` repository scan.

| Word | Effect | Reason | Tests | Site | Last audited | Class | Owner |
|------|--------|--------|-------|------|--------------|-------|-------|
| STDIN? | `-- ptr bool` | Engine-builder mode cell that checked drivers set before emitting stdin or file-backed startup behavior. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/habu1.f | 2026-06-26 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| FP-EMIT | `--` | Metabuild primitive-body emitter: runs the per-primitive data-driven body-emitter xt (`FP-XT`) to lay down raw machine code. The emitter varies per prim and is not typed Habu, so it is confined to this named boundary instead of the checker's opaque-execute reject (`E-EXEC-OPAQUE-XT`, dot `habu-checker-exec-of-5923c543`). | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/habu1.f | 2026-07-19 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| tok-imm? | `ptr u8 n -- n` | Engine primitive axiom: LFIND the token in the live dictionary and push flags&2 (the DNAME-IMM bit), so DO-TOK1 can reject a signature-carrying live immediate as a checked body step (p5 wrong-certificate class, dot habu-checker-fitting-arity-70dc94e4). | `test/immediate-model-test.f` (stdlib/tail-process fork, test/run.f) | src/core/checker.f | 2026-07-13 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| parse-imm | `ptr u8 n n --` | Declares a parsing immediate's compile-time payload token count to the checker (GRID: 1, WHERE 3), exempting it from the p5 immediate reject and skipping its payload in the body scan. A wrong count skips live code, so each declaration site is an audited soundness boundary; UNSAFE-TOK? bars it from checked bodies (top-level only). | `lib/ptx/header-test.f` (lint-libs slice + resident ptx group), `test/lower-txn-protection.f`, `test/run.f` | src/core/checker.f | 2026-07-13 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| BPW-TAB | `-- ptr ptr n` | Watch-table storage is dictionary data whose cells hold watched DATA pointers; the checker cannot infer this created table's pointee role. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug-watch.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BPW-PRINT-ADDR | `ptr n --` | Debug watch printer intentionally displays a raw cell address; formatting a pointer through `.` is a REPL/debug boundary. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug-watch.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BPW-DATA-CELL | `n -- ptr n` | Converts a fixed DATA cell offset to a typed numeric-cell address for watch registration. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug-watch.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BP-SLOT-ADDR | `n -- ptr ptr u8` | Breakpoint slots live in fixed DATA and store code pointers; slot field typing is outside arithmetic inference. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BP-SLOT-INSTR | `n -- ptr n` | Breakpoint slots store the saved 32-bit instruction word in fixed DATA. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BP-SLOT-HITS | `n -- ptr n` | Breakpoint slots store hit counters in fixed DATA. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BP-SLOT-CTRL | `n -- ptr n` | Breakpoint slots store packed skip/persistent control bits in fixed DATA. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BP-NULL | `-- ptr u8` | Debug slot zero is used as a null code pointer sentinel; the checker has no null-pointer literal role. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BP-PRINT-ADDR | `ptr u8 --` | Breakpoint listing intentionally prints raw code pointers through the numeric printer. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BP-PATCH32 | `n ptr u8 --` | Breakpoint installation patches executable code with a 32-bit BRK/restored instruction; code mutation is a native debug boundary. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BP-XT>PTR | `n -- ptr u8` | A ticked xt is represented as the target code address; the checker cannot refine the cell to a code pointer. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-N>REC | `n -- ptr a` | Refresh prelude converts numeric dictionary-record addresses into opaque record pointers before truncating stale engine definitions; raw dictionary layout is the boundary. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-A>U8 | `ptr a -- ptr u8` | Refresh prelude treats inline dictionary-name bytes as a byte string while finding the truncation marker. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-N>U8 | `n -- ptr u8` | Refresh prelude refines the numeric long-name pointer stored in a dictionary record into a byte pointer while finding the truncation marker. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-USIG-END-PTR | `-- ptr a` | Refresh prelude refines the checker signature-table terminator address so it can rewrite the reset sentinel. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-UEND! | `n --` | Refresh prelude resets the checker signature cursor before reloading the current checker model; the cursor cell is checker-internal raw state. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-NDICT! | `n --` | Refresh prelude mutates the live dictionary cursor after locating the reload marker; dictionary truncation is the explicit refresh boundary. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-CHECK-OFF | `--` | Refresh prelude disables the currently baked checker before reloading the current checker source; the raw `set-check` token is unsafe once strict checking is active. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-A@ | `-- ptr u8` | Refresh prelude reads a byte-string scratch pointer from a generic pointer cell while matching dictionary names. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-B@ | `-- ptr u8` | Refresh prelude reads a second byte-string scratch pointer from a generic pointer cell while matching dictionary names. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-SN@ | `-- ptr u8` | Refresh prelude reads the searched-name byte pointer from a generic pointer cell while finding the reload marker. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-A! | `ptr u8 --` | Refresh prelude stores a byte-string scratch pointer into a generic pointer cell while matching dictionary names. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-B! | `ptr u8 --` | Refresh prelude stores a second byte-string scratch pointer into a generic pointer cell while matching dictionary names. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-SN! | `ptr u8 --` | Refresh prelude stores the searched-name byte pointer into a generic pointer cell while finding the reload marker. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| XREF-N>REC | `n -- ptr a` | Converts a numeric live dictionary-record address into an opaque record pointer for checked xref helpers; the record base comes from `dbase@` plus `DREC` arithmetic. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f | 2026-06-27 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| XREF-A>U8 | `ptr a -- ptr u8` | Treats the inline-name bytes inside a dictionary record as a byte string; fixed raw record byte offsets are outside pointer-role inference. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f | 2026-06-27 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| XREF-N>U8 | `n -- ptr u8` | Converts a numeric long-name address fetched from a dictionary record into a byte pointer; the record stores mixed numeric and pointer cells. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f | 2026-06-27 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| XREF-PATCH32 | `n ptr a --` | Explicit `undefine` retires dictionary records by patching raw wordlist/status cells inside the live dictionary; the record layout is outside checked pointer inference. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SEAL-LATCH@ | `-- n` | Reads the friend-arena seal latch from the sealed DATA band by raw `data-base` offset; a raw state cell (0 open / sealed) outside checked pointer/role inference. Used by the FORGET/HIDE truncation guard. | `test/seal.f`, `test/run.f` | src/habu/xref.f | 2026-07-05 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SEAL-NDICT@ | `-- n` | Reads the seal-time ndict truncation watermark from the sealed DATA band by raw `data-base` offset; a raw state cell outside checked inference. Used by the FORGET/HIDE truncation guard. | `test/seal.f`, `test/run.f` | src/habu/xref.f | 2026-07-05 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-crash-entry | `--` | Target signal entry register shuffle is raw ABI-specific ARM64; it only mutates generated registers. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-crash-mctx>r21 | `--` | Target ucontext-to-mcontext addressing is ABI-specific raw register code. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-crash-xreg>r9 | `--` | Crash dump register extraction walks target mcontext layout in generated registers. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-crash-pc>r9 | `--` | Crash dump PC extraction reads target-specific mcontext fields in generated registers. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-crash-print-regs | `--` | Crash handler emits target-specific FP/LR/SP/PC fields through raw register/syscall code. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-crash-pc-word | `n --` | Crash diagnostics bounds-check a saved-PC-relative word against the fixed code mapping before raw instruction loads. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-crash-pc-8 | `--` | Crash diagnostics request the guarded instruction word at saved PC minus 8. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-crash-pc-4 | `--` | Crash diagnostics request the guarded instruction word at saved PC minus 4. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-crash-pc0 | `--` | Crash diagnostics request the guarded instruction word at the saved PC. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-crash-pc+4 | `--` | Crash diagnostics request the guarded instruction word at saved PC plus 4. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-trap-mctx>r9 | `--` | SIGTRAP handler target ucontext-to-mcontext addressing is raw ABI-specific ARM64. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-mctx-pc>r10 | `--` | SIGTRAP handler reads target-specific PC fields from mcontext in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-mctx-x19>r12 | `--` | SIGTRAP handler reads the target data-stack register from mcontext in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-mctx-sp-16! | `--` | Breakpoint resume emulates the compiled prologue by mutating target mcontext SP. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-mctx-pc+4! | `--` | Breakpoint resume skips the BRK instruction by mutating target mcontext PC. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-bp-hit-save | `--` | Breakpoint hit handling saves handler scratch registers and updates fixed DATA slot hit counters in generated code. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-bp-print-hit | `--` | Breakpoint hit reporting prints raw PC and stack-top values from target mcontext/register state. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-bp-stack-range | `--` | Breakpoint stack dumping derives raw stack bounds from fixed DATA and mcontext state. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-bp-watch-head | `--` | Breakpoint watch dumping reads fixed DATA watch metadata and emits a signal-safe header. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-bp-watch-row | `--` | Breakpoint watch dumping reads one raw watched pointer/value pair in generated code. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-bp-restore-oneshot | `--` | One-shot breakpoint restore mutates executable code, flips page permissions, and flushes I-cache. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-bp-emulate | `--` | Persistent/skip breakpoint resume emulates a compiled-word entry prologue in target mcontext. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-bp-scan | `label label label label --` | Breakpoint table scan emits branches to caller-provided labels and leaves hit-slot state in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-bp-stack-dump | `label label --` | Breakpoint stack dump emits a caller-labelled loop over raw data-stack cells. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-bp-watch-dump | `label label --` | Breakpoint watch dump emits a caller-labelled loop over raw watched cells. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-emit-tty-probe | `--` | Startup source selection emits target-specific tty ioctl setup; the syscall/register effects are not Forth stack effects. | `test/proc-pty.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| c-emit-drop-x12 | `--` | Control-flow local-scope restore emits a raw `add sp, sp, #bytes` teardown from generated register state. | `test/engine-suite.f`, `test/run.f` | src/habu/habu2.f | 2026-06-29 |  |  |
| c-dup-def-fail | `--` | Duplicate-definition failure emitter writes the fixed diagnostic and pending definition token, then exits with the duplicate-definition code. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-reject-dup-def | `--` | Definition-publish guard scans the active target wordlist case-insensitively before dictionary mutation and branches to the duplicate-definition failure emitter. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-qualify-def | `--` | Definition-time namespace qualifier emitter: rewrites the pending token to the qualified tail, creates namespace wordlist records, and exits on malformed qualification through raw runtime code. | `test/gate-dictionary.f`, `tools/xref-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-store-def-name | `--` | Stores the pending dictionary name and qualified wordlist after `c-qualify-def`, then restores the full captured token for checker publication. | `test/gate-dictionary.f`, `tools/xref-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-defer-die-token | `n --` | Deferred-word failure emitter writes the current token and exits with the supplied execution-vector code from generated compiler/runtime paths. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| c-defer-find-unset | `--` | Deferred-word creation resolves the shared `DEFER-UNSET` sentinel xt through the raw target dictionary. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| c-defer-cell | `--` | Deferred-word creation allocates the vector cell and seeds it with the unset sentinel xt in raw target data space. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| c-defer-emit-code | `--` | Deferred-word code emitter writes the wrapper that loads the vector cell and branches to the stored implementation xt. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| c-defer-meta-write | `--` | Deferred-word publisher appends the magic marker and vector-cell address used by compile-time `is` validation. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| c-defer-room | `--` | Deferred-word creation emits raw dictionary/code-space capacity checks before mutating compiler state. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| c-defer | `--` | Interpreter `defer` definer consumes the name/signature, creates the wrapper and metadata, and publishes the declared checked effect. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| c-defer-target-meta | `--` | Compile-mode `is` validation resolves the deferred target and rejects non-deferred words before code emission. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| j-is | `--` | Compile-mode `is` emitter spills the typed quotation xt and stores it into the deferred word's vector cell. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| c-pd-copy | `--` | Pre-trust defer capture byte-copy helper: copies x5 bytes of name/sig into a pending-table slot in raw target data space. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-16 |  |  |
| c-pd-die-full | `--` | Pre-trust defer table overflow / over-cap emitter writes the offending defer token and exits 72. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-16 |  |  |
| c-pd-capture | `--` | Records a defer declared before `trust`/`checker-defer` exist: copies its qualified name and effect signature into the pending table for later replay. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-16 |  |  |
| c-pretrust-ready? | `--` | Non-dying probe of `trust`/`checker-defer` findability; selects the pending-capture vs inline-registration branch in `defer`. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-16 |  |  |
| bdrainpretrust | `--` | `DRAIN-PRE-TRUST-DEFERS` prim body: replays the trust and checker-defer registrations for every pending pre-trust defer, then empties the table. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-16 |  |  |
| c-lbrace-die | `--` | Locals-placement guard writes the fixed diagnostic and exits when a `{:` local is declared inside a quotation. | `test/gate-diagnostics.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| EM-HXT-EXECUTE | `n --` | Narrow higher-order emitter boundary: checked dispatcher words pass one build-time emitter xt through this raw `execute` shim. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/habu2.f | 2026-06-26 |  |  |
| c-local-ref | `label label --` | Compile-mode local-reference emitter: branches to the caller's not-local continuation or emits local loads, and rejects quotation-local captures with raw exit code 75. | `test/engine-suite.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| EM-DATA-VA>N | `-- n` | Engine-builder raw emitter boundary: exposes the fixed DATA-VA pointer as the numeric immediate needed by `LIT64,` when emitting the startup mmap check. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/habu2.f | 2026-06-26 |  |  |
| em-interpret-colon | `label --` | Emits interpreter-mode colon handling and kernel-colon setup before falling through to word dispatch. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| C-FIND-GLOBAL | `ptr n n --` | Package checker bridge resolves core checker words from the global wordlist while preserving active package cells, so package-local words cannot shadow the checker state API. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| C-FIND-GLOBAL? | `ptr n n --` | Optional bootstrap lookup preserves active package cells and exposes the raw lookup result only through generated registers, before the checker bridge exists. | `tools/bootstrap-codegen-test.f`, `test/engine-suite.f` | src/habu/habu2.f | 2026-07-14 |  |  |
| C-FIND-CHECKER | `ptr n n label --` | Bootstrap package keywords may omit a missing checker bridge only while the friend latch is open; the same path fails closed after sealing. | `tools/bootstrap-codegen-test.f`, `test/engine-suite.f`, `test/seal-package.f` | src/habu/habu2.f | 2026-07-14 |  |  |
| SEAL-VIOLATION | `-- n` | Reopens the reserved early engine-error package after checker startup solely to register the immutable constant's exact output effect. | `tools/bootstrap-codegen-test.f`, `test/engine-suite.f`, `test/seal.f` | src/core/engine-error-effects.f | 2026-07-14 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| SEAL-PACKAGE | `-- n` | Reopens the reserved early engine-error package after checker startup solely to register the immutable constant's exact output effect. | `tools/bootstrap-codegen-test.f`, `test/engine-suite.f`, `test/seal-package.f` | src/core/engine-error-effects.f | 2026-07-14 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| BAD-TAG | `-- n` | Reopens the reserved early engine-error package after checker startup solely to register the immutable constant's exact output effect. | `tools/bootstrap-codegen-test.f`, `test/engine-suite.f`, `test/gate-engine.f` | src/core/engine-error-effects.f | 2026-07-14 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| CALLABLE-ABI | `-- n` | Reopens the reserved early engine-error package after checker startup solely to register the immutable constant's exact output effect. | `tools/bootstrap-codegen-test.f`, `test/engine-suite.f`, `test/gate-engine.f` | src/core/engine-error-effects.f | 2026-07-14 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| CATCH-STACK | `-- n` | Reopens the reserved early engine-error package after checker startup solely to register the immutable constant's exact output effect. | `tools/bootstrap-codegen-test.f`, `test/engine-suite.f`, `test/gate-engine.f` | src/core/engine-error-effects.f | 2026-07-14 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| CODE-CERT | `-- n` | Reopens the reserved early engine-error package after checker startup solely to register the immutable constant's exact output effect. | `tools/bootstrap-codegen-test.f`, `test/engine-suite.f`, `test/gate-engine.f` | src/core/engine-error-effects.f | 2026-07-14 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| c-call-checker-defer | `--` | Deferred-word keyword bridge records the published name in the checker-owned defer-target registry so `is` can reject non-defer targets statically. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| c-call-checker-package | `--` | Package keyword bridge pushes the package token to `CHECKER-PACKAGE`; raw dictionary lookup and generated call setup are outside Forth inference. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-call-checker-public | `--` | Public keyword bridge calls `CHECKER-PUBLIC` so checker signature scope follows runtime package scope. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-call-checker-private | `--` | Private keyword bridge calls `CHECKER-PRIVATE` so checker signature scope follows runtime package scope. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-call-checker-end-package | `--` | ;package keyword bridge calls `CHECKER-END-PACKAGE` before clearing runtime package cells. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-package-fail | `n --` | Package keyword failure emitter prints the current token and exits with the supplied named error code; raw process exit is outside Forth stack inference. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-package-name-guard | `--` | Package namespace-name guard scans the current token in generated registers and rejects embedded namespace separators before dictionary mutation. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-package-new-private-wid | `--` | Package reopen helper allocates one private wordlist id and stores it in the existing namespace record; register and fixed-DATA effects are raw emitter state. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-package-alloc-wids | `--` | Package creation helper allocates paired public/private wordlist ids from the fixed WID counter. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-package-new-record | `--` | Package creation helper emits a namespace dictionary record with public/private wordlist cells and leaves the record/public/private registers for the caller. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-package-existing-private | `label --` | Package reopen helper branches to the caller's done label after ensuring an existing namespace has a private wordlist id. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-package-ensure | `--` | Package keyword dictionary lookup/creation scans namespace records, reuses public wordlists, creates missing private wordlists, and leaves package ids in generated registers. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-package | `--` | Interpreter `package` keyword consumes the following token, rejects nested/malformed packages, opens private scope, syncs checker package state, and saves the parent current wordlist. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-call-checker-export | `--` | Bridges the `EXPORT` keyword to `CHECKER-EXPORT`: finds the global checker word, pushes the original source-name token from the fixed token cells, and calls through the saved x11 record; raw register bridge is outside Forth stack inference. | `test/type-export-suite.f`, `test/run.f` | src/habu/habu2.f | 2026-07-10 |  |  |
| c-export-tail! | `--` | `EXPORT` tail rewriter: scans the pending token for a non-edge first colon and rewrites the fixed token cells to the tail span (FIND parity for edge colons); raw register scan is outside Forth stack inference. | `test/type-export-suite.f`, `test/run.f` | src/habu/habu2.f | 2026-07-10 |  |  |
| c-export | `--` | Interpreter `EXPORT` keyword: rejects use outside a package, applies the seal guard to the source spelling, resolves the source via FIND, syncs the checker alias, and publishes a dictionary record sharing the source code pointer/body span with immediate/wide name bits copied. | `test/type-export-suite.f`, `test/run.f` | src/habu/habu2.f | 2026-07-10 |  |  |
| c-seal-package-fail | `--` | Sealed-system-package failure emitter prints the offending token from the fixed token cells and exits `ENGINE-ERROR:SEAL-PACKAGE`; raw process exit is outside Forth stack inference. | `test/seal-package.f`, `test/run.f` | src/habu/habu2.f | 2026-07-04 |  |  |
| c-seal-match | `--` | Sealed-system-package matcher scans the native reserved-name table (`RESTAB`) in generated registers, case-folds the candidate token `TKA[0,x24)`, and calls the seal failure emitter on a match. | `test/seal-package.f`, `test/run.f` | src/habu/habu2.f | 2026-07-04 |  |  |
| c-qualify-seal-guard | `--` | Definition-time seal guard: when the friend latch is closed and the pending token is a non-edge `NAME:tail`, matches the prefix against the reserved-name table and fails closed; raw latch/register scan is outside Forth stack inference. | `test/seal-package.f`, `test/run.f` | src/habu/habu2.f | 2026-07-04 |  |  |
| c-package-seal-guard | `--` | `package` keyword seal guard: when the friend latch is closed, matches the pending package name against the reserved-name table and fails closed before wordlist allocation. | `test/seal-package.f`, `test/run.f` | src/habu/habu2.f | 2026-07-04 |  |  |
| c-public | `--` | Interpreter `public` keyword switches the active package's current wordlist to the exported public wordlist and syncs checker public mode. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-private | `--` | Interpreter `private` keyword switches the active package's current wordlist back to the private wordlist and syncs checker private mode. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-end-package | `--` | Interpreter `;package` keyword restores the saved parent current wordlist and clears both runtime and checker package frames. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| c-call-checker-using | `--` | `using` keyword bridge pushes the package token to `CHECKER-USING`; raw dictionary lookup of the checker word and the generated call setup are outside Forth inference. | `test/using-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-20 |  |  |
| c-using-name-guard | `--` | `using` name guard consumes the following token and rejects a missing name or an embedded namespace separator in generated registers before lookup. | `test/using-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-20 |  |  |
| c-using-wid | `--` | `using` helper scans namespace records for the named package and leaves its public wordlist id in a generated register, failing closed on an unknown package. | `test/using-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-20 |  |  |
| c-using-push | `--` | `using` helper pushes a public wordlist id onto the fixed-capacity using stack, rejects overflow, and mirrors the name into the checker; register and fixed-DATA effects are raw emitter state. | `test/using-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-20 |  |  |
| c-using | `--` | Interpreter `using` keyword imports a package's public wordlist for bare lookup in the current scope. | `test/using-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-20 |  |  |
| c-end-using | `--` | Interpreter `;using` keyword pops the most recent using from the using stack, rejecting an unbalanced close. | `test/using-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-20 |  |  |
| emit-find-used | `--` | Used-publics resolver leaf scans the dictionary for a bare tail across the live used public wordlists, failing closed on a match in more than one; raw register scan is outside Forth inference. | `test/using-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-20 |  |  |
| em-interpret-define-keywords | `--` | Emits interpreter-mode defining-word dispatch cases grouped separately from literal and lookup fallback. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-interpret-string-keywords | `--` | Emits interpreter-mode string parsing-word dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-interpret-number | `label --` | Emits interpreter-mode number parsing and branches to the caller's not-number label on failure. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| em-interpret-find | `--` | Emits interpreter-mode dictionary lookup, undefined routing, the pre-exec deref/execute arity guard call (LARITY), and execute dispatch. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-07-04 |  |  |
| em-interpret-words | `--` | Chains the factored interpreter-mode defining, string, number, and lookup dispatch emitters. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-interpret | `--` | Chains the factored interpreter-mode colon and word-dispatch emitters. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-drop-locals | `--` | Emits optional locals-frame teardown before a compiled definition returns. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-ret | `--` | Emits the raw return epilogue for a compiled definition. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-flush-pend | `--` | Finalizes the pending dictionary entry length and flips/flushed the generated code region. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-publish-trusted | `label --` | Emits checked/trusted publication for declarations, DOES> signatures, and trust metadata, branching to the supplied publication label. | `test/run.f` | src/habu/habu2.f | 2026-07-17 |  |  |
| em-compile-publish-hooked | `label label --` | Emits hook-based publication for ordinary compiled definitions, branching to the supplied publication or finish label. | `test/run.f` | src/habu/habu2.f | 2026-07-17 |  |  |
| p2w-entry | `label ptr a n n n --` | Pass-2 width-aware transport dispatch case (item 12 slice 3b): keyword match, per-operand width query, and the `ext` lowering emitter run through `JIT-XT-EXECUTE`. | `test/run.f` | src/habu/habu2.f | 2026-07-06 |  |  |
| em-compile-p2wide | `--` | Emits the pass-2 width dispatch stage: the 18 whole-bundle transport cases between the local-reference and keyword tiers. | `test/run.f` | src/habu/habu2.f | 2026-07-06 |  |  |
| em-p2-start | `--` | Emits the pass-2 re-entry: saves the live input, repoints the tokenizer at the captured body, rewinds CP/DP, resets per-definition compile state, and re-emits the prologue. | `test/run.f` | src/habu/habu2.f | 2026-07-06 |  |  |
| em-p2-trigger | `--` | Emits the certified-definition width query: any wider-than-cell width fact enters the pass-2 re-run (wide facts inside a does> split body fail closed). | `test/run.f` | src/habu/habu2.f | 2026-07-06 |  |  |
| em-p2-check-definer | `--` | Emits the sig'd publish gate: pass 1 runs the hook and the pass-2 trigger; the pass-2 second ';' skips the hook re-check (the pass-1 certify already registered the signature). | `test/run.f` | src/habu/habu2.f | 2026-07-06 |  |  |
| em-p2-finish | `--` | Emits the publish-tail pass-2 exit: resumes the saved real input and clears the pass-2 state cells. | `test/run.f` | src/habu/habu2.f | 2026-07-06 |  |  |
| em-compile-publish | `--` | Selects trusted-signature or hook publication for a closed compiled definition. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-semi | `label --` | Emits semicolon close handling and binds the caller-provided not-semi continuation label. | `test/run.f` | src/habu/habu2.f | 2026-06-27 |  |  |
| em-compile-control-keywords | `--` | Emits compile-mode control-flow keyword dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-string-keywords | `--` | Emits compile-mode string parsing-word dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-meta-keywords | `--` | Emits compile-mode meta/parsing keyword dispatch cases such as tick, postpone, DOES>, quotations, and checked `is` assignment. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |  |  |
| em-compile-loop-keywords | `--` | Emits compile-mode loop, return-stack, recursion, and locals keyword dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-keywords | `--` | Chains factored compile-mode keyword dispatch groups. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-local | `--` | Emits compile-mode local-reference lookup and fallthrough. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-literal | `--` | Emits compile-mode numeric literal handling for integer and float literals. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-arith-ops | `--` | Emits arithmetic and bitwise optimized operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-shuffle-ops | `--` | Emits optimized stack-shuffle operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-compare-ops | `--` | Emits optimized comparison operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-unary-ops | `--` | Emits optimized unary numeric operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-float-ops | `--` | Emits optimized floating-point operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-ops | `--` | Chains factored compile-mode arithmetic, shuffle, comparison, unary, and float operator dispatch emitters. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-call | `--` | Emits compile-mode lookup, immediate execution, and call generation. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-reset-compile-state | `--` | Emits reset of compile/repl/evaluate state cells after rollback or recovery. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-eval-throw-recover | `--` | Emits the evaluate throw-escape recovery entry: transactional frame rollback that delivers the escaping throw code via EVALERR-CELL instead of exiting the process. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |  |  |
| em-repl-recover | `--` | Emits REPL recovery after errors, restoring line-start compile state and stacks. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-undef | `--` | Emits undefined-word diagnostics and evaluate/REPL recovery routing. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-eval-clean-exit | `--` | Emits clean evaluate end-of-buffer return path. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-repl-read | `--` | Emits REPL line-state save, read callback call, EOF handling, and input reset. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-compile-exit | `--` | Emits interpreter end-of-input handling for evaluate, REPL ok/read, and process exit. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| em-interpret-underflow | `--` | Emits the data-stack underflow diagnostic (named E-UNDERFLOW + offending word) for the LMAIN depth-floor guard; inside evaluate it delivers a catchable RC-REJECT via the eval throw-recovery (RX restored first), else REPL recovery / batch exit 70. | `test/run.f` | src/habu/habu2.f | 2026-07-15 |  |  |
| em-adt-con-fam | `--` | Emits the construct family-operand step: TFL bridge call, fail-closed unknown-family die, CMFAM/CMM state stores. | `test/run.f` | src/habu/habu2.f | 2026-07-09 |  |  |
| em-adt-con-pushes | `--` | Emits the construct pad/tag VS-constant pushes with frame-saved counters around LVPUSHC. | `test/run.f` | src/habu/habu2.f | 2026-07-09 |  |  |
| em-adt-con-var | `--` | Emits the construct variant-operand step: TFL bridge call, fail-closed unknown-variant die, pad/tag emission, mode clear. | `test/run.f` | src/habu/habu2.f | 2026-07-09 |  |  |
| c-die-bad-tag | `--` | Emits the MATCH invalid-tag die INLINE into the user word: a jump over the message, "hb: bad <family> tag\n" copied inline (the name bytes travel with the word), then a self-contained write(2) + exit_group(ENGINE-ERROR:BAD-TAG). | `test/run.f` | src/habu/habu2.f | 2026-07-09 |  |  |
| em-match-semi | `--` | Emits the MATCH `;match` tail: family-name bridge, inline invalid-tag die, ENDCASE-style join patch loop, match-frame pop, CMM clear. | `test/run.f` | src/habu/habu2.f | 2026-07-09 |  |  |
| em-adt-match-fam | `--` | Emits the MATCH family-operand step: signature-scope TFL bridge call, fail-closed unknown-family die, fam stored on the match-frame stack, CMM state store. | `test/run.f` | src/habu/habu2.f | 2026-07-09 |  |  |
| em-adt-match-var | `--` | Emits the MATCH variant-operand step (or routes `;match`): TFL bridge call, fail-closed unknown-variant die, pending tag/pads stash, CMM state store. | `test/run.f` | src/habu/habu2.f | 2026-07-09 |  |  |
| em-adt-match-of | `--` | Emits the MATCH per-variant compare/branch/prologue (peek tag, cbz-skip, drop tag+pads), pushes the branch-kind marker, arms the branch body. | `test/run.f` | src/habu/habu2.f | 2026-07-09 |  |  |
| em-compile-adt-mode | `--` | Emits the ADT-lowering mode dispatch (CMM-CELL) at the compile-dispatch head: construct operand states; MATCH states land in slice 3. | `test/run.f` | src/habu/habu2.f | 2026-07-09 |  |  |
| em-compile | `--` | Chains the factored compile-mode dispatch, call, undefined, and exit emitters. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| emit-main | `--` | Allocates main-loop labels (incl. LARITY) and chains EM-STARTUP/COMMENT/INTERPRET/COMPILE. | `test/run.f` | src/habu/habu2.f | 2026-07-04 |  |  |
| SRCA@ | `-- ptr u8` | Reads EMIT-FORTH's saved source pointer from a raw variable for the final `BYTES,` copy. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| AOT-BLOB-BUF@ | `-- ptr u8` | Views the AOT-REPL capture code-blob scratch buffer as bytes for the blob copy and `BYTES,` emit. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |  |  |
| AOT-REC-BUF@ | `-- ptr a` | Views the AOT-REPL capture dict-record scratch buffer for the 48-byte record copy and `BYTES,` emit. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |  |  |
| AOT-SITE-BUF@ | `-- ptr u8` | Views the AOT-REPL capture call-site table scratch buffer as bytes for the packed 4B reloc rows (blob-off u16 + name-off u16) and `BYTES,` emit. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |  |  |
| AOT-NAMES-BUF@ | `-- ptr u8` | Views the AOT-REPL capture name-pool scratch buffer as bytes for the name intern and `BYTES,` emit. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |  |  |
| AOT-DSITE-BUF@ | `-- ptr u8` | Views the AOT-REPL capture DATA/CODE-literal relocation table scratch buffer as bytes for the packed u16 blob-offset tables and `BYTES,` emit. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |  |  |
| AOT-BOOTRUN-BUF@ | `-- ptr u8` | Views the AOT-REPL capture boot-run name-list scratch buffer as bytes for the `[len][name]` intern and `BYTES,` emit of the install-tail entry words. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |  |  |
| AOT-PWID-BUF@ | `-- ptr u8` | Views the protected-WID registry AOT capture scratch buffer as bytes for the u32-WID serialize (ACAP-PWID-*) and `BYTES,` emit (TFAM 2b-v). | `test/run.f` | src/habu/habu2.f | 2026-07-04 |  |  |
| AOT-DBASE | `-- ptr a` | Host build-time cast of the metabuild dictionary base to a record pointer for the AOT-REPL capture reverse-lookup. | `test/run.f` | src/habu/aot-capture.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| AOT-A>U8 | `ptr a -- ptr u8` | Host build-time byte view of a code/dict address for the AOT-REPL capture blob and name copies. | `test/run.f` | src/habu/aot-capture.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| AOT-N>U8 | `n -- ptr u8` | Host build-time byte view of a code/dict address value for the AOT-REPL capture blob source and EXT-name reads. | `test/run.f` | src/habu/aot-capture.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| AOT-CELL@ | `ptr a -- n` | Host build-time cell read of a metabuild dict record field for the AOT-REPL capture reverse-lookup. | `test/run.f` | src/habu/aot-capture.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| emit-forth | `ptr u8 n --` | Top-level engine builder: consumes source bytes, allocates every forward-ref label, and emits the complete image. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |  |  |
| LINUX-VA>PTR | `va -- ptr n` | Linux runtime loader addresses are tagged as `va`; converting one to a host pointer for GOT/header reads is the raw image boundary. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| IMAGE-TEXT-SIZE-OFF | `-- n` | Linux executable header offset where the snapshot writer reads the mapped text size. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| IMAGE-TEXT-CONTENT-ADJ | `-- n` | Linux text-size field adjustment from segment size to code-content size for snapshot streaming. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| IMAGE-TEXT-TRAILER-ADJ | `-- n` | Linux trailer address adjustment for snapshot restore when the text-size field includes the code offset. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| DATA-VA | `-- ptr a` | Linux fixed DATA virtual address used by snapshot and AOT startup writers as both cell-address and byte-span base. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| DATA-SIZE | `-- n` | Linux fixed DATA mapping size used by snapshot validation and image inspection. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| MBUF-RC>PTR | `n -- ptr u8` | Narrows the raw anonymous-mmap return cell for the target image-builder output buffer into the typed byte span used by checked image writers. | `tools/image-bytes-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/os/image-bytes.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| CODE-OFF | `-- n` | Linux executable code offset used by checked snapshot streaming code. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| LINUX-DLOPEN-SLOT-OFF | `-- n` | Linux dynamic ELF GOT byte offset for the `dlopen` relocation inside the computed RW segment. | `test/run.f`, `test/gate-aot-positive.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| LINUX-DLSYM-SLOT-OFF | `-- n` | Linux dynamic ELF GOT byte offset for the `dlsym` relocation inside the computed RW segment. | `test/run.f`, `test/gate-aot-positive.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| LINUX-IMAGE-BASE | `-- n` | Linux runtime image base is recovered from the code base and executable offset before reading the snapshot header. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| LINUX-TEXT-CELL | `-- ptr n` | Linux runtime text-size header cell is reached through raw image-address arithmetic. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| LINUX-TEXT-SIZE | `-- n` | Linux runtime text size is read from the mapped executable header to locate the dynamic RW segment. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| LINUX-RW-VA | `-- va` | Linux dynamic RW segment starts after the live text mapping, not at a fixed address, so runtime FFI slots derive from the image header. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| DLOPEN-SLOT-VA | `-- va` | Linux dynamic ELF GOT virtual address for `dlopen`, computed from the live RW segment so it cannot overlap the snapshot text. | `test/run.f`, `test/gate-aot-positive.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| DLSYM-SLOT-VA | `-- va` | Linux dynamic ELF GOT virtual address for `dlsym`, computed from the live RW segment so it cannot overlap the snapshot text. | `test/run.f`, `test/gate-aot-positive.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| DLOPEN-SLOT | `-- ptr n` | Linux dynamic ELF GOT cell where ld.so resolves `dlopen` before Habu FFI reads the function pointer. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| DLSYM-SLOT | `-- ptr n` | Linux dynamic ELF GOT cell where ld.so resolves `dlsym` before Habu FFI reads the function pointer. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SNAP-EXTRA-PTR | `-- ptr u8` | Linux snapshot writer stages the dynamic RW segment after the header buffer and streams it after the padded live text. | `test/run.f`, `test/engine-suite.f` | src/os/linux/elf.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SNAP-EXTRA-SIZE | `-- n` | Linux snapshot writer appends the fixed-size `.dynamic` plus GOT segment after the padded text payload. | `test/run.f`, `test/engine-suite.f` | src/os/linux/elf.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| IMAGE-TEXT-SIZE-OFF | `-- n` | macOS executable header offset where the snapshot writer reads the mapped text size. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| IMAGE-TEXT-CONTENT-ADJ | `-- n` | macOS text-size field adjustment from section size to code-content size for snapshot streaming. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| IMAGE-TEXT-TRAILER-ADJ | `-- n` | macOS trailer address adjustment because the section size excludes the code offset. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| DATA-VA | `-- ptr a` | macOS fixed DATA virtual address used by snapshot and AOT startup writers as both cell-address and byte-span base. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| DATA-SIZE | `-- n` | macOS fixed DATA mapping size used by snapshot validation and image inspection. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| CODE-OFF | `-- n` | macOS executable code offset used by checked snapshot streaming code. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| MACHO>N-PTR | `n -- ptr n` | macOS image boundary cast: turns a computed Mach-O header/GOT cell address into a typed cell pointer; all offset arithmetic around it remains checked. | `lib/ffi-test.f`, `test/run.f` | src/os/macos/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| DLOPEN-SLOT | `-- ptr n` | macOS dyld-resolved `__DATA_CONST,__got` cell for libSystem `_dlopen`, located from the live Mach-O text size. | `lib/ffi-test.f`, `test/run.f` | src/os/macos/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| DLSYM-SLOT | `-- ptr n` | macOS dyld-resolved `__DATA_CONST,__got` cell for libSystem `_dlsym`, adjacent to `DLOPEN-SLOT`. | `lib/ffi-test.f`, `test/run.f` | src/os/macos/layout.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SNAP-EXTRA-PTR | `-- ptr u8` | macOS snapshot writer stages the `__DATA_CONST` GOT page plus chained-fixups blob after the header buffer and streams it after the padded live text. | `test/run.f`, `test/engine-suite.f` | src/os/macos/macho.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SNAP-EXTRA-SIZE | `-- n` | macOS snapshot images append one `__DATA_CONST` page plus the fixed chained-fixups payload after the padded text payload. | `test/run.f`, `test/engine-suite.f` | src/os/macos/macho.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| ARGC-CELL | `-- n` | Common DATA header byte offset for the process argc startup cell. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/layout.f | 2026-06-26 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| ARGV-CELL | `-- n` | Common DATA header byte offset for the process argv vector startup cell. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/layout.f | 2026-06-26 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| ENVP-CELL | `-- n` | Common DATA header byte offset for the process envp vector startup cell. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/layout.f | 2026-06-26 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| JIT-XT-EXECUTE | `n --` | Narrow higher-order JIT boundary: checked dispatch entry words pass one build-time emitter xt through this raw `execute` shim. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/jit.f | 2026-06-27 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| fold-entry | `label ptr a n n --` | JIT constant-fold case: emits the keyword guard then dispatches one fold handler through `JIT-XT-EXECUTE` and branches to the main-loop label. | `test/run.f` | src/habu/jit.f | 2026-06-27 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| vop-entry | `label ptr a n n n --` | JIT binop case: fold-vs-emit split with two indirect xts and raw asm, returning to the main-loop label. | `test/run.f` | src/habu/jit.f | 2026-06-27 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| vopi-entry | `label ptr a n n n n --` | JIT binop-immediate case: fold/register/immediate split with three indirect xts and raw asm, returning to the main-loop label. | `test/run.f` | src/habu/jit.f | 2026-06-27 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| vshuf-entry | `label ptr a n n n --` | JIT reg-aware stack-shuffle case (dup/over/swap/drop/nip as register moves); `sxt` runs through `JIT-XT-EXECUTE`. | `test/run.f` | src/habu/jit.f | 2026-06-27 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| vun-entry | `label ptr a n n n --` | JIT unary-op case: con-fold vs in-place reg op via `foldxt`/`emitxt` and raw asm, returning to the main-loop label. | `test/run.f` | src/habu/jit.f | 2026-06-27 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-prof-mctx>r21 | `--` | Profiler SIGALRM handler derives the target mcontext address from raw signal-entry registers. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-prof-pc>r9 | `--` | Profiler SIGALRM handler reads the target-specific saved PC field from mcontext. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-prof-sigaction-frame | `--` | Profiler builds the target kernel sigaction record directly on the generated stack. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-prof-sigaction | `--` | Profiler installs SIGALRM through the target raw sigaction syscall ABI. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-prof-sigaction-done | `--` | Profiler releases the generated sigaction stack scratch frame. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-prof-timer-frame | `--` | Profiler builds the target itimerval record directly on the generated stack. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-prof-timer | `--` | Profiler arms the interval timer through the raw setitimer syscall ABI. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| c-prof-timer-done | `--` | Profiler releases the generated timer stack scratch frame. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| emit-prof-prims | `--` | Emits the sampling-profiler prims as raw asm via FPRIM-L. | `test/run.f`, `test/gate-debug.f` | src/habu/prof.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| TIMES | `R i64 [ R -- R ] -- R` | Counted loop keeps the quotation available across repeated `execute`; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f | 2026-06-16 | stdlib-boundary | habu-multishot-quotations-typed-8832cace |
| EACH | `R ptr a i64 [ R a -- R ] -- R` | Array iterator keeps the quotation across element calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f | 2026-06-16 | stdlib-boundary | habu-multishot-quotations-typed-8832cace |
| MAP | `R ptr a i64 [ R a -- R a ] -- R` | Array map keeps the quotation across element calls and mutates cells in place; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f | 2026-06-16 | stdlib-boundary | habu-multishot-quotations-typed-8832cace |
| DEV-FLD-BEGIN | `-- n` | Field-record seam: forwards to the pre-hook field-record transaction `PF-BEGIN`; the checker cannot type the sealed pre-hook registry word from a post-hook checked body. Reconciled with `src/core/type-field.f` at merge. | `test/decl-event-suite.f`, `test/run.f` | src/core/decl-event.f | 2026-07-20 | stdlib-boundary | habu-type-dsl-unify-b65d46c1 |
| DEV-FLD-ADD | `n n n ptr u8 n n n n n n n n -- n` | Field-record seam: forwards to the pre-hook `PF-ADD` (validated field append under a transaction); the checker cannot type the sealed pre-hook registry word from a post-hook checked body. | `test/decl-event-suite.f`, `test/run.f` | src/core/decl-event.f | 2026-07-20 | stdlib-boundary | habu-type-dsl-unify-b65d46c1 |
| DEV-FLD-COMMIT | `n -- ` | Field-record seam: forwards to the pre-hook `PF-COMMIT` (outer commit publishes field rows); the checker cannot type the sealed pre-hook registry word from a post-hook checked body. | `test/decl-event-suite.f`, `test/run.f` | src/core/decl-event.f | 2026-07-20 | stdlib-boundary | habu-type-dsl-unify-b65d46c1 |
| DEV-FLD-ROLLBACK | `n -- ` | Field-record seam: forwards to the pre-hook `PF-ROLLBACK` (retire provisional field rows); the checker cannot type the sealed pre-hook registry word from a post-hook checked body. | `test/decl-event-suite.f`, `test/run.f` | src/core/decl-event.f | 2026-07-20 | stdlib-boundary | habu-type-dsl-unify-b65d46c1 |
| DEV-FLD-COUNT | `-- n` | Field-record seam: forwards to `TYPE-FIELD:COUNT` (committed field high-water); the checker cannot type the sealed pre-hook registry word from a post-hook checked body. | `test/decl-event-suite.f`, `test/run.f` | src/core/decl-event.f | 2026-07-20 | stdlib-boundary | habu-type-dsl-unify-b65d46c1 |
| DEV-SUMV-ADD | `n ptr u8 n n n n n -- n` | Variant-registry seam: forwards to the pre-hook `SUMV-ADD` (register one variant with the dup/canon/reserved gate); the checker cannot type the sealed pre-hook registry word from a post-hook checked body. | `test/decl-event-suite.f`, `test/run.f` | src/core/decl-event.f | 2026-07-20 | stdlib-boundary | habu-type-dsl-unify-b65d46c1 |
| DEV-REG-GROW1 | `ptr a n n -- ` | Growable-arena boundary: forwards to the pre-hook `REG-GROW1` (relocating buffer realloc storing the new base back); raw relocating-memory grow the checker cannot model. Missing checked growable-arena capability is a chain dot. | `test/decl-event-suite.f`, `test/run.f` | src/core/decl-event.f | 2026-07-20 | stdlib-boundary | habu-type-dsl-unify-b65d46c1 |
| SM-FAM-LIVE? | `n -- bool` | STRUCTURE generator seam: forwards to the pre-hook `PF-FAM-LIVE?` (is a family id live) so the checked validation rejects a stale/rolled-back id before touching any TFAM reader; the checker cannot type the sealed pre-hook registry word from a post-hook checked body. Re-points to the shared constructor-gen module at the type-DSL cutover. | `test/structure-make-suite.f`, `test/run.f` | src/core/structure-make.f | 2026-07-21 | stdlib-boundary | habu-structure-generate-make-872a6e75 |
| SM-PRODUCT? | `n -- bool` | STRUCTURE generator seam: forwards to the pre-hook `TFAM-PRODUCT?` (family is product-kind); the checker cannot type the sealed pre-hook registry word from a post-hook checked body. Re-points to the shared constructor-gen module at the type-DSL cutover. | `test/structure-make-suite.f`, `test/run.f` | src/core/structure-make.f | 2026-07-21 | stdlib-boundary | habu-structure-generate-make-872a6e75 |
| SM-PUBLIC? | `n -- bool` | STRUCTURE generator seam: forwards to the pre-hook `TFAM-PUBLIC?` (family is public, so it owns a reserved constructor package); the checker cannot type the sealed pre-hook registry word from a post-hook checked body. Re-points to the shared constructor-gen module at the type-DSL cutover. | `test/structure-make-suite.f`, `test/run.f` | src/core/structure-make.f | 2026-07-21 | stdlib-boundary | habu-structure-generate-make-872a6e75 |
| SM-FLD-START | `n -- n` | STRUCTURE generator seam: forwards to the pre-hook `TFAM-FLD-START@` (the family's committed field-range start); the checker cannot type the sealed pre-hook registry word from a post-hook checked body. Re-points to the shared constructor-gen module at the type-DSL cutover. | `test/structure-make-suite.f`, `test/run.f` | src/core/structure-make.f | 2026-07-21 | stdlib-boundary | habu-structure-generate-make-872a6e75 |
| SM-FLD-COUNT | `n -- n` | STRUCTURE generator seam: forwards to the pre-hook `TFAM-FLD-COUNT@` (the family's committed field count); the checker cannot type the sealed pre-hook registry word from a post-hook checked body. Re-points to the shared constructor-gen module at the type-DSL cutover. | `test/structure-make-suite.f`, `test/run.f` | src/core/structure-make.f | 2026-07-21 | stdlib-boundary | habu-structure-generate-make-872a6e75 |
| SM-SUMV-FIND | `n ptr u8 n -- n bool` | STRUCTURE generator seam: forwards to the pre-hook `SUMV-FIND` (does the family already own a variant of this tail) so the checked validation rejects a second generation; the checker cannot type the sealed pre-hook registry word from a post-hook checked body. Re-points to the shared constructor-gen module at the type-DSL cutover. | `test/structure-make-suite.f`, `test/run.f` | src/core/structure-make.f | 2026-07-21 | stdlib-boundary | habu-structure-generate-make-872a6e75 |
| SM-EMIT | `n n n -- ` | STRUCTURE generator seam: the whole infallible mutation for an already-validated published product (family, field-range start, field count) — rebuild the committed field schema nodes into one contiguous root run, add the make(0)/unmake(1) variant rows at the product width, set the variant range, derive the constructor package (`TDECL-CTOR-PUBLISH`), and generate both words (`TDECL-PROD-WORDS`); all sealed pre-hook registry / generation words the checker cannot type from a post-hook checked body. GENERATE makes every decision before calling it, so it cannot reject for well-formed input. Re-points to the shared constructor-gen module at the type-DSL cutover. | `test/structure-make-suite.f`, `test/run.f` | src/core/structure-make.f | 2026-07-21 | stdlib-boundary | habu-structure-generate-make-872a6e75 |
| FOLD | `R ptr a i64 b [ R b a -- R b ] -- R b` | Array fold keeps the quotation across accumulator calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f | 2026-06-16 | stdlib-boundary | habu-multishot-quotations-typed-8832cace |
| INCLUDE-MMAP-PTR | `n -- ptr u8` | Refines the checked anonymous `mmap` result into the byte pointer backing include buffers after size selection and `-1` failure checking; syscall-result pointer refinement is outside checker inference. | `test/gate-dictionary.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/include.f | 2026-06-28 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| INCLUDE-EVALUATE | `ptr u8 n --` | Source composition reads and bounds file bytes in checked code, then crosses the dynamic `evaluate` boundary that the checker intentionally rejects in ordinary checked definitions. | `test/gate-dictionary.f`, `test/run.f` | src/core/include.f | 2026-06-28 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| ARENA-RC>PTR | `n -- ptr a` | Thin identity refinement from a checked, nonnegative anonymous `mmap` result into the checker's cell arena pointer; syscall-result pointer typing is outside checker inference. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-03 | discharge-candidate | habu-checker-self-typing-9ff8ba86 |
| TOKBUF-RC>PTR | `n -- ptr u8` | Thin identity refinement from a checked, nonnegative anonymous `mmap` result into the checker's token byte-buffer pointer; syscall-result pointer typing is outside checker inference. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-03 | discharge-candidate | habu-checker-self-typing-9ff8ba86 |
| USIGS-RC>PTR | `n -- ptr u8` | Thin identity refinement from a checked, nonnegative anonymous `mmap` result into the checker's transient signature byte store; syscall-result pointer typing is outside checker inference. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-03 | discharge-candidate | habu-checker-self-typing-9ff8ba86 |
| USIGS-CELL-AT | `n -- ptr a` | Refines a cell-aligned offset inside the byte-addressed transient signature store so checker metadata can write cell headers (e.g. the USIGS-CLEAR head cell) while byte-copy paths keep `ptr u8`. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-04 | discharge-candidate | habu-checker-self-typing-9ff8ba86 |
| HIDX-MEM-NULL | `-- ptr a` | The unallocated symbol-index cache sentinel is a null pointer; the checker cannot type a literal `0` as `ptr a`, so this one-line refinement supplies the typed null that `HIDX-MEM-CLEAR` stores and `HIDX-MEM-READY?` tests. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-04 | discharge-candidate | habu-checker-self-typing-9ff8ba86 |
| HIDX-RC>PTR | `n -- ptr n` | Thin identity refinement from a checked, nonnegative anonymous `mmap` result into the checker's symbol-index cell table; syscall-result pointer typing is outside checker inference. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-03 | discharge-candidate | habu-checker-self-typing-9ff8ba86 |
| EFFECT-QUERY | `ptr u8 n -- bool` | Effect-read export API entry: resolves a NAME's active effect (user row or prim axiom) via FIND-SIG and reads FEP/ER.DIN/ER.DOUT — raw checker effect-store state outside checker inference — into query state for the checked EFFECT-DIN-N/EFFECT-DOUT-N/EFFECT-DIN-FAM/EFFECT-DOUT-FAM readers that a cold-prefix consumer (src/core/top-row.f, tier-2 dot habu-typed-top-tier-589c550f) calls from an unchecked boundary. | `test/effect-read-api-test.f`, `test/run.f` | src/core/checker.f | 2026-07-15 | discharge-candidate | habu-checker-self-typing-9ff8ba86 |
| CELL | `-- n` | The target cell-width source loads before the checker so pre-checker records can use it; this row publishes the already-defined constant to checked users. | `tools/bootstrap-codegen-test.f`, `test/run.f` | src/core/cell-effects.f | 2026-07-13 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| CELL-WIDTH-CHECK | `--` | The target-width assertion must execute during the pre-checker prefix; its post-hook row lets the focused checked bootstrap regression execute the identical body again. | `tools/bootstrap-codegen-test.f`, `test/run.f` | src/core/cell-effects.f | 2026-07-13 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| STRUCT-BYTE+ | `ptr a n -- ptr u8` | `CFIELD:` needs to refine a structure base plus byte offset into a byte pointer; generic `+` can produce only `ptr a`, and `BYTE+` requires an existing byte pointer. | `test/gate-dictionary.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| BEGIN-STRUCTURE | `-- ptr a n` | Structure defining words use `CREATE`/`DOES>` and parse definition names, so the checker needs declared effects for the top-level layout DSL. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| +FIELD | `ptr a n n -- ptr a n` | Field definers consume and return the in-progress layout cursor while creating accessor words through `CREATE`/`DOES>`. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| PTR-FIELD: | `ptr a n -- ptr a n` | Pointer field definer preserves the layout cursor while creating a pointer-valued accessor; `CREATE`/`DOES>` keeps this as a trusted defining boundary. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| PTR-VARIABLE | `--` | Pointer variables are created through `CREATE`/`DOES>` with a pointer-valued runtime effect that the checker cannot infer from the definer body. | `test/pointer-storage-test.f` (stdlib/tail-pure, test/run.f) | src/core/pointer-storage-effects.f | 2026-07-13 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| CFIELD: | `ptr a n -- ptr a n` | Byte field definer preserves the layout cursor while creating a byte-pointer accessor; `CREATE`/`DOES>` keeps this as a trusted defining boundary. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| END-STRUCTURE | `ptr a n --` | Sealing a structure consumes the layout cursor and writes the final byte size into the created size word. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| NG-EVAL | `--` | Audited `evaluate` wrapper for `DEFTYPE`: compiles each generated `TRUSTED: >NAME ( n -- tail ) ;` / `NAME>N` value-nominal converter so the check hook certifies it. `evaluate` cannot be checker-typed; each generated body is a proven no-op identity cast, so this single boundary covers every declaration-derived converter pair (the src/core/roles.f DTC-EVAL / maki/extent.f XG-EVAL pattern). | `test/deftype-suite.f`, `test/run.f` | lib/type/deftype.f | 2026-07-18 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >IDX | `n -- idx` | Runtime identity cast from a generic cell to the nominal index role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| IDX>N | `idx -- n` | Runtime identity cast from the nominal index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >LEN | `n -- len` | Runtime identity cast from a generic cell to the nominal length role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| LEN>N | `len -- n` | Runtime identity cast from the nominal length role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >COUNT | `n -- count` | Runtime identity cast from a generic cell to the nominal count role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| COUNT>N | `count -- n` | Runtime identity cast from the nominal count role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >OFF | `n -- off` | Runtime identity cast from a generic cell to the nominal offset role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| OFF>N | `off -- n` | Runtime identity cast from the nominal offset role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >FD | `n -- fd` | Runtime identity cast from a generic cell to the nominal file-descriptor role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| FD>N | `fd -- n` | Runtime identity cast from the nominal file-descriptor role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >RC | `n -- rc` | Runtime identity cast from a generic cell to the nominal return-code role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| RC>N | `rc -- n` | Runtime identity cast from the nominal return-code role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >PID | `n -- pid` | Runtime identity cast from a generic cell to the nominal process-id role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| PID>N | `pid -- n` | Runtime identity cast from the nominal process-id role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >MS | `n -- ms` | Runtime identity cast from a generic cell to the nominal millisecond role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| MS>N | `ms -- n` | Runtime identity cast from the nominal millisecond role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >NS | `n -- ns` | Runtime identity cast from a generic cell to the nominal nanosecond role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| NS>N | `ns -- n` | Runtime identity cast from the nominal nanosecond role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >TOK | `n -- tok` | Runtime identity cast from a generic cell to the nominal token-index role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| TOK>N | `tok -- n` | Runtime identity cast from the nominal token-index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >REG | `n -- reg` | Runtime identity cast from a generic cell to the nominal register role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| REG>N | `reg -- n` | Runtime identity cast from the nominal register role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >LABEL | `n -- label` | Runtime identity cast from a generic cell to the nominal code-label role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| LABEL>N | `label -- n` | Runtime identity cast from the nominal code-label role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >VA | `n -- va` | Runtime identity cast from a generic cell to the nominal virtual-address role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| VA>N | `va -- n` | Runtime identity cast from the nominal virtual-address role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >SYMIDX | `n -- symidx` | Runtime identity cast from a generic cell to the nominal dynamic-symbol-index role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| SYMIDX>N | `symidx -- n` | Runtime identity cast from the nominal dynamic-symbol-index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >ASM | `n -- asm` | Runtime identity cast from a generic cell to the nominal assembled-code phase token; the checker cannot infer phase-token refinement from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| ASM>N | `asm -- n` | Runtime identity cast from the nominal assembled-code phase token back to a generic cell; the checker cannot infer phase-token erasure from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >IMG | `n -- img` | Runtime identity cast from a generic cell to the nominal executable-image phase token; the checker cannot infer phase-token refinement from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| IMG>N | `img -- n` | Runtime identity cast from the nominal executable-image phase token back to a generic cell; the checker cannot infer phase-token erasure from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >SNAP | `n -- snap` | Runtime identity cast from a generic cell to the nominal snapshot-header phase token; the checker cannot infer phase-token refinement from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| SNAP>N | `snap -- n` | Runtime identity cast from the nominal snapshot-header phase token back to a generic cell; the checker cannot infer phase-token erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| TTHROWS-RAW | `a n --` | Top-level test assertion boundary around execution-token `catch`; checked colon definitions should use `TTHROWSQ`, but top-level scripts cannot push `[: ;]` quotations. | `lib/test/assert-test.f`, `test/run.f` | lib/test/assert.f | 2026-06-22 | test-metaprog | habu-typed-depth-introspection-18f0efda |
| P>N | `ptr a -- n` | FFI argument marshalling: reinterpret any pointer as the raw integer cell the AAPCS64 trampoline loads into x0-x7; the checker has no pointer-to-cell coercion. | `lib/ffi-abi-test.f`, `lib/ffi-test.f`, `test/gate-stdlib.f` | lib/ffi-abi.f | 2026-06-27 | stdlib-boundary | habu-typed-defining-words-aa224eb5 |
| DLOPEN-RAW | `ptr u8 n -- n` | Private exact `dlopen` boundary: the path is read-only, flags are scalar, and the sealed `FFI` package fixes both directions before the trusted-only bounded call. | `lib/ffi-test.f`, `lib/task-test.f`, `lib/ptx/cuda-driver-test.f`, `test/seal-package.f` | lib/ffi-abi.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| DLSYM-RAW | `n ptr u8 -- n` | Private exact `dlsym` boundary: handle is scalar, symbol is read-only, and the sealed `FFI` package prevents replacement or extension of the call surface. | `lib/ffi-test.f`, `lib/task-test.f`, `lib/ptx/cuda-driver-test.f`, `test/seal-package.f` | lib/ffi-abi.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-STORE-X1 | `-- n` | Test-local fixed code emitter for one x1 store instruction; no address or instruction is caller-selected. | `lib/ffi-abi-test.f` | lib/ffi-abi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-STORE | `ptr a n -- n` | Exact test writer fixes argument zero to an eight-byte writable pointer and argument one to a scalar before calling the local AAPCS64 stub. | `lib/ffi-abi-test.f`, `test/protection-span.f` | lib/ffi-abi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-KPARAM-SUM2 | `-- n` | Test-local fixed code emitter for the two-parameter kernel fixture; no address or instruction is caller-selected. | `lib/ffi-abi-test.f` | lib/ffi-abi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-X8-STORE | `-- n` | Test-local fixed code emitter for one x8 store instruction; no address or instruction is caller-selected. | `lib/ffi-abi-test.f` | lib/ffi-abi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-STACK-STORE | `-- n` | Test-local fixed code emitter for one stack-argument store instruction; no address or instruction is caller-selected. | `lib/ffi-abi-test.f` | lib/ffi-abi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-X8-CALL | `ptr a -- n` | Exact test-only mixed-ABI binding: x8 is fixed as a one-cell writer and its extent is installed by the binding, not selected by the caller. | `lib/ffi-abi-test.f`, `test/protection-span.f` | lib/ffi-abi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-STACK-CALL | `ptr a -- n` | Exact test-only mixed-ABI binding: stack slot zero is fixed as a one-cell writer and uses the distinct stack extent table. | `lib/ffi-abi-test.f`, `test/protection-span.f` | lib/ffi-abi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-KPARAM-CALL | `ptr a -- n` | Exact test-only one-argument read-only binding for the kernel-parameter fixture; direct primitive use is confined to this fixed schema. | `lib/ffi-abi-test.f` | lib/ffi-abi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-STRLEN$ | `ptr u8 -- n` | Test-only exact libc `strlen` binding fixes its sole pointer read-only and resolves the symbol before staging task-local arguments. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-STRNCMP$ | `ptr u8 ptr u8 n -- n` | Test-only exact libc `strncmp` binding fixes two read-only pointers and one scalar length. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-GETPID$ | `-- n` | Test-only exact zero-argument libc `getpid` binding. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-CTX-SET | `ffi-ctx -- rc` | Test-only nominal-role binding proves a distinct nominal input cannot be substituted even though the ABI cell is scalar. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-VOID$ | `--` | Test-only void-result binding drops the single machine return cell at the trusted boundary. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-SUM10 | `-- n` | Test-local fixed code emitter for the ten-integer sum fixture; no address or instruction is caller-selected. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-FSUM3 | `-- n` | Test-local fixed code emitter for the three-register floating sum fixture; no address or instruction is caller-selected. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-FADD-X0 | `-- n` | Test-local fixed code emitter for the mixed x0/d0 fixture; no address or instruction is caller-selected. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-FADD-FSTACK | `-- n` | Test-local fixed code emitter for the floating stack-spill fixture; no address or instruction is caller-selected. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-X8-STORE | `-- n` | Test-local fixed code emitter for the x8 store fixture; no address or instruction is caller-selected. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-STRLEN-LATE | `ptr u8 -- n` | Regression binding stages `strlen` before resolving it, proving the dedicated loader block cannot overwrite task-local call arguments. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-SUM10-CALL | `-- n` | Test-only exact ten-integer binding covers x0-x7 plus two stack-spilled cells through the bounded integer trampoline. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-FSUM3-CALL | `-- r` | Test-only exact three-register floating-point binding. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-FADD-X0-CALL | `-- r` | Test-only exact mixed x0 and d0 binding. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-FADD-FSTACK-CALL | `-- r` | Test-only exact floating-register plus stack-spill binding with separate extent tables. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-X8-ABI-CALL | `ptr a -- n` | Test-only exact sret binding fixes x8 to an eight-byte writable output. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| FFI-T-SQRT-CALL | `r -- r` | Test-only exact libm square-root binding returns one floating result. | `lib/ffi-test.f` | lib/ffi-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| TASK-N>PTR | `n -- ptr a` | Reinterpret task-control-block cell storage as a pointer when loading the current task pointer. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-06-30 | stdlib-boundary | habu-typed-defining-words-aa224eb5 |
| TASK-PATCH | `n n --` | Code-emission boundary: emits JIT task-trampoline instructions via `patch32`, a TRUSTED-only capability primitive (machine-code sink, rejected from CHECKED code as E-CAP-TRUSTED). | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-07-09 | stdlib-boundary | habu-checker-capability-gate-14022ba9 |
| TASK-CELL>PTR-SLOT | `ptr a -- ptr ptr a` | Reinterpret a data-region cell address as a pointer-valued slot; the checker cannot infer the slot payload type from the offset. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-06-30 | stdlib-boundary | habu-typed-defining-words-aa224eb5 |
| TASK | `n --` | Defining word that allocates a task control-block record and returns it through DOES>; CREATE/DOES> effect is outside checker inference. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-06-30 | stdlib-boundary | habu-typed-defining-words-aa224eb5 |
| +USER | `n n -- n` | Defining word for task-local user storage; CREATE/DOES> returns an address derived from the current data region. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-06-30 | stdlib-boundary | habu-typed-defining-words-aa224eb5 |
| FACILITY | `--` | Defining word for owner-tracked pthread mutex storage; CREATE/DOES> returns the facility record address. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-06-30 | stdlib-boundary | habu-typed-defining-words-aa224eb5 |
| MUNMAP-CALL | `ptr a n -- n` | Exact task-internal `munmap` binding marks the unmapped address read-only from the callee's perspective and stages its byte length as scalar. | `lib/task-test.f` | lib/task.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| PTHREAD-CREATE-CALL | `ptr a n n ptr a -- n` | Exact task-internal `pthread_create` binding fixes the thread id output to eight writable bytes and the opaque task argument read-only. | `lib/task-test.f` | lib/task.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| PTHREAD-JOIN-CALL | `n ptr a -- n` | Exact task-internal `pthread_join` binding fixes its return-value output to eight writable bytes. | `lib/task-test.f` | lib/task.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| PTHREAD-EXIT-CALL | `n --` | Exact task-internal noreturn `pthread_exit` binding consumes the machine return cell inside the trusted boundary. | `lib/task-test.f` | lib/task.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| SCHED-YIELD-CALL | `-- n` | Exact zero-argument task-internal `sched_yield` binding. | `lib/task-test.f` | lib/task.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| MUTEX-INIT-CALL | `ptr a n -- n` | Exact task-internal mutex initialization binding fixes the mutex object to its full writable extent. | `lib/task-test.f` | lib/task.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| MUTEX-LOCK-CALL | `ptr a -- n` | Exact task-internal mutex lock binding fixes the mutex object to its full writable extent. | `lib/task-test.f` | lib/task.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| MUTEX-UNLOCK-CALL | `ptr a -- n` | Exact task-internal mutex unlock binding fixes the mutex object to its full writable extent. | `lib/task-test.f` | lib/task.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| TASK-CSTRLEN | `ptr u8 -- n` | Task-concurrency fixture resolves `strlen` once at load time, then pauses after staging to prove every task owns separate argument and extent tables. | `lib/task-test.f` | lib/task-test.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| c-task-live-guard | `--` | Engine emitter guard that rejects dictionary/source mutation while pthread tasks are live; raw exit path and token printing are assembly-side. | `lib/task-test.f`, `test/gate-stdlib.f`, `test/run.f` | src/habu/habu2.f | 2026-06-30 |  |  |
| TASK-RUN-USER | `-- n` | Runs the current task's user body under catch and returns the throw code. The body is a ( -- ) xt supplied to ACTIVATE and stored in the task control block, so it is dynamic per task; a structure field cannot be a typed xt cell, so its effect is unknown at the catch site and catching it is rejected in checked code (E-EXEC-OPAQUE-XT). This scheduler dispatch stays a trusted boundary. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-07-19 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-INIT | `n -- rc` | Exact CUDA `cuInit` scalar binding in the sealed `CUDA` package. | `lib/ptx/cuda-driver-test.f`, `maki/device-smoke.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-DEVICE-GET | `ptr a idx -- rc` | Exact CUDA device lookup binding fixes the output to one writable cell and the index to a scalar. | `lib/ptx/cuda-driver-test.f`, `maki/device-smoke.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-DEVICE-GET-ATTRIBUTE | `ptr a n cuda-dev -- rc` | Exact CUDA device-attribute binding fixes the output to one writable cell, stages the attribute id as a scalar, and preserves the nominal device role. | `lib/ptx/cuda-driver-test.f`, `maki/eval/active-target.f` | lib/ptx/cuda-driver.f | 2026-07-19 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-DEVICE-PRIMARY-CTX-RETAIN | `ptr a cuda-dev -- rc` | Exact CUDA primary-context binding fixes the output to one writable cell and preserves the nominal device role. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-CTX-SET-CURRENT | `cuda-ctx -- rc` | Exact CUDA current-context binding preserves the nominal context role. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-MODULE-LOAD | `ptr a ptr u8 -- rc` | Exact CUDA module-load binding fixes the output to one writable cell and the path pointer read-only. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-MODULE-GET-FUNCTION | `ptr a cuda-mod ptr u8 -- rc` | Exact CUDA function lookup fixes the output to one writable cell, preserves the module role, and marks the name read-only. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-MEM-ALLOC | `ptr a len -- rc` | Exact CUDA allocation binding fixes the output to one writable cell and the byte length to scalar input. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-MEM-FREE | `cuda-devptr -- rc` | Exact CUDA free binding preserves the nominal device-pointer role. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-MEMSET-D32 | `cuda-devptr n count -- rc` | Exact CUDA memset binding preserves the device-pointer role and stages value and count as scalars. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/redadd-device-test.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-MEMCPY-HTOD | `cuda-devptr ptr u8 len -- rc` | Exact CUDA host-to-device copy marks the host source read-only and preserves device-pointer and length roles. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-MEMCPY-DTOH | `ptr u8 cuda-devptr len -- rc` | Exact CUDA device-to-host copy derives the writable host extent from its length argument. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-FUNC-SET-BLOCK-SHAPE | `cuda-fn n n n -- rc` | Exact CUDA block-shape binding preserves the function role and stages dimensions as scalars. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-PARAM-SET-SIZE | `cuda-fn len -- rc` | Exact CUDA parameter-size binding preserves the function and byte-length roles. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-PARAM-SET-V | `cuda-fn idx ptr u8 len -- rc` | Exact CUDA parameter-copy binding preserves function/index/length roles and marks the source read-only. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-FUNC-SET-SHARED-SIZE | `cuda-fn n -- rc` | Exact CUDA dynamic-shared-size binding preserves the nominal function role; byte count is a plain n for the cuLaunchGrid path. | `tools/ptx/mma-gemm-check.f` | lib/ptx/cuda-driver.f | 2026-07-15 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-FUNC-SET-ATTRIBUTE | `cuda-fn n n -- rc` | Exact CUDA function-attribute binding preserves the nominal function role; attribute id and value are plain n (used for MAX_DYNAMIC_SHARED_SIZE_BYTES opt-in). | `tools/ptx/mma-gemm-check.f` | lib/ptx/cuda-driver.f | 2026-07-15 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-LAUNCH-GRID | `cuda-fn n n -- rc` | Exact CUDA grid-launch binding preserves the function role and stages grid dimensions as scalars. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-CTX-SYNCHRONIZE | `-- rc` | Exact zero-argument CUDA context synchronization binding. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-MODULE-UNLOAD | `cuda-mod -- rc` | Exact CUDA module unload binding preserves the nominal module role. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-DEVICE-PRIMARY-CTX-RELEASE | `cuda-dev -- rc` | Exact CUDA primary-context release binding preserves the nominal device role. | `lib/ptx/cuda-driver-test.f`, `tools/ptx/cuda-launch.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-EVENT-CREATE | `ptr a n -- rc` | Exact CUDA event creation binding fixes the output to one writable cell and flags to scalar input. | `lib/ptx/cuda-driver-test.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-EVENT-DESTROY | `cuda-event -- rc` | Exact CUDA event destruction binding preserves the nominal event role. | `lib/ptx/cuda-driver-test.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-EVENT-RECORD | `cuda-event n -- rc` | Exact CUDA event record binding preserves the event role and stages the stream handle as a scalar. | `lib/ptx/cuda-driver-test.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-EVENT-SYNCHRONIZE | `cuda-event -- rc` | Exact CUDA event synchronization binding preserves the nominal event role. | `lib/ptx/cuda-driver-test.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| CU-EVENT-ELAPSED-TIME | `ptr a cuda-event cuda-event -- rc` | Exact CUDA elapsed-time binding fixes its float output to four writable bytes and preserves both event roles. | `lib/ptx/cuda-driver-test.f` | lib/ptx/cuda-driver.f | 2026-07-11 | stdlib-boundary | habu-ptx-m1-c-1df1d6e7 |
| c-package-record-match | `label label --` | Generated package-dictionary matcher compares the current token with one record and branches to the supplied match or next labels. | `test/seal-package.f`, `test/run.f` | src/habu/habu2.f | 2026-07-11 |  |  |
| c-package-prot-guard | `--` | Generated package reopen guard rejects any package whose public wordlist is registered as protected. | `test/seal-package.f`, `test/run.f` | src/habu/habu2.f | 2026-07-11 |  |  |
| p2f-entry | `label ptr a n n --` | Pass-2 typed-fetch dispatch consumes source-offset width and descriptor rows, emits validation, and executes the frozen bundle-fetch lowering. | `test/run.f` | src/habu/habu2.f | 2026-07-11 |  |  |
| INSTALL | `--` | Protected checker-hook installer owns the fixed `LOWER-CERT-HOOK:HOOK` and compile-immediate `PREFLIGHT` execution tokens, installs the protected preflight cell before arming the default fail-closed checker, and is the required prelude for every custom-hook install. | `tools/build-fixpoint-test.f`, `test/gate-aot-negative.f`, `test/top-row-hook-test.f`, `test/engine-suite.f`, `test/run.f` | src/core/check-hook.f | 2026-07-17 | stdlib-boundary | cap:checker-hook-identity |
| TR-INSTALL | `--` | Installs the tier-1 top-row tracker hook through the guarded-deref `set-top-check` trust-boundary prim (mirrors `LOWER-CERT-HOOK:INSTALL`'s `' HOOK set-check`); hook installation is not expressible in the checked language. | `test/top-row-warn-test.f`, `test/run.f` | src/core/top-row.f | 2026-07-14 | stdlib-boundary | cap:checker-hook-identity |
| TR-CERT-DOUT-EMPTY? | `ptr u8 n -- bool` | Unchecked boundary that calls the checker effect-read API (EFFECT-QUERY/EFFECT-DOUT-N) from the checked tier-1 tracker to detect a certified word producing no fixed outputs, so the row pops its declared din precisely instead of graying the tail; reading raw effect-store state is not expressible in the checked language. | `test/top-row-warn-test.f`, `test/run.f` | src/core/top-row.f | 2026-07-15 | stdlib-boundary | habu-checker-self-typing-9ff8ba86 |
| CHECKER-CERT-CALL | `ptr u8 n n n --` | Single dynamic-call boundary for the installed lowering-certificate producer; installation is private and single-assignment. | `tools/build-fixpoint-test.f`, `test/lower-cert.f`, `test/run.f` | src/core/checker.f | 2026-07-11 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| SCRIPT-BUILD-Z? | `ptr u8 -- bool` | Recognizes the internal `--build` argv marker in a raw argv c-string. | `tools/hb-cli-contracts-test.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-07-11 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-SOURCE-Z? | `ptr u8 -- bool` | Recognizes either source-list argv marker in a raw argv c-string. | `tools/hb-cli-contracts-test.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-07-11 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-SOURCE? | `-- bool` | Detects user-load or verified-compiler source-list mode from captured process argv. | `tools/hb-cli-contracts-test.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-07-11 | builder-emit | habu-raw-self-path-4514ffd3 |
| SNAP= | `[ R -- S ] [ R -- S ] --` | Typed depth-introspection comparator (dot habu-typed-depth-introspection-18f0efda) and the sole snapshot boundary now that the untyped `T{ -> }T` DSL is retired: the checker verifies the two quotations leave an identical row shape S at CHECK time, so a shape mismatch is rejected before runtime; only the depth-marked drain of each quotation's output row stays trusted, and both drains are inlined into this one word. Values are compared through the checked TS-* judge path. | `lib/test/snap-test.f`, `lib/array-test.f`, `test/run.f` | lib/test/snap.f | 2026-07-03 | test-metaprog | habu-typed-depth-introspection-18f0efda |
| BUILD-CHECK-RAW | `ptr u8 n -- n` | Build helper boundary around `CHECK!`; the checker cannot certify a source definition by evaluating its own checker recursively. | `lib/build-test.f`, `test/run.f` | lib/build.f | 2026-06-18 | stdlib-boundary | habu-primitive-effect-axiom-1119f176 |
| CHECK-QUIET-CANDIDATE! | `ptr u8 n -- n` | Shared test harness boundary that temporarily suppresses checker diagnostics and runs `CHECK-CANDIDATE!`; recursive checker invocation and the `DIAG-QUIET` suppression counter are centralized here. | `test/engine-suite.f`, `lib/array-test.f`, `lib/vector-test.f`, `lib/string-test.f`, `lib/json-write-test.f`, `tools/image-bytes-test.f`, `tools/asm-checked-test.f`, `lib/ptx/tile-test.f`, `lib/ptx/collective-test.f`, `test/run.f` | test/checker-assert.f | 2026-06-30 | test-metaprog | habu-primitive-effect-axiom-1119f176 |
| MBUF | `-- ptr u8` | Image-byte test reuses the raw checked boundary loaded from `src/os/image-bytes.f`; the test may run after the file is already baked, so it republishes the audited effect locally. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-BOUNDS-RC | `-- n` | Image-byte test republishes the raw bounds-error status accessor from the image writer boundary. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-RESET | `--` | Image-byte test republishes the raw image-writer reset effect from the audited image-byte boundary. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-LEN | `n -- len` | Image-byte test republishes the nominal length constructor used by image-writer negative fixtures. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-OFF | `n -- off` | Image-byte test republishes the nominal offset constructor used by image-writer negative fixtures. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-HERE | `-- n` | Image-byte test republishes the raw image cursor read effect from the image writer boundary. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| IMG-M8 | `n --` | Image-byte test republishes the raw byte emitter effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| IMG-M16 | `n --` | Image-byte test republishes the raw 16-bit emitter effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| IMG-M32 | `n --` | Image-byte test republishes the raw 32-bit emitter effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| IMG-M64 | `n --` | Image-byte test republishes the raw 64-bit emitter effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-BYTES-LEN | `ptr u8 len --` | Image-byte test republishes the typed byte-copy effect for role-confusion fixtures. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-NAME16-LEN | `ptr u8 len --` | Image-byte test republishes the typed fixed-name copy effect for role-confusion fixtures. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-PAD-OFF | `off --` | Image-byte test republishes the typed pad-to-offset effect for role-confusion fixtures. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-LE32@ | `off -- n` | Image-byte test republishes the typed little-endian read effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| TLP-W32 | `n n -- n` | Layout-lowering golden reader: reinterprets a compiled subject's xt as the byte base for one u32 instruction load — test-only code introspection, same class as the imgdump/jitdump readers; every use sits directly under the suite's golden asserts. | `test/type-layout-lower-pending.f`, `test/run.f` | test/type-layout-lower-pending.f | 2026-07-06 |  |  |
| TLP-UN2 | `tlp-res<n,n> -- n n` | Matching raw 2-cell unpack of the seeded width-2 bundle so plain value asserts can prove whole-bundle transport preservation. | `test/type-layout-lower-pending.f`, `test/run.f` | test/type-layout-lower-pending.f | 2026-07-06 |  |  |
| TLP-UN4 | `tlp-mix<n,n> -- n n n n` | Matching raw 4-cell unpack of the seeded width-4 bundle for the execution asserts. | `test/type-layout-lower-pending.f`, `test/run.f` | test/type-layout-lower-pending.f | 2026-07-06 |  |  |
| TLP-XT | `ptr u8 n -- n` | Golden-subject xt lookup via raw search-wl: the subjects carry wide effects, so their records are DNAME-WIDE and interpret `'` correctly fails closed; the goldens only read code bytes (documented raw-xt introspection residual, habu-tfam-12-interpret). | `test/type-layout-lower-pending.f`, `test/run.f` | test/type-layout-lower-pending.f | 2026-07-09 |  |  |
| M-LE32! | `n off --` | Image-byte test republishes the typed little-endian patch effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-LE64! | `n off --` | Image-byte test republishes the typed 64-bit patch effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-BE-RESET | `off --` | Image-byte test republishes the big-endian patch cursor reset effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-BE-HERE | `-- n` | Image-byte test republishes the big-endian patch cursor read effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-BE32 | `n --` | Image-byte test republishes the big-endian 32-bit emit effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-BE64 | `n --` | Image-byte test republishes the big-endian 64-bit emit effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| M-BE-BYTES-LEN | `ptr u8 len --` | Image-byte test republishes the big-endian typed byte-copy effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| MSIZE | `-- n` | Image-byte test republishes the image buffer capacity to drive the cursor-overflow regression (the silent maker-build failure class). | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-07-03 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| P5 | `-- i64` | Engine-suite trusted immediate around `POSTPONE`; the compile-time body emits `IM5`, while the declared effect is the runtime value compiled into `TP`. | `test/engine-suite.f`, `test/run.f` | test/engine-suite.f | 2026-06-24 | test-metaprog | habu-primitive-effect-axiom-1119f176 |
| PROP-CHECK-HOOK | `ptr u8 n -- n` | Property-test fail-closed source hook wraps `CHECK!`; recursive checker invocation cannot be certified by the checked source it protects. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| PROP-INSTALL-HOOK | `--` | Property-test installer rearms the canonical compile preflight before setting its fail-closed custom checker hook. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-07-17 |  |  |
| CLEAR-MEAS | `R n -- n` | Property-test oracle drains the arbitrary residual data-stack tail left by a generated program while preserving the measured count; this is exactly the value-agnostic depth boundary. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-22 |  |  |
| ERR@ | `-- n` | Reads the engine `evaluate` recovery cell from the live `data-base` header so the in-process property oracle can distinguish clean execution from recovered traps. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| MARK | `--` | Property-test checkpoint captures code, dictionary, and user-signature cursors; these raw interpreter stores are outside the checker model. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| FORGET | `--` | Property-test rollback restores code, dictionary, and user-signature cursors after a generated program; raw interpreter-state mutation is the boundary. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| SMARK | `--` | Nested property-test checkpoint for shrink/metamorphic probes captures code, dictionary, and user-signature cursors. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| SFORGET | `--` | Nested property-test rollback restores code, dictionary, and user-signature cursors after shrink/metamorphic probes. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| CHK-MARK | `--` | Candidate-check checkpoint captures interpreter state before evaluating one generated definition under the verdict hook. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| CHK-FORGET | `--` | Candidate-check rollback removes a generated definition when the checker verdict was not certified. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| CHK-HOOK | `ptr u8 n -- n` | Candidate verdict hook records `CHECK!` result but returns success so rejected generated definitions can be rolled back in-process. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| CHK | `ptr u8 n --` | Installs the candidate verdict hook, evaluates generated source, restores the fail-closed hook, and rolls back non-certified candidates. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| RUN-MEAS | `n n --` | Builds and evaluates the generated measurement program, records `LAST-MEAS` or `LAST-TRAP`, and normalizes dynamic evaluation paths that the checker cannot express directly. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| REND-SIG$ | `-- ptr u8 n` | Reads the checker's last rendered signature buffer for the property round-trip amplifier; renderer state is internal checker state. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| CONFIRM-FR? | `-- bool` | False-reject oracle deliberately compiles one generated program with checking disabled, restores the hook, and measures runtime behavior to prove a rejection was real incompleteness. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |  |  |
| AX-COUNT | `-- n` | Primitive-axiom census reads the live checker PES table row count (`#PE`); the internal axiom table is outside the checker model. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test-core.f | 2026-07-03 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| AX-NAME$ | `n -- ptr u8 n` | Primitive-axiom census recovers one PES axiom's folded primitive name from the checker symbol table. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test-core.f | 2026-07-03 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| AX-STK | `n -- n` | Primitive-axiom census walks a persistent effect-node stack list (`EN-PUSH` chain) to count its arity; raw checker node layout is the boundary. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test-core.f | 2026-07-03 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| AX-ARITY | `n -- n n` | Primitive-axiom census reads one PES axiom's declared data in/out arity from its effect record. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test-core.f | 2026-07-03 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| AXEVAL | `-- n` | Primitive-axiom census evaluates the generated per-axiom measurement runner in-process; dynamic `evaluate` is outside the checker model. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test-core.f | 2026-07-03 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| LIVE-COUNT | `-- n` | Primitive-effect inventory reads the live PES axiom row count (`#PE`) for the row-for-row cross-check; the internal axiom table is outside the checker model. | `tools/primitive-effect-inventory-test.f`, `test/run.f` | tools/primitive-effect-inventory.f | 2026-07-17 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| LIVE-PKG$ | `n -- ptr u8 n` | Primitive-effect inventory recovers one PES axiom's folded defining-package spelling from the checker symbol table. | `tools/primitive-effect-inventory-test.f`, `test/run.f` | tools/primitive-effect-inventory.f | 2026-07-17 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| LIVE-NAME$ | `n -- ptr u8 n` | Primitive-effect inventory recovers one PES axiom's folded primitive name from the checker symbol table. | `tools/primitive-effect-inventory-test.f`, `test/run.f` | tools/primitive-effect-inventory.f | 2026-07-17 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| LIVE-STK | `n -- n` | Primitive-effect inventory walks a persistent effect-node stack list (`EN-PUSH` chain) to count its arity; raw checker node layout is the boundary. | `tools/primitive-effect-inventory-test.f`, `test/run.f` | tools/primitive-effect-inventory.f | 2026-07-17 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| LIVE-ARITY | `n -- n n` | Primitive-effect inventory reads one PES axiom's declared data in/out arity from its effect record. | `tools/primitive-effect-inventory-test.f`, `test/run.f` | tools/primitive-effect-inventory.f | 2026-07-17 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| LIVE-TRUSTED-ONLY? | `n -- bool` | Primitive-effect inventory reads one PES axiom's `PE-TRUSTED-ONLY` flag (set by `PRIM-TRUSTED-ONLY!`); the flag word cell is internal checker state. | `tools/primitive-effect-inventory-test.f`, `test/run.f` | tools/primitive-effect-inventory.f | 2026-07-17 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| MOVZHW | `n n n -- n` | ARM64 source test reuses the raw unchecked encoder effect after conditional source loading or CLI-runner bake. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| ENC-ADD | `n n n -- n` | ARM64 source test republishes the raw add-instruction encoder effect for fixture assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| ENC-LDR | `n n n -- n` | ARM64 source test republishes the raw load-instruction encoder effect for fixture assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| ENC-LDAR | `n n -- n` | ARM64 source test republishes the raw acquire-load encoder effect for publication-order assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-07-15 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| ENC-BLR | `n -- n` | ARM64 source test republishes the raw branch-link-register encoder effect for fixture assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| >LIMM | `n -- n` | ARM64 source test republishes the immediate-layout helper effect used by encoder assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| ENC-ANDI | `n n n -- n` | ARM64 source test republishes the raw logical-immediate encoder effect for fixture assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| CW@ | `n -- ptr u8` | ARM64 source test republishes the code-buffer byte pointer boundary used to inspect emitted words. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| CODE-BYTE+ | `ptr u8 n -- ptr u8` | ARM64 source test republishes typed code-buffer byte-pointer arithmetic used by fixture reads. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| ARESET | `--` | ARM64 source test republishes the assembler-buffer reset effect for fixture setup. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| ADD, | `n n n --` | ARM64 source test republishes the raw instruction emitter effect for fixture setup. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| LDAR, | `n n --` | ARM64 source test republishes the acquire-load mnemonic effect for emitted-word assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-07-15 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| ASM-LEN | `-- n` | ARM64 source test republishes the assembler buffer length accessor effect for fixture assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| LIT64, | `n n --` | ARM64 source test republishes the literal-emitter effect for fixture setup. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 | test-metaprog | habu-builder-trust-rows-c5d41af6 |
| MEM-ALLOC-PTR | `n -- ptr u8` | Refines a raw anonymous `mmap` result into a typed byte pointer after size validation and `-1` failure checking; the checker cannot express this syscall-result refinement yet. | `lib/memory-test.f`, `test/run.f` | lib/memory.f | 2026-06-21 | stdlib-boundary | habu-typed-defining-words-aa224eb5 |
| ALLOC-BYTES>N | `CAD-NUM:alloc-byte-len -- n` | Private MEM (B5.5) proof-erasure projection: reads a validated `alloc-byte-len`'s raw cell solely for the `mmap` size operand, where the raw allocation primitive still consumes a bare `n`. MEM-private, no public export; byte/cell allocation roles cannot swap. Retire when the `mmap` size primitive accepts the nominal allocation role directly. | `lib/memory-test.f` | lib/memory.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| ALLOC-CELLS>N | `CAD-NUM:alloc-cell-count -- n` | Private MEM (B5.5) proof-erasure projection: reads a validated `alloc-cell-count`'s raw cell before the `cells` primitive in the cell-allocation sink, where that primitive still consumes a bare `n`. MEM-private, no public export; byte/cell allocation roles cannot swap. Retire when the `cells`/`mmap` allocation primitive accepts the nominal allocation role directly. | `lib/memory-test.f` | lib/memory.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| WB-SCOPE | `R CAD-NUM:alloc-byte-len [ R ptr u8 CAD-NUM:alloc-byte-len -- S ] -- S` | Exception-safe body of `MEM:WITH-BYTES`: allocates a mapping, runs the body quotation, and releases on BOTH normal return and throw. `catch` restricts to a stack-preserving quotation and a nested quotation captures no local, so the body's arbitrary result row `S` cannot be threaded through `catch` in checked code (same limit as `SNAP=`/`EACH`). The public `WITH-BYTES` is a thin CHECKED forwarder over it, so the signature stays checker-verified at call sites. Retire under linear owner types. | `lib/memory-test.f` | lib/memory.f | 2026-07-21 | stdlib-boundary | habu-mem-with-bytes-c5613bb5 |
| WB-RUN-CUR | `--` | Private `MEM:WITH-BYTES` frame op: pushes the current mapping (fat pointer + length) parked off the data stack and `EXECUTE`s its stored body xt; the true effect is row-polymorphic `( R ptr u8 CAD-NUM:alloc-byte-len -- S )`, which the checker cannot express for an xt fetched from a variable. Caught by a no-argument quotation so a throw leaves the row clean. | `lib/memory-test.f` | lib/memory.f | 2026-07-21 | stdlib-boundary | habu-mem-with-bytes-c5613bb5 |
| WB-REL-CUR | `--` | Private `MEM:WITH-BYTES` frame op: releases the current mapping via `MEM:RELEASE-BYTES`, reading the fat pointer and length from off-stack frame state (the fat pointer cannot be re-typed through a bare cell read). One release per scope; the outer frame is restored after, so a repeated release is structurally impossible. | `lib/memory-test.f` | lib/memory.f | 2026-07-21 | stdlib-boundary | habu-mem-with-bytes-c5613bb5 |
| IMG-MMAP-PTR | `n -- ptr u8` | Refines a raw file-backed `mmap` result into a typed byte pointer after checking the `-1` failure result; the checker cannot express syscall-result refinement yet. | `tools/imgdump-test.f`, `test/run.f` | tools/imgdump.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| CODE | `-- ptr u8` | Lazily maps the assembler output buffer outside DATA and refines the raw mmap result to the byte pointer used by `EMITW`, `BYTES,`, and image writers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/arch/arm64/icode.f | 2026-06-26 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| ICODE-TABS | `-- ptr n` | Lazily maps the assembler label/fixup table block outside DATA and refines the raw mmap result to the numeric-cell pointer used by `LBLP`/`FXS`/`FXN`/`FXK`/`FXH`. | `test/run.f`, `tools/build-fixpoint-test.f`, `test/icode-fixup-test.f` | src/arch/arm64/icode.f | 2026-07-14 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| ENV-DATA | `-- ptr n` | Returns the fixed engine data-region header pointer used for argc/argv/envp cells. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| ENV-DASH | `-- n` | Shared ASCII dash byte constant used by argv parsing helpers. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| ARGC | `-- n` | Reads the process argc value captured by the native startup entry. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| ARGV-BASE | `-- ptr ptr u8` | Refines the raw argv vector pointer read from the engine startup byte-offset cell. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| ENVP-BASE | `-- ptr ptr u8` | Refines the raw envp vector pointer read from the engine startup byte-offset cell. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| ZLEN | `ptr u8 -- n` | Measures a NUL-terminated byte string read from argv or envp. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| ARGV$ | `n -- ptr u8 n` | Converts one argv c-string pointer to a counted byte string. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| ENV-FALSE | `-- bool` | Produces a typed false flag for env and argv parsing helpers. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| ENV=? | `ptr u8 ptr u8 n -- bool` | Compares one envp c-string against a counted variable name and following `=`. | `lib/process-env-test.f`, `test/run.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| NULL$ | `-- ptr u8 n` | Returns a typed empty string pair used for absent environment values. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| GETENV | `ptr u8 n -- ptr u8 n` | Returns a counted environment value by scanning the raw envp vector captured at startup. | `lib/process-env-test.f`, `test/run.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| TMP-PATH-CAP | `-- n` | Fixed scratch capacity for target temp-path construction during pre-hook build drivers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| TPP@ | `-- ptr u8` | Reads the temporary-path scratch cursor from a raw variable. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| TMP-PATH-CHECK | `n --` | Validates the fixed target path scratch capacity before raw byte copies. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| TMP-PATH | `ptr u8 n -- ptr u8 n` | Builds `$HB_TMP` or `/tmp` child paths in pre-hook engine build drivers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-LOAD-Z? | `ptr u8 -- bool` | Recognizes the `--load` argv marker in a raw argv c-string. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-SEP? | `n -- bool` | Recognizes the `--` argv separator in source-list mode. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-ARG-START | `-- n` | Computes the first user script argument after source-list handling. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-ARGC | `-- n` | Returns user script argument count after source-list handling. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-ARGV | `n -- ptr u8` | Returns one user script argv c-string after source-list handling. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-ARGV$ | `n -- ptr u8 n` | Returns one user script argument as counted bytes. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-ARG-START | `-- n` | Computes the first user argument for standalone bundles, where argv[0] is the executable path. | `tools/hb-build-test.f`, `test/run.f` | src/habu/bundle-argv.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-ARGC | `-- n` | Returns standalone bundle user argument count. | `tools/hb-build-test.f`, `test/run.f` | src/habu/bundle-argv.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-ARGV | `n -- ptr u8` | Returns one standalone bundle user argv c-string. | `tools/hb-build-test.f`, `test/run.f` | src/habu/bundle-argv.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| SCRIPT-ARGV$ | `n -- ptr u8 n` | Returns one standalone bundle user argument as counted bytes. | `tools/hb-build-test.f`, `test/run.f` | src/habu/bundle-argv.f | 2026-06-28 | builder-emit | habu-raw-self-path-4514ffd3 |
| HB@ | `-- ptr u8` | Reads the stdin-engine baked-source buffer pointer stored in a raw variable. | `test/run.f` | src/habu/stdin.f | 2026-06-16 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| EVAL-HOST | `ptr u8 n --` | Compiles a REPL source buffer in the metabuild host dict for AOT capture; `evaluate`'s net effect is source-dependent so the boundary declares the balanced install-tail effect. | `test/run.f` | src/habu/stdin.f | 2026-07-03 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BLD-PB@ | `-- ptr u8` | Reads the standalone-build source buffer pointer stored in a raw variable. | `test/run.f`, `tools/hb-build.f` | src/habu/build.f | 2026-06-24 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| CHECK-BODY | `ptr u8 n -- n` | Shared source pre-verification recursively invokes the checker on an assembled definition body and renders the checker-owned uncheckable diagnostic before returning the verdict; recursive checker invocation and diagnostic-state access are the explicit verifier boundary. | `tools/hb-build-test.f`, `tools/check-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/verify-source.f | 2026-07-01 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| CHECK-DOES-BODY | `ptr u8 n ptr u8 n -- n` | Shared source pre-verification routes `DOES>` bodies through the checker's dedicated `CHECK-DOES!` entrypoint; ordinary `CHECK!` cannot model the created-word data-field pointer. | `tools/check-test.f`, `test/run.f` | src/habu/verify-source.f | 2026-06-28 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| TRUST-SIGNATURE | `ptr u8 n ptr u8 n --` | Shared source pre-verification records source-order defining-word signatures for parsed names; the checker cannot infer a dynamic mutation of its signature table from scanner state. | `tools/hb-build-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/verify-source.f | 2026-06-28 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| MULTI-ERR-MODE? | `-- bool` | Shared source pre-verification reads the checker-internal multi-error mode flag; the checker registry does not publish `MULTI-ERR?` to later checked loads, so the verify loop's continue-past-reject decision rides the same verifier boundary as `CHECK-BODY`. | `tools/check-all-errors-test.f`, `test/run.f` | src/habu/verify-source.f | 2026-07-07 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SIG-RAW-MODE! | `n --` | Shared source pre-verification toggles the checker-internal raw-definer signature mode (`SIG-RAW-DEFINER!`) around a raw storage definer's effect string, so create/variable/constant publish TVK-RAW cells; the checker registry does not publish `SIG-RAW-DEFINER!` to later checked loads, so it rides the same verifier boundary as `MULTI-ERR-MODE?`. | `test/pointer-storage-test.f`, `test/run.f` | src/habu/verify-source.f | 2026-07-15 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| CA-MULTI-BEGIN | `--` | The all-errors driver arms the checker-internal multi-error load mode around its single whole-buffer verify pass; mode control words are not registry-published to checked tool loads. | `tools/check-all-errors-test.f` | tools/check-all-errors-core.f | 2026-07-07 | builder-emit | habu-multi-err-checking-42db26f4 |
| CA-MULTI-END | `-- n` | Reads the multi-error reject count and clears the mode for the fail-closed exit decision; same unpublished-mode-word boundary as `CA-MULTI-BEGIN`. | `tools/check-all-errors-test.f` | tools/check-all-errors-core.f | 2026-07-07 | builder-emit | habu-multi-err-checking-42db26f4 |
| AOT-PB@ | `-- ptr u8` | Reads the AOT build source buffer pointer stored in a raw variable. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-lib.f | 2026-06-24 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| MAP-IN-BLOB | `ptr a ptr u8 -- n` | AOT relocation maps an old call-target byte address to its new CODE offset by walking a dictionary record's compacted blob span; the record-cell read plus the pointer round-trips through the scratch cells are outside checked pointer inference until the typed dictionary-record schema lands. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-lib.f | 2026-07-16 | builder-emit | habu-typed-dictionary-record-c67adddb |
| AOT-DBASE@ | `-- ptr a` | Reads the runtime dictionary base pointer for AOT dictionary-record scans; record fields are mixed, so callers specialize the pointee type at each access. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-closure.f | 2026-06-24 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| AOT-DBASE-N | `-- n` | Reads the runtime dictionary base as an integer for CELL-TEXTPTR?'s value-domain range test, which classifies a stripped-AOT data cell as a code/dict pointer by live dictionary extents instead of a magnitude window. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-closure.f | 2026-07-20 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| AOT-CP-N | `-- n` | Reads the live emitted-code high-water (CP) as an integer bounding the code span for the same live-extents pointer classification; the checker registry does not publish the primitive to the checked AOT tail. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-closure.f | 2026-07-20 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| AOT-PTR@ | `ptr a -- ptr a` | Reads a dictionary long-name pointer field whose pointee is another address; the checker cannot express this pointer-to-pointer load yet. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-closure.f | 2026-06-24 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| JSON-DIAGS | `-- ptr a` | AOT diagnostics read the checker's JSON-mode flag; the checker registry does not publish its own words to later checked loads, so the variable is typed as an axiom for the checked AOT tail. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-closure.f | 2026-07-07 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| CHECK! | `ptr u8 n -- n` | The AOT driver hook wraps the engine checker entrypoint for user source; the entrypoint's effect is modeled as a primitive axiom so the checked AOT tail compiles under the toolchain hook. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-closure.f | 2026-07-07 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| AOT-CTOR-EVAL | `ptr u8 n --` | The AOT maker compiles generated sumtype-constructor bodies with `evaluate` at its own interpret level; dynamic source evaluation cannot be expressed by the checker. | `test/run.f`, `tools/hb-build.f` | src/habu/aot.f | 2026-07-16 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| INSTALL-USER-HOOK | `--` | The AOT maker rearms the canonical compile preflight before installing its fail-closed user-source checker hook. | `test/run.f`, `tools/hb-build.f` | src/habu/aot.f | 2026-07-17 | builder-emit | cap:checker-hook-identity |
| MK-SBUF@ | `-- ptr u8` | Reads the hb-build maker source buffer pointer stored in a raw variable while compiling the separate maker image. | `tools/hb-build-test.f`, `test/run.f` | src/habu/maker.f | 2026-06-24 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| STB@ | `-- ptr u8` | Reads the snapshot source text base pointer stored in a raw variable. | `test/run.f`, `tools/build-fixpoint.f snap` | src/habu/snap-lib.f | 2026-06-26 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| STB-CELL@ | `-- ptr n` | Reads the snapshot source text base pointer as a cell-address for executable-header size lookup. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-06-26 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SDB@ | `-- ptr u8` | Reads the snapshot dictionary/data-region pointer stored in a raw variable. | `test/run.f`, `tools/build-fixpoint.f snap` | src/habu/snap-lib.f | 2026-06-26 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SNAP-CHECK-HOOK | `ptr u8 n -- n` | Snapshot image installs the fail-closed checker hook into emitted images that need a fresh hook; recursive `CHECK!` hook bodies are trusted boundaries. | `test/run.f`, `test/gate-debug.f` | src/habu/snap-lib.f | 2026-06-26 |  |  |
| SNAP-INSTALL-HOOK | `--` | Snapshot image rearms the canonical compile preflight before installing its fail-closed snapshot checker hook. | `test/run.f`, `test/gate-debug.f` | src/habu/snap-lib.f | 2026-07-17 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SNC-PTR | `-- ptr u8` | Scratch snapshot region view over a raw anonymous mmap; canonical-base writer scratch. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SNC-TEXT-N | `-- n` | Reads the saved text base cell as a plain integer for relocation band math. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SND-PTR | `-- ptr u8` | Scratch snapshot data view over a raw anonymous mmap; live-cell zeroing target. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SND-ZERO-CELL | `n --` | Zeroes one loader-overwritten live cell in the data scratch copy by layout offset. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SND-ZERO-SPAN-CELL | `n --` | Zeroes one evaluate-frame cell in the data scratch copy by layout offset. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SND-QUARANTINE@ | `n -- n` | Reads one quarantined dangling-pointer offset from the create table for scratch zeroing. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SNAP-RETIRE-GO | `--` | Snapshot build driver executes the fixed first-cold-checker `CHECKER-SNAPSHOT-PREPARE` hook, retires the builder tail, prepares the payload checker, and writes the image; the fixed cell carries a dynamic xt, SNAPGO lives in require'd snap-lib.f outside the assembled snap source that the staged fixpoint pre-pass certifies, and the snapshot-prepare words are prefix-internal with no charted effects. Replaces snap.f's former `0 set-check` window so the generated snap source stays free of raw check-off lines for BF-AUDIT-BOUNDARY. | `tools/build-fixpoint-test.f`, `test/engine-suite.f`, `test/run.f` fixpoint phase | src/habu/snap.f | 2026-07-15 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| S2-PATH-CAP | `-- n` | Fixed path-buffer capacity for the stage2 fixpoint driver. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/stage2.f | 2026-06-26 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| S2-PATH-BUF | `-- ptr u8` | Stage2 fixpoint path scratch buffer used while building private artifact paths. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/stage2.f | 2026-06-26 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SBUF@ | `-- ptr u8` | Reads the stage2 source buffer pointer stored in a raw variable. | `test/run.f` | src/habu/stage2.f | 2026-06-26 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| IMGD-MMAP-PTR | `n -- ptr u8` | Converts the raw image mmap result into a typed byte pointer after checking mmap failure; OS mapping pointers are outside checker inference. | `tools/imagedisasm-test.f`, `test/run.f` | tools/imagedisasm.f | 2026-06-25 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| MK-SPAN | `ptr<space-global,t> u32 -- span<space-global,t,fresh-extent-n>` | PTX from-raw-parts boundary: consumes a runtime extent assertion and retypes the base pointer as a span with a fresh rigid extent token. The checker cannot validate allocation length. | `lib/ptx/tile-test.f`, `test/run.f` | lib/ptx/tile.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| MK-SPAN-ONCE | `ptr<space-global,t> u32 -- span<space-global-once,t,fresh-extent-n>` | PTX from-raw-parts boundary for an externally proven read-once/affine gradient buffer; it mints a distinct `space-global-once` span, not a cast from an ordinary span. | `lib/ptx/tile-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| MK-SPAN= | `ptr<space-global,t> ptr<space-global,u> u32 -- span<space-global,t,fresh-extent-n> span<space-global,u,fresh-extent-n>` | PTX from-raw-parts boundary for two buffers sharing one asserted runtime extent; the repeated fresh template stamps both output spans with the same rigid extent token. | `lib/ptx/tile-test.f`, `test/run.f` | lib/ptx/tile.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| MK-MATRIX | `ptr<space-global,t> u32 u32 -- matrix<space-global,t,fresh-extent-r,fresh-extent-c>` | PTX dense row-major matrix from-raw-parts boundary: consumes asserted row/column extents and retypes the base pointer as a matrix. The checker cannot validate allocation shape. | `lib/ptx/collective-test.f`, `test/run.f` | lib/ptx/tile.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| MK-MATRIX-ONCE | `ptr<space-global,t> u32 u32 -- matrix<space-global-once,t,fresh-extent-r,fresh-extent-c>` | PTX dense row-major matrix from-raw-parts boundary for externally proven read-once rows; it mints a distinct `space-global-once` matrix used by row once words. | `lib/ptx/collective-test.f` | lib/ptx/tile.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| GRID-CTX | `span<space-global,t,e> -- gridctx<b,e,fresh-mask-live>` | PTX tile-DSL v0: derives a flat grid-strided context from a global span and mints a fresh rigid mask token for that context; lowers to PTX index/mask setup the checker cannot infer (a tile primitive). | `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-06-29 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| GRID-CTX-ONCE | `span<space-global-once,t,e> -- gridctx<b,e,fresh-mask-live>` | PTX tile-DSL read-once context derivation: same index/mask lowering as GRID-CTX but only accepts `space-global-once` spans. | `lib/ptx/tile-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| COOP-CTX | `span<space-global,t,e> -- coopctx<b,e,fresh-mask-live>` | PTX cooperative shared-memory context: derives a block-uniform staging context from `%tid.x` without an early bounds branch, so all lanes reach shared-memory barriers. | `lib/ptx/tile-smem-test.f`, `lib/ptx/tile-test.f`, `tools/ptx/smem-cg.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| FANIN-CTX | `ptr<space-global,t> -- fanctx<b,extent-n,fresh-mask-live>` | PTX fan-in scalar context: mints a fresh rigid active-lane mask over `%r1` lanes while preserving scalar addressing, so fan-in VJPs cannot be expressed as ordinary lane-indexed spans. | `lib/ptx/tile-test.f`, `tools/ptx/scatter-add-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| FANIN-LOAD | `ptr<space-global,t> fanctx<b,e,m> -- tile<t,b,m>` | PTX fan-in scalar load: broadcasts one scalar global cell to every active lane under a fan-in context; rejects ordinary grid contexts. | `lib/ptx/tile-test.f`, `tools/ptx/scatter-add-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| INDEX-CTX | `span<space-global,u32,i> span<space-global,t,e> -- idxctx<b,i,e,fresh-mask-live>` | PTX indexed-memory context: mints a fresh active-lane mask over the index span and carries both index extent `i` and data extent `e`, so arbitrary gather/scatter words cannot mix index and data shapes. | `lib/ptx/tile-test.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| UNIQUE-INDEX-CTX | `span<space-global,u32,i> span<space-global,t,e> -- uniqidxctx<b,i,e,fresh-mask-live>` | PTX indexed-memory context plus an audited external uniqueness witness for `idx`; plain indexed stores require this witness instead of assuming duplicate-free indices. | `lib/ptx/tile-test.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| INDEX-DENSE-LOAD | `span<space-global,t,i> idxctx<b,i,e,m> -- tile<t,b,m>` | PTX dense-side companion load under an indexed context: reads lane `i` from a span whose extent matches the index span, preserving the indexed context mask. | `lib/ptx/tile-test.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| UNIQUE-INDEX-DENSE-LOAD | `span<space-global,t,i> uniqidxctx<b,i,e,m> -- tile<t,b,m>` | PTX dense-side load for unique-index kernels; same dense lane access as `INDEX-DENSE-LOAD` while preserving the uniqueness witness for a later plain indexed store. | `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-07-01 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| INDEX-LOAD | `span<space-global,u32,i> span<space-global,t,e> idxctx<b,i,e,m> -- tile<t,b,m>` | PTX generic indexed gather: loads `data[idx[i]]` with a runtime `idx[i] < e` guard while the checker enforces shared index/data extent tokens through `idxctx`. | `lib/ptx/tile-test.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| REP1 | `a [ n -- n ] -- a` | PTX phantom-preserving effects (dot habu-ptx-phantom-preserving): applies a checked unary register emitter to a single-cell kernel token's `n` register and returns the SAME phantom `a`. The `a<->n` from-register identity inside `q execute` is the only trusted coercion (the codegen analogue of the cg.f `*-REG` mints); forge/kind/arity safety is the checker's own unification over `a` and the `[ n -- n ]` quotation. Retires the per-op RELU / EXP. / NEG / RELU-V4 / RELU.V4 boundaries. | `lib/ptx/rep-test.f`, `lib/ptx/rep-neg-test.f` | lib/ptx/rep.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| REP2 | `a a [ n n -- n ] -- a` | PTX phantom-preserving effects: applies a checked binary register emitter to two operands of ONE shared phantom `a` and returns `a`. Forge is rejected because both operands and the result must unify to one `a` (an `mmaslice`/`mmbslice` mix cannot relabel); a wide layout family cannot bind single-cell `a` (kind); the `[ n n -- n ]` quotation pins arity. Retires the per-op `+.` / `-.` / `*.` / `/.` / `U/` and their V4/`.V4` variants. | `lib/ptx/rep-test.f`, `lib/ptx/rep-neg-test.f` | lib/ptx/rep.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| REPMIX2 | `a b [ n n -- n ] -- a` | PTX phantom-preserving effects: applies a checked binary register emitter preserving the FIRST operand's phantom `a` while consuming an independent single-cell operand `b`; the output must equal `a`, so it cannot forge a different family. Retires the per-op SCALE / B- / B/ and their V4/`.V4` variants. | `lib/ptx/rep-test.f`, `lib/ptx/rep-neg-test.f` | lib/ptx/rep.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| REPMIX3 | `a b c [ n n n -- n ] -- a` | PTX phantom-preserving effects (dot habu-ptx-phantom-preserving), leg 2a: applies a checked ternary register emitter preserving the FIRST operand's phantom `a` while consuming two independent single-cell operands `b c`; the output must equal `a`, so it cannot forge a different family; a wide operand cannot bind single-cell `a` (kind); the `[ n n n -- n ]` quotation pins arity. Retires the ACC-FMA boundary. | `lib/ptx/rep-test.f`, `lib/ptx/rep-neg-test.f` | lib/ptx/rep.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| REPMIX3B | `a b c [ n n n -- n ] -- b` | PTX phantom-preserving effects, leg 2a: applies a checked ternary register emitter preserving the SECOND operand's phantom `b` — the FMA-shaped ops whose result phantom is the MIDDLE operand, not the first (FMA., BLOCK-MAX-SELECT). The emitter's needed register order IS the declared operand order, so the wrapper reshuffles nothing and the lowering is byte-identical; the output must equal `b`, so it cannot forge the first family. Retires the FMA. / BLOCK-MAX-SELECT boundaries. | `lib/ptx/rep-test.f`, `lib/ptx/rep-neg-test.f` | lib/ptx/rep.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| SINK3 | `a b c [ n n n -- ] --` | PTX phantom-preserving effects, leg 2a: applies a checked ternary SINK emitter that CONSUMES three independent single-cell operands `a b c` and returns nothing (the store class). The sink mints no phantom, so an output declaration rejects (mint/forge); a wide operand cannot bind single-cell `a` (kind); the `[ n n n -- ]` quotation pins arity. Retires the STORE / STORE-ONCE / SCATTER-ADD / FANIN-SCATTER-ADD / INDEX-DENSE-STORE / ROW-STORE / ROW-STORE-ONCE / ROW-SCATTER-ADD / SSTORE / STORE-V4 / STORE.V4 boundaries. | `lib/ptx/rep-test.f`, `lib/ptx/rep-neg-test.f` | lib/ptx/rep.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| SINK4 | `a b c d [ n n n n -- ] --` | PTX phantom-preserving effects, leg 2a: the 4-operand indexed SINK — same sink discipline as SINK3 with one more single-cell operand; the `[ n n n n -- ]` quotation pins the wider arity. Retires the INDEX-SCATTER-ADD / INDEX-STORE boundaries. | `lib/ptx/rep-test.f`, `lib/ptx/rep-neg-test.f` | lib/ptx/rep.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| MINT-LOAD | `span<s,t,e> gridctx<b,e,m> [ n n -- n ] -- tile<t,b,m>` | PTX checked-mint capability (dot habu-ptx-phantom-preserving, leg 2b): applies a checked register emitter and REPACKAGES the span+gridctx operands into a `tile` whose type args are all PROJECTED from the operands (element from the span, block+mask from the ctx). Forge is rejected two ways: the declared types PIN the projection (an element/block relabel rejects by unification, an `acc` re-tag rejects by family), and the checked-mint output-provenance seal (`src/core/checker.f` NP-MINT-CHECK) rejects a `:` caller that declares an input-unbound output var. The `a<->n` from-register coercion inside `execute` is the only trusted boundary. Retires the LOAD / LOAD-ONCE boundaries. | `lib/ptx/mint-test.f`, `lib/ptx/mint-neg-test.f` | lib/ptx/rep.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| MINT-ROW-SPAN | `matrix<s,t,e,k> rowidx<e> [ n n -- n ] -- span<s,t,k>` | PTX checked-mint capability, leg 2b: repackages a matrix + row index into the row's column `span` (element+col-extent projected from the matrix, row extent consumed by the index). Same two forge layers as MINT-LOAD (projection pinned, provenance seal). Retires the ROW-SPAN / ROW-SPAN-ONCE boundaries. | `lib/ptx/mint-test.f`, `lib/ptx/mint-neg-test.f` | lib/ptx/rep.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| MINT-ROW-LOAD | `span<s,t,k> rowctx<b,k,m> [ n n -- n ] -- tile<t,b,m>` | PTX checked-mint capability, leg 2b: repackages a span + row context into a `tile` (element from the span, block+mask from the row ctx). Same two forge layers as MINT-LOAD. Retires the ROW-LOAD / ROW-LOAD-ONCE boundaries. | `lib/ptx/mint-test.f`, `lib/ptx/mint-neg-test.f` | lib/ptx/rep.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| TILE-LOOP | `n tile<t,b,m> [ tile<t,b,m> -- tile<t,b,m> ] -- tile<t,b,m>` | PTX tile-DSL checked counted loop (K-reduction / streaming): applies an accumulator-preserving body `n` times. The checker enforces the body's `( tile -- tile )` effect at every call site (capability (a) of habu-checker-capability-typed); the emit unroll lowers to PTX the checker cannot infer (a tile primitive). | `lib/ptx/tile-loop-test.f`, `lib/ptx/tile-loop-neg-test.f` | lib/ptx/tile-loop.f | 2026-06-27 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| STAGE | `span<space-global,t,e> coopctx<b,e,m> -- span<space-shared,t,e>` | PTX tile-DSL shared-memory staging (capability (b)): cooperatively copies a global block into `SMEM`, emits `bar.sync`, and returns a `space-shared` span. It rejects elementwise `gridctx` so lanes cannot branch around barriers. | `lib/ptx/tile-smem-test.f`, `lib/ptx/tile-smem-neg-test.f`, `tools/ptx/smem-cg.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile-smem.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| SLOAD | `span<space-shared,t,e> coopctx<b,e,m> -- tile<t,b,m>` | PTX tile-DSL shared load (capability (b)): reads a register tile from a `space-shared` span under the same cooperative mask; rejects a `space-global` span and rejects elementwise contexts. | `lib/ptx/tile-smem-test.f`, `lib/ptx/tile-smem-neg-test.f`, `tools/ptx/smem-cg.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile-smem.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| ACC-ZERO | `gridctx<b,e,m> -- acc<t,b,m>` | PTX tile-DSL register accumulator (capability (c)): a fresh zeroed accumulator of the new `acc<t,b,m>` type (distinct from tile<>, never unify). Emits `mov.f32 0f0` via cg.f the checker cannot infer; device-verified (tools/ptx/acc-device-test.f). | `lib/ptx/tile-acc-test.f`, `lib/ptx/tile-acc-neg-test.f` | lib/ptx/tile-acc.f | 2026-06-27 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| ACC-TILE | `acc<t,b,m> -- tile<t,b,m>` | PTX tile-DSL accumulator finalize (capability (c)): the completion gate - converts an `acc<>` to a storable `tile<>` so an unfinalized accumulator cannot be stored to global. Identity in emit (the accumulator register is the result tile); device-verified (tools/ptx/acc-device-test.f). | `lib/ptx/tile-acc-test.f`, `lib/ptx/tile-acc-neg-test.f` | lib/ptx/tile-acc.f | 2026-06-27 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| ACC-LOOP | `n acc<t,b,m> [ acc<t,b,m> -- acc<t,b,m> ] -- acc<t,b,m>` | PTX tile-DSL accumulator-typed counted loop (capability (c)): the K-reduction over an `acc<>` accumulator, enforcing an accumulator-preserving body. Emit unrolls; lowers to PTX the checker cannot infer (a tile primitive). | `lib/ptx/tile-acc-test.f` | lib/ptx/tile-acc.f | 2026-06-27 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| GRID-CTX-V4 | `span<space-global,t,e> -- gridctx<b,e,fresh-mask-live>` | PTX v4 tile DSL: derives a flat grid context where each thread owns four consecutive elements and mints a fresh rigid mask token for that context; general `N` is handled by scalar residual lanes in load/store. | `lib/ptx/tile-v4-test.f` | lib/ptx/tile-v4.f | 2026-06-29 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| V4-ALIGN | `span<space-global,t,e> -- vspan<space-global,t,e>` | PTX M10 vec4 alignment obligation: the trusted boundary that asserts a global span's base is 16-byte aligned (like MK-SPAN asserts extent) and re-tags it as a `vspan`. Identity in emit (the base is unchanged); the only route to a vspan, so a vectorized access on an unaligned base is a fail-closed type error. | `lib/ptx/tile-v4a-test.f`, `lib/ptx/tile-v4a-neg-test.f` | lib/ptx/tile-v4a.f | 2026-07-15 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| GRID-CTX.V4 | `span<space-global,t,e> -- gridctx<b,e,fresh-mask-live>` | PTX M10 vec4 tile DSL: derives a flat grid context where each thread owns four consecutive elements and mints a fresh rigid mask; touches no memory so it needs no alignment proof (a plain span). | `lib/ptx/tile-v4a-test.f`, `lib/ptx/tile-v4a-neg-test.f` | lib/ptx/tile-v4a.f | 2026-07-15 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| LOAD.V4 | `vspan<space-global,t,e> gridctx<b,e,m> -- vtile<t,b,m>` | PTX M10 typed vec4 load: consumes a 16B-proven `vspan` (alignment obligation) and yields a `vtile` (the vec4 lane type, distinct from scalar tile<>). Lowers to `ld.global.v4.f32` plus the @%p-guarded scalar residual tail; emit shared with tile-v4.f. | `lib/ptx/tile-v4a-test.f`, `lib/ptx/tile-v4a-neg-test.f` | lib/ptx/tile-v4a.f | 2026-07-15 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| PIPE-SETUP | `matrix<space-global,f32,m,k> matrix<space-global,f32,k,q> matrix<space-global,f32,m,q> -- mmctx<m,k,q>` | Pipelined-GEMM tile DSL: tile/thread coordinate derivation (ctaid/tid decomposition, shared base) consuming the typed A[M,K]*B[K,N]->C[M,N] operands; emit is MM-THREAD-SETUP verbatim. | `lib/ptx/tile-pipe-test.f`, `lib/ptx/tile-pipe-neg-test.f` | lib/ptx/tile-pipe.f | 2026-07-16 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| PIPE-ACC-ZERO | `mmctx<m,k,q> -- mmctx<m,k,q> mmracc<f32,block-256,geom-mt4x4,mask-live>` | Pipelined-GEMM tile DSL: mints the zeroed 16-register 4x4 micro-tile accumulator (%f10..%f25); emit is MM-ACC-ZERO-EMIT verbatim. `mmracc` is nominally distinct from `tile`/`acc`, so naive stores on it reject. | `lib/ptx/tile-pipe-test.f`, `lib/ptx/tile-pipe-neg-test.f` | lib/ptx/tile-pipe.f | 2026-07-16 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| PIPE-LOOP | `mmctx<m,k,q> mmracc<f32,block-256,g,w> [ mmstage<f32,block-256,geom-as64x32-bs32x64,w,p> mmracc<f32,block-256,g,w> -- mmracc<f32,block-256,g,w> ] -- mmctx<m,k,q> mmracc<f32,block-256,g,w>` | Pipelined-GEMM tile DSL: the cp.async double-buffered software pipeline (prologue stage, runtime $KLOOP, prefetch, commit_group/wait_group, barriers, parity flip); emit is MM-PIPE-KLOOP-WITH verbatim. The body receives one READY stage per iteration (parity p) and must preserve the accumulator. Trusted for the `mmstage` mint + raw xt execute of the folded-in compute-slot adapter (absorbs the former PIPE-RUN). The dynamic pending/committed/ready PROTOCOL ORDERING is now a CHECKED discipline (lib/ptx/cpp-slot.f); only the runtime loop-carried parity ALTERNATION stays a runtime-dataflow property outside an emit-time checker. | `lib/ptx/tile-pipe-test.f`, `lib/ptx/tile-pipe-neg-test.f` | lib/ptx/tile-pipe.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| STAGE-SLICES | `mmstage<f32,block-256,geom-as64x32-bs32x64,w,p> -- mmaslice<f32,block-256,geom-as64x32-bs32x64,w,p> mmbslice<f32,block-256,geom-as64x32-bs32x64,w,p>` | Pipelined-GEMM tile DSL: splits the blocked 2-D staged layout into its strided A slice (scalar-only) and contiguous 16B-aligned B slice (v4-legal); emit is MM-CUR-BASES verbatim. The only mmbslice constructor, so the v4 alignment obligation is asserted exactly here (the vspan analogue). | `lib/ptx/tile-pipe-test.f`, `lib/ptx/tile-pipe-neg-test.f` | lib/ptx/tile-pipe.f | 2026-07-16 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| A-FRAG | `mmaslice<f32,block-256,geom-as64x32-bs32x64,w,p> n -- mmafrag<f32,block-256,geom-as64x32-bs32x64,w,p>` | Pipelined-GEMM tile DSL: one k-column A operand fragment - 4 strided scalar ld.shared (%f26..29); emit is MM-KSTEP-A verbatim. | `lib/ptx/tile-pipe-test.f`, `lib/ptx/tile-pipe-neg-test.f` | lib/ptx/tile-pipe.f | 2026-07-16 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| B-FRAG.V4 | `mmbslice<f32,block-256,geom-as64x32-bs32x64,w,p> n -- mmbfrag<f32,block-256,geom-as64x32-bs32x64,w,p>` | Pipelined-GEMM tile DSL: the vectorized shared load - one ld.shared.v4.f32 (%f30..33); emit is MM-KSTEP-B verbatim. Demands the 16B-alignment-proven contiguous mmbslice; a strided mmaslice or plain shared span is a fail-closed type error. | `lib/ptx/tile-pipe-test.f`, `lib/ptx/tile-pipe-neg-test.f` | lib/ptx/tile-pipe.f | 2026-07-16 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| COMMIT | `cpp-pending<p> -- cpp-committed<p>` | cp.async pipeline-slot typestate (package CPPSLOT): closes the current cp.async issue group so the in-flight copies become a committed group (pending -> committed); wraps the CPP-COMMIT step emitter. Trusted only for the slot's phantom register coercion (the cp.async issue owns the mint); the ORDERING is checked at every caller. | `lib/ptx/cpp-slot-test.f`, `lib/ptx/cpp-slot-neg-test.f` | lib/ptx/cpp-slot.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| WAIT | `cpp-committed<p> -- cpp-ready<p>` | cp.async pipeline-slot typestate (package CPPSLOT): drains the committed group and bar.sync-fences it so the staged tile is block-visible (committed -> ready); wraps CPP-WAIT + CPP-SYNC. committed->ready is a block barrier: CTL-BARRIER-flagged (checker.f PTX-CPWAIT-ROWS?), so a WAIT under divergent control rejects, composing with the M5 model. | `lib/ptx/cpp-slot-test.f`, `lib/ptx/cpp-slot-neg-test.f` | lib/ptx/cpp-slot.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| MMA-STAGE-ISSUE | `n n -- cpp-pending<p>` | Single-buffer cp.async ISSUE mint for the CHECKED MMA-PIPE-KLOOP-SINGLE protocol (dot habu-ptx-phantom-preserving-3df9db92): emits the As/Bs stage (MMA-CP-STAGE verbatim) and mints the iteration's cpp-pending slot witness. Trusted only for the phantom mint - the NP-MINT-CHECK seal proved fresh nominal mints are trusted BY DESIGN (a checked word cannot fabricate the family cell), so this is the audited-mint-core class, identical in kind to the CPPSLOT COMMIT/WAIT transitions. The commit->wait->read ORDERING it enters is checked at the caller: wait-before-commit, dropped-wait, and read-after-issue all reject fail-closed on this very mint (the production-shaped falsification regressions), and byte identity across every pinned MMA config is pinned by the off-device golden dump. | `lib/ptx/cg-mma-slot-neg-test.f`, `tools/ptx/mma-gemm-check.f` | lib/ptx/cg-mma.f | 2026-07-17 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| ROW | `-- rowidx<e>` | PTX tile-DSL M6: blockIdx.x as a row index proven < R under the launch ABI; a primitive the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| ROW-CTX | `span<space-global,t,k> -- rowctx<b,k,fresh-mask-live>` | PTX tile-DSL M6: one-block-per-row context (lane = tid, mask = tid < N) and fresh rigid mask token for that context; a primitive the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| ROW-CTX-ONCE | `span<space-global-once,t,k> -- rowctx<b,k,fresh-mask-live>` | PTX tile-DSL read-once row context derivation; same index/mask lowering as ROW-CTX but only accepts once-space spans. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| BLOCK-MAX | `tile<f32,b,m> -- uniform<f32>` | PTX tile-DSL M6: shared-memory thread-0 fold over `PTX-BLOCK@` lanes; inactive lanes contribute max identity (-inf) at the reducer, independent of the tile value. Warp-shfl remains future perf work. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| BLOCK-SUM | `tile<f32,b,m> -- uniform<f32>` | PTX tile-DSL M6: shared-memory thread-0 fold over `PTX-BLOCK@` lanes; inactive lanes contribute sum identity (0) at the reducer, so direct row sums and backward cotangents do not depend on `ROW-LOAD` seeding. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| BROADCAST | `uniform<f32> -- tile<f32,b,m>` | PTX tile-DSL AD: fills a tile from a uniform (named form of the broadcast in `PTX:B-`/`PTX:B/`); the mutual adjoint of BLOCK-SUM for reverse-mode AD; a primitive the checker cannot infer. | `lib/ptx/autograd-test.f` | lib/ptx/collective.f | 2026-06-29 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| UN | `-- uniform<f32>` | PTX tile-DSL RMSNorm: the row's active-lane count n (=%r1, the col extent k) as an f32 uniform — the mean/RMS reduction denominator; mints a uniform from the kernel ABI k register (no operand to project from), a primitive the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-07-20 | stdlib-boundary | habu-ptx-opt-layer-325b9507 |
| SAVED-X | `-- tile<f32,b,m>` | PTX tile-DSL AD saved-value: a nonlinear adjoint's saved forward input tile; materialised by the save-vs-recompute pass the checker cannot infer (body throws E-PTX-NOIMPL pending buffer reload, habu-ad-thread-saved). | `lib/ptx/ad-saved-test.f` | lib/ptx/ad-saved.f | 2026-06-27 | stdlib-boundary | habu-adg-lowering-multi-24043a69 |
| SAVED-Y | `-- tile<f32,b,m>` | PTX tile-DSL AD saved-value: a nonlinear adjoint's saved forward output tile (EXP. bwd = dz*y); body throws E-PTX-NOIMPL pending buffer reload. | `lib/ptx/ad-saved-test.f` | lib/ptx/ad-saved.f | 2026-06-27 | stdlib-boundary | habu-adg-lowering-multi-24043a69 |
| SAVED-Z | `-- tile<f32,b,m>` | PTX tile-DSL AD saved-value: `PTX:B/`'s saved output tile z (ds = -Sum(dz*z)/s); body throws E-PTX-NOIMPL pending buffer reload. | `lib/ptx/ad-saved-test.f` | lib/ptx/ad-saved.f | 2026-06-27 | stdlib-boundary | habu-adg-lowering-multi-24043a69 |
| SAVED-MX | `-- uniform<f32>` | PTX tile-DSL AD saved-value: BLOCK-MAX's saved block-uniform max (arg-max select); body throws E-PTX-NOIMPL pending buffer reload. | `lib/ptx/ad-saved-test.f` | lib/ptx/ad-saved.f | 2026-06-27 | stdlib-boundary | habu-adg-lowering-multi-24043a69 |
| SAVED-S | `-- uniform<f32>` | PTX tile-DSL AD saved-value: `PTX:B/`'s saved block-uniform divisor s; body throws E-PTX-NOIMPL pending buffer reload. | `lib/ptx/ad-saved-test.f` | lib/ptx/ad-saved.f | 2026-06-27 | stdlib-boundary | habu-adg-lowering-multi-24043a69 |
| SPAN-REG | `n -- span<space-global,f32,extent-n>` | PTX codegen: from-register identity cast - a kernel arg is a PTX register number, this asserts its span type so the emit driver runs the checked kernel checked (the codegen from_raw_parts boundary). | `tools/ptx/saxpy-cg.f` | lib/ptx/cg.f | 2026-06-29 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| UNIFORM-REG | `n -- uniform<f32>` | PTX codegen: from-register identity cast asserting a register holds a uniform scalar param; thin boundary so the emit driver stays checked. | `tools/ptx/saxpy-cg.f` | lib/ptx/cg.f | 2026-06-29 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| PTR-REG | `n -- ptr<space-global,f32>` | PTX codegen: from-register identity cast asserting a kernel arg register holds a scalar global f32 pointer for checked fan-in emit drivers. | `tools/ptx/scatter-add-grad-cg.f`, `tools/ptx/scatter-add-gradcheck.f` | lib/ptx/cg.f | 2026-07-01 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| SPAN-ONCE-REG | `n -- span<space-global-once,f32,extent-n>` | PTX codegen: from-register identity cast asserting a kernel arg register holds a read-once/affine span for checked once-space emit tests. | `tools/ptx/once-cg.f`, `tools/ptx/saxpy-test.f` | lib/ptx/cg.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| INDEX-SPAN-REG | `n -- span<space-global,u32,extent-i>` | PTX codegen: from-register identity cast asserting a kernel arg register holds a u32 index span for generic indexed gather/scatter emit drivers. | `tools/ptx/indexed-scatter-cg.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/cg.f | 2026-07-01 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| INDEX-VALUE-SPAN-REG | `n -- span<space-global,f32,extent-i>` | PTX codegen: from-register identity cast asserting a kernel arg register holds dense per-index values with the same extent as the index span. | `tools/ptx/indexed-scatter-cg.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/cg.f | 2026-07-01 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| DATA-SPAN-REG | `n -- span<space-global,f32,extent-d>` | PTX codegen: from-register identity cast asserting a kernel arg register holds the indexed data span whose extent is checked separately from the index span. | `tools/ptx/indexed-scatter-cg.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/cg.f | 2026-07-01 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| MATRIX-REG | `n -- matrix<space-global,f32,extent-r,extent-c>` | PTX codegen: from-register identity cast asserting a kernel arg register holds a row-major matrix, so row-kernel emit drivers run checked bodies. | `tools/ptx/softmax-launch.f`, `tools/ptx/saxpy-test.f` | lib/ptx/cg.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| MATRIX-ONCE-REG | `n -- matrix<space-global-once,f32,extent-r,extent-c>` | PTX codegen: from-register identity cast asserting a kernel arg register holds a read-once row-major matrix for checked once-space row emit tests. | `lib/ptx/collective-test.f` | lib/ptx/cg.f | 2026-06-30 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| R>BITS | `r -- n` | PTX codegen f64->f32 marshalling: reinterpret a Habu 64-bit float as its bit pattern (the one thin cast; F64>F32 then repacks to 32-bit in checked code). | `lib/ptx/header-test.f` | lib/ptx/cg.f | 2026-06-29 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| BITS>R | `n -- r` | PTX codegen f32->f64 readback: reinterpret a device-returned f32 bit pattern (widened by F32>F64) back into a Habu float - lets a GPU training loop read weights back and recompute gradients. | `lib/ptx/header-test.f` | lib/ptx/cg.f | 2026-06-29 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| MM-ABI | `-- matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n>` | GEMM launch-ABI mint: asserts the MM kernel entry's three cvta'd params (%rd1..%rd3, dims %r1..%r3 from MM-PARAMS) carry the related A[M,K], B[K,N], C[M,N] operands - the MK-SPAN analogue for the fixed GEMM ABI, and the only trusted word left in cg-matmul.f after stage 3 replaced the MM-STATE/MM-A/B/C-REG phase shim with the certified tile-pipe kernel body. | `lib/ptx/gemm-checked-test.f`, `lib/ptx/gemm-checked-neg-test.f`, `lib/ptx/tile-pipe-test.f` | lib/ptx/cg-matmul.f | 2026-07-16 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| Q-REG | `n -- matrix<space-global,f32,extent-q,extent-d>` | Attention codegen from-register cast for Q; the checked entry unifies its shape with K, V, and O. | `lib/ptx/attention-checked-test.f`, `lib/ptx/attention-checked-neg-test.f` | lib/ptx/cg-attention.f | 2026-07-12 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| K-REG | `n -- matrix<space-global,f32,extent-q,extent-d>` | Attention codegen from-register cast for K; distinct entry word keeps the ABI role explicit while sharing the checked `[Q,D]` relation. | `lib/ptx/attention-checked-test.f`, `lib/ptx/attention-checked-neg-test.f` | lib/ptx/cg-attention.f | 2026-07-12 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| V-REG | `n -- matrix<space-global,f32,extent-q,extent-d>` | Attention codegen from-register cast for V; the checked entry rejects a mismatched sequence or head dimension. | `lib/ptx/attention-checked-test.f`, `lib/ptx/attention-checked-neg-test.f` | lib/ptx/cg-attention.f | 2026-07-12 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| O-REG | `n -- matrix<space-global,f32,extent-q,extent-d>` | Attention codegen from-register cast for O; the checked entry ties output shape to Q, K, and V. | `lib/ptx/attention-checked-test.f`, `lib/ptx/attention-checked-neg-test.f` | lib/ptx/cg-attention.f | 2026-07-12 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| STATE | `matrix<space-global,f32,q,d> matrix<space-global,f32,q,d> matrix<space-global,f32,q,d> matrix<space-global,f32,q,d> -- attnctx<q,d,attn-stage-q> attnacc<f32,block-128,mask-live>` | Attention codegen token shim: consumes the four related matrices and creates the phase-indexed context and register-accumulator token. | `lib/ptx/attention-checked-test.f`, `lib/ptx/attention-checked-neg-test.f` | lib/ptx/cg-attention.f | 2026-07-12 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| STAGE-Q | `attnctx<q,d,attn-stage-q> attnacc<f32,b,m> -- attnctx<q,d,attn-stage-score> attnacc<f32,b,m>` | Target primitive for cooperative Q staging and its barrier; the nominal phase transition prevents score computation before staging. | `lib/ptx/attention-checked-test.f`, `lib/ptx/attention-checked-neg-test.f` | lib/ptx/cg-attention.f | 2026-07-12 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| SCORE | `attnctx<q,d,attn-stage-score> attnacc<f32,b,m> -- attnctx<q,d,attn-stage-softmax> attnacc<f32,b,m>` | Target primitive for the QK score reduction into shared memory; the accumulator and exact predecessor phase are preserved by the signature. | `lib/ptx/attention-checked-test.f`, `lib/ptx/attention-checked-neg-test.f` | lib/ptx/cg-attention.f | 2026-07-12 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| SOFTMAX | `attnctx<q,d,attn-stage-softmax> attnacc<f32,b,m> -- attnctx<q,d,attn-stage-output> attnacc<f32,b,m>` | Target primitive for stable in-place shared-memory softmax; it can only consume a completed score phase. | `lib/ptx/attention-checked-test.f`, `lib/ptx/attention-checked-neg-test.f` | lib/ptx/cg-attention.f | 2026-07-12 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| OUTPUT | `attnctx<q,d,attn-stage-output> attnacc<f32,b,m> -- attnctx<q,d,attn-stage-done> attnacc<f32,b,m>` | Target primitive for the PV reduction and global output store; FINISH accepts only its done-phase result. | `lib/ptx/attention-checked-test.f`, `lib/ptx/attention-checked-neg-test.f` | lib/ptx/cg-attention.f | 2026-07-12 | stdlib-boundary | habu-ptx-phantom-preserving-3df9db92 |
| CRH | `-- ptr u8` | Crash-handler header buffer is raw dictionary storage copied into signal-safe write output. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| BFR-BYTE@ | `ptr u8 n -- u8` | Refresh prelude byte reader over dictionary name bytes; raw record pointers are refined before this checked scanner can read them. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-29 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| SHK-BYTE+ | `ptr u8 n -- ptr u8` | Refines treeshaker byte-pointer arithmetic for token scanning; the raw `+` is the typed pointer-offset boundary. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| ZBYTE@ | `ptr u8 n -- u8` | Reads one byte from argv/envp C strings through byte-offset pointer arithmetic. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-29 | builder-emit | habu-raw-self-path-4514ffd3 |
| ZBYTE! | `u8 ptr u8 n --` | Writes one byte into target temp-path scratch through byte-offset pointer arithmetic. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-29 | builder-emit | habu-raw-self-path-4514ffd3 |
| ZPTR+ | `ptr u8 n -- ptr u8` | Refines argv/envp C-string byte-pointer arithmetic after the `NAME=` prefix. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-29 | builder-emit | habu-raw-self-path-4514ffd3 |
| TMP-PATH-COPY-SRC | `ptr u8 n --` | Copies a script path suffix into the fixed target temp-path scratch using raw byte offsets. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-29 | builder-emit | habu-raw-self-path-4514ffd3 |
| ENGINE-SELF-MACOS | `-- n` | Resolves the running engine's own executable path from the macOS `apple[]` `executable_path` entry (contiguous after envp on the startup stack); the NUL-terminated pointer walk and NULL tests are outside checker inference. | `lib/engine-id-test.f`, `test/run.f` | lib/engine-id.f | 2026-07-04 | stdlib-boundary | habu-raw-self-path-4514ffd3 |
| ENGINE-SELF-LINUX | `-- n` | Resolves the running engine's own executable path via `readlink("/proc/self/exe")` into a raw byte buffer; the raw path-buffer pointer view is outside checker inference. | `lib/engine-id-test.f`, `test/run.f` | lib/engine-id.f | 2026-07-04 | stdlib-boundary | habu-raw-self-path-4514ffd3 |
| RAW>NODE | `n -- CAD-KIND:node-id` | Private Model IR refinement after the allocator or node-range validator proves the raw table position names a committed or newly allocated node. | `maki/model-ir-test.f` | maki/model-ir.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| NODE>RAW | `CAD-KIND:node-id -- n` | Private representation projection used only before Model IR bounds validation or indexing of the owner table; no public raw cast is exported. | `maki/model-ir-test.f` | maki/model-ir.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>SLOT | `n -- MIR:input-slot` | Private Model IR refinement after the slot allocator or slot-range validator proves the raw position names a live input slot. | `maki/model-ir-test.f` | maki/model-ir.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| SLOT>RAW | `MIR:input-slot -- n` | Private representation projection used only by Model IR slot validators, owner-table accessors, and canonical rendering. | `maki/model-ir-test.f` | maki/model-ir.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>REF | `n -- MIR:operand-ref` | Private operand-reference refinement after node or slot identity validation; the signed wire encoding remains owned by Model IR. | `maki/model-ir-test.f` | maki/model-ir.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| REF>RAW | `MIR:operand-ref -- n` | Private representation projection used by the signed-reference validator and renderer; callers cannot erase the public operand role. | `maki/model-ir-test.f` | maki/model-ir.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>INPUT-INDEX | `n -- MIR:input-index` | Private input-ordinal refinement after signed and global-capacity validation; each accessor rechecks the node-local operand count. | `maki/model-ir-test.f` | maki/model-ir.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| INPUT-INDEX>RAW | `MIR:input-index -- n` | Private input-ordinal projection used only after the node handle is validated and before the node-local bound check. | `maki/model-ir-test.f` | maki/model-ir.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>REF-POS | `n -- MIR:ref-pos` | Private flat-reference-table position refinement after signed and capacity validation. | `maki/model-ir-test.f` | maki/model-ir.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| REF-POS>RAW | `MIR:ref-pos -- n` | Private flat-reference-table projection used only by bounded owner-table load/store helpers. | `maki/model-ir-test.f` | maki/model-ir.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>ANODE | `n -- ADAG:node-id` | Private async-DAG node refinement after the allocator or node-range validator proves the raw table position names a committed DAG node. | `maki/async-dag-test.f` | maki/async-dag.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| ANODE>RAW | `ADAG:node-id -- n` | Private async-DAG node projection used only before bounds revalidation, owner-table indexing, and the render boundary; no public raw cast is exported. | `maki/async-dag-test.f` | maki/async-dag.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>ASTREAM | `n -- ADAG:stream-id` | Private async-DAG stream refinement after the stream allocator or stream-range validator proves the raw position names a live stream. | `maki/async-dag-test.f` | maki/async-dag.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| ASTREAM>RAW | `ADAG:stream-id -- n` | Private async-DAG stream projection used only by bounds revalidation, the cross-stream dependency guard, and the render boundary. | `maki/async-dag-test.f` | maki/async-dag.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>AEVENT | `n -- ADAG:event-id` | Private async-DAG event refinement after the event allocator or event-range validator proves the raw position names a created event. | `maki/async-dag-test.f` | maki/async-dag.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| AEVENT>RAW | `ADAG:event-id -- n` | Private async-DAG event projection used only by bounds revalidation and the liveness/record owner-table accessors. | `maki/async-dag-test.f` | maki/async-dag.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>DECL | `n -- decl` | Private R7 stage mint: seeds a declared Model IR object; the only public entry is `MODEL:DECLARE`, so a raw n cannot forge a stage. | `maki/typestate-test.f` | maki/typestate.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>ELAB | `n -- elab` | Private R7 stage mint: the elaborated-model witness, minted only by `MODEL:ELABORATE` from a `MODEL:decl`. | `maki/typestate-test.f` | maki/typestate.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>SOLVED | `n -- solved` | Private R7 stage mint: the type/shape-solved witness, minted only by `TIR:SOLVE` from a `MODEL:elab`. | `maki/typestate-test.f` | maki/typestate.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>LEGAL | `n -- legal` | Private R7 stage mint: the region-legalized witness, minted only by `RIR:LEGALIZE` from a `TIR:solved`. | `maki/typestate-test.f` | maki/typestate.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>DRAFT | `n -- draft` | Private R7 stage mint: seeds an incomplete plan (`PLAN:DRAFT`); a draft cannot enter lowering until `PLAN:FINISH` promotes it. | `maki/typestate-test.f` | maki/typestate.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>COMPLETE | `n -- complete` | Private R7 stage mint: the completed-plan witness, minted only by `PLAN:FINISH` from a `RIR:legal` + `PLAN:draft`. | `maki/typestate-test.f` | maki/typestate.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>DRAFTED | `n -- drafted` | Private R7 stage mint: seeds a drafted kernel IR (`KIR:DRAFT`); unverified, so it cannot enter target emission. | `maki/typestate-test.f` | maki/typestate.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>VERIFIED | `n -- verified` | Private R7 stage mint: the verified-kernel witness, minted only by `KIR:VERIFY` from a `PLAN:complete` + `KIR:drafted`. | `maki/typestate-test.f` | maki/typestate.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>EMITTED | `n -- emitted` | Private R7 stage mint: the emitted-candidate witness, minted only by `CAND:EMIT` from a `KIR:verified` + `CAD-KIND:target-id`. | `maki/typestate-test.f` | maki/typestate.f | 2026-07-13 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-BUILD-PROOF | `-- build-proof` | Private R7 stage mint: the class-private build-proof token that seals the `ART:built` product, minted only by `ART:BUILD` from a `CAND:emitted` + the `CAD-KIND:artifact-id` it is built from; so a caller holding an artifact id cannot forge the "was actually built" witness (identity threading, dot habu-public-producers-for-7084d81c; replaces the retired fieldless `RAW>BUILT`). | `maki/typestate-test.f` | maki/typestate.f | 2026-07-14 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-CERTIFY-PROOF | `-- certify-proof` | Private R7 evidence mint: the class-private certify proof token, minted only by `EVID:CERTIFY`; its existence downstream of a real gate is the proof, so a raw n cannot forge `EVID:certified`. | `maki/evidence/schema-test.f` | maki/evidence/schema.f | 2026-07-14 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-GOLDEN-PROOF | `-- golden-proof` | Private R7 evidence mint: the class-private golden proof token, minted only by `EVID:GOLDEN`; leg/precision provenance rides the `EVID:golden` product fields. | `maki/evidence/schema-test.f` | maki/evidence/schema.f | 2026-07-14 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-GRADCHECK-PROOF | `-- gradcheck-proof` | Private R7 evidence mint: the class-private gradcheck proof token, minted only by `EVID:GRADCHECK`. | `maki/evidence/schema-test.f` | maki/evidence/schema.f | 2026-07-14 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-PROFILE-PROOF | `-- profile-proof` | Private R7 evidence mint: the class-private profile proof token, minted only by `EVID:PROFILE`. | `maki/evidence/schema-test.f` | maki/evidence/schema.f | 2026-07-14 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-GRANT-PROOF | `-- grant-proof` | Private R7 promotion mint: the sealed grant token, minted only by `POLICY:CHECK` after the value-level artifact binding holds, so a raw n cannot forge `POLICY:granted`. | `maki/evidence/policy-test.f` | maki/evidence/policy.f | 2026-07-14 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>SCHEMA-ID | `n -- CAD-KIND:schema-id` | Private schema-id registry refinement after canonical-name validation, capacity validation, and append-only slot allocation (the maki/artifact.f `RAW>ARTIFACT-ID` pattern); the only public producer is `SCHEMA:REGISTER`, which interns the version-independent schema name, so a raw n cannot forge a schema identity (MODEL-CAD-V2-PLAN.md § 23.9 foreign-id contract; dot habu-schema-schema-id-3a6827e9, retiring the former maki/evidence/policy.f placeholder). | `maki/schema-test.f` | maki/schema.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| SCHEMA-ID>RAW | `CAD-KIND:schema-id -- n` | Private schema identity projection used only by bounds validation, name-table access, `SCHEMA:EQUAL?`, and the `SCHEMA:ID>WIRE` / `WIRE>ID` codec; no public raw conversion is exported. | `maki/schema-test.f` | maki/schema.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>PROMOTED | `n -- promoted` | Private R7 stage mint: the promoted-artifact witness, minted only by `ART:PROMOTE` from an `ART:built` + a sealed `POLICY:granted`, so a raw n cannot forge a promoted artifact and no caller can fabricate one around the sealed promotion transition. | `maki/evidence/promote-test.f` | maki/evidence/promote.f | 2026-07-14 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>RGN | `n -- CAD-KIND:region` | Private fusion-planner region refinement after `FP-CK` and the region-range validator prove the raw table position names a planned region; also mints the region identity stored into the sealed typed `FP-RID` column (R3 owner-module rule; landed by the closed dot `habu-maki-apply-cad-27b7a7d7`, owned by `habu-epic-model-cad-70b629a9`; storage sealed by `habu-nominal-storage-migrate-47ee0f93`). | `maki/fusion-plan-test.f` | maki/fusion-plan.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RGN>RAW | `CAD-KIND:region -- n` | Private region projection used only by fusion-plan bounds revalidation, the `FP-RID` typed-cell raw-index projection that keys the parallel per-region fact arrays, and the `REGION_<rid>` render boundaries; no public raw cast is exported (landed by the closed dot `habu-maki-apply-cad-27b7a7d7`, owned by `habu-epic-model-cad-70b629a9`; storage sealed by `habu-nominal-storage-migrate-47ee0f93`). | `maki/fusion-plan-test.f` | maki/fusion-plan.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>TARGET-ID | `n -- CAD-KIND:target-id` | Private target-registry refinement after semantic descriptor validation, capacity validation, and append-only slot allocation. | `maki/target/target-test.f` | maki/target/target.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| TARGET-ID>RAW | `CAD-KIND:target-id -- n` | Private target identity projection used only by bounds validation and owner-table access; no public raw conversion is exported. | `maki/target/target-test.f` | maki/target/target.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>ARTIFACT-ID | `n -- CAD-KIND:artifact-id` | Private artifact-registry refinement after key validation and append-only slot allocation (interned by the section-7.4 store key); the only public producer is `ARTIFACT:REGISTER`, so a raw n cannot forge an artifact id (the maki/target/target.f `RAW>TARGET-ID` pattern; dot habu-public-producers-for-7084d81c). | `maki/artifact-test.f` | maki/artifact.f | 2026-07-14 | prim-axiom | habu-epic-model-cad-70b629a9 |
| ARTIFACT-ID>RAW | `CAD-KIND:artifact-id -- n` | Private artifact identity projection used only by bounds validation, key-table access, and `ARTIFACT:EQUAL?`; no public raw conversion is exported (retiring maki/evidence/policy.f's former `AID>RAW`). | `maki/artifact-test.f` | maki/artifact.f | 2026-07-14 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>NUMERIC-POLICY-ID | `n -- CAD-KIND:numeric-policy-id` | Private numeric-policy identity refinement (the maki/target/target.f `RAW>TARGET-ID` pattern); the raw IS the proof-domain rank (0..3), so a policy is content-addressed by its single `dom`. The only public producer is `NPOL:REGISTER`, bound to a real dom value, so a raw n cannot forge a numeric-policy id (MODEL-CAD-V2-PLAN.md § 23.9 foreign-id contract; dot habu-npol-numeric-policy-a90657e1). | `maki/numpolicy-test.f` | maki/numpolicy.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| NUMERIC-POLICY-ID>RAW | `CAD-KIND:numeric-policy-id -- n` | Private numeric-policy identity projection used only by bounds validation and the `NPOL:ID>WIRE` / `WIRE>ID` codec; no public raw conversion is exported. | `maki/numpolicy-test.f` | maki/numpolicy.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>PRODUCER-ID | `n -- CAD-KIND:producer-id` | Private producer-id registry refinement after canonical-name validation, capacity validation, and append-only slot allocation (the maki/artifact.f `RAW>ARTIFACT-ID` pattern); the only public producer is `PRODUCER:REGISTER`, which interns the version-independent producer name, so a raw n cannot forge a producer identity (MODEL-CAD-V2-PLAN.md § 23.9 foreign-id contract; dot habu-producer-producer-id-5e016e1f). | `maki/producer-test.f` | maki/producer.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| PRODUCER-ID>RAW | `CAD-KIND:producer-id -- n` | Private producer identity projection used only by bounds validation, name-table access, `PRODUCER:EQUAL?`, and the `PRODUCER:ID>WIRE` / `WIRE>ID` codec; no public raw conversion is exported. | `maki/producer-test.f` | maki/producer.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>CONFIG-ID | `n -- CAD-KIND:config-id` | Private config-id registry refinement after canonical-fact-string validation, capacity validation, and append-only slot allocation (the maki/artifact.f `RAW>ARTIFACT-ID` pattern); the only public producer is `CONFIG:REGISTER`, which interns the build/config facts remaining after target facts (target-id) and numeric facts (numeric-policy-id), so a raw n cannot forge a config identity (MODEL-CAD-V2-PLAN.md § 23.9 foreign-id contract; dot habu-config-config-id-06aa21bd). | `maki/config-test.f` | maki/config.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| CONFIG-ID>RAW | `CAD-KIND:config-id -- n` | Private config identity projection used only by bounds validation, fact-table access, `CONFIG:EQUAL?`, and the `CONFIG:ID>WIRE` / `WIRE>ID` codec; no public raw conversion is exported. | `maki/config-test.f` | maki/config.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>AUDIT-EVENT-ID | `n -- CAD-KIND:audit-event-id` | Private audit-event-id journal refinement after descriptor validation, capacity validation, and monotonic append-sequence assignment (the maki/artifact.f `RAW>ARTIFACT-ID` pattern); the only public producer is `JOURNAL:APPEND`, which mints the next occurrence-identified sequence at append time, so a raw n cannot forge an audit-event identity (MODEL-CAD-V2-PLAN.md § 23.9 foreign-id contract; dot habu-v2-txn-journal-d0bc644f). | `maki/journal-test.f` | maki/journal.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| AUDIT-EVENT-ID>RAW | `CAD-KIND:audit-event-id -- n` | Private audit-event identity projection used only by bounds validation, descriptor-table access, `JOURNAL:SEQ`/`EQUAL?`, and the `JOURNAL:ID>WIRE` / `WIRE>ID` codec; no public raw conversion is exported. | `maki/journal-test.f` | maki/journal.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>REV-ID | `n -- CAD-KIND:rev-id` | Private rev-id registry refinement after canonical-revision-content validation, capacity validation, and append-only slot allocation (the maki/artifact.f `RAW>ARTIFACT-ID` pattern); the only public producer is `REV:COMMIT`, which content-addresses the canonical revision content (parent + write set), so a raw n cannot forge a revision identity (MODEL-CAD-V2-PLAN.md § 23.9 foreign-id contract; dot habu-v2-txn-journal-d0bc644f). | `maki/rev-test.f` | maki/rev.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| REV-ID>RAW | `CAD-KIND:rev-id -- n` | Private revision identity projection used only by bounds validation, content-table access, `REV:EQUAL?`, and the `REV:ID>WIRE` / `WIRE>ID` codec; no public raw conversion is exported. | `maki/rev-test.f` | maki/rev.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>OBLIGATION-ID | `n -- CAD-KIND:obligation-id` | Private obligation-id registry refinement after canonical-encoding content-key computation and content-addressed dedup / append (the maki/producer.f `RAW>PRODUCER-ID` pattern); the only public producer is `OBLIG:INTERN`, which content-addresses an obligation by its canonical ENCODE bytes, so a raw n cannot forge an obligation identity (MODEL-CAD-V2-PLAN.md § 23.9 foreign-id contract; dot habu-v2-evidence-applicability-73ac58b9). | `maki/db/obligation-test.f` | maki/db/obligation.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| OBLIGATION-ID>RAW | `CAD-KIND:obligation-id -- n` | Private obligation identity projection used only by bounds validation, content-key access, `OBLIG:ID-EQUAL?`, and the `OBLIG:ID>WIRE` / `WIRE>ID` / `KEY>WIRE` / `WIRE>KEY` codecs; no public raw conversion is exported. | `maki/db/obligation-test.f` | maki/db/obligation.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>EVIDENCE-ID | `n -- CAD-KIND:evidence-id` | Private evidence-id registry refinement after canonical-descriptor validation, capacity validation, and append-only slot allocation (the maki/producer.f `RAW>PRODUCER-ID` pattern); the only public producer is `EVIDENCE:REGISTER`, which interns the canonical evidence descriptor, so a raw n cannot forge an evidence identity (MODEL-CAD-V2-PLAN.md § 23.9 foreign-id contract; dot habu-v2-evidence-applicability-73ac58b9). | `maki/db/evidence-test.f` | maki/db/evidence.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| EVIDENCE-ID>RAW | `CAD-KIND:evidence-id -- n` | Private evidence identity projection used only by bounds validation, descriptor-table access, `EVIDENCE:EQUAL?`, and the `EVIDENCE:ID>WIRE` / `WIRE>ID` / `KEY>WIRE` / `WIRE>KEY` codecs; no public raw conversion is exported. | `maki/db/evidence-test.f` | maki/db/evidence.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>ACTION-ID | `n -- CAD-KIND:action-id` | Private action-id registry refinement after canonical-name validation, capacity validation, and append-only slot allocation (the maki/producer.f `RAW>PRODUCER-ID` pattern); the only public producer is `ACTION:REGISTER`, which interns the canonical action name after a completeness check, so a raw n cannot forge an action identity (MODEL-CAD-V2-PLAN.md § 23.9 machine-facing action registry; dot habu-v2-machine-action-a7357409). | `maki/db/action-test.f` | maki/db/action.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| ACTION-ID>RAW | `CAD-KIND:action-id -- n` | Private action identity projection used only by bounds validation, name-table access, `ACTION:EQUAL?`, `DISPATCH` resolution, and the `ENUM-AT` / `DIGEST` canonical enumeration; no public raw conversion is exported. | `maki/db/action-test.f` | maki/db/action.f | 2026-07-17 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>RUN-ID | `n -- CAD-KIND:run-id` | Private run-id registry refinement after run-key completeness validation, capacity validation, and content-addressed intern of the canonical-run-key digest (the maki/artifact.f `RAW>ARTIFACT-ID` pattern); the only public producer is `RUN:SEAL`, which interns the SHA-256 digest of the canonical run key, so a raw n cannot forge a run identity (MODEL-CAD-V2-PLAN.md § 23.4 experiment registry; dot habu-v2-experiment-run-7c1d1906). | `maki/experiment/run-test.f` | maki/experiment/run.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RUN-ID>RAW | `CAD-KIND:run-id -- n` | Private run identity projection used only by bounds validation, content-key-table access, `RUN:EQUAL?`, and the `RUN:KEY>WIRE` / `WIRE>KEY` codec and `BATCH-ID`; no public raw conversion is exported. | `maki/experiment/run-test.f` | maki/experiment/run.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>SUITE-ID | `n -- CAD-KIND:suite-id` | Private suite-id registry refinement after capacity validation and content-addressed intern of the DifferentialSuite digest (the maki/db/evidence.f `RAW>EVIDENCE-ID` pattern); the only public producer is `SUITEID:REGISTER`, which interns a sealed suite's `DIFFSUITE:DIGEST-INTO` content key, so a raw n cannot forge a suite identity (MODEL-CAD-V2-PLAN.md § 23.9 foreign-id contract; dot habu-v2-differential-runner-13359019). | `maki/db/diff-suite-id-test.f` | maki/db/diff-suite-id.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| SUITE-ID>RAW | `CAD-KIND:suite-id -- n` | Private suite identity projection used only by bounds validation, content-key-table access, `SUITEID:EQUAL?`, and the `SUITEID:KEY>WIRE` / `WIRE>KEY` codec; no public raw conversion is exported. | `maki/db/diff-suite-id-test.f` | maki/db/diff-suite-id.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| RAW>GRANT | `n -- CAPTOK:grant` | Private capability-grant refinement over an append-only authority-slot allocation (the maki/db/action.f `RAW>ACTION-ID` pattern; a package-local `CAPTOK:grant` nominal, not a CAD-KIND identity); the only public producers are `CAPTOK:ROOT` (the trusted authority-origin mint) and `CAPTOK:ATTENUATE` (a subset-checked child derivation), so a raw n cannot forge a capability grant and no nested attenuation can exceed its parent's authority (dot habu-v2-capability-and-0970a96d). | `maki/db/capability-test.f` | maki/db/capability.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| GRANT>RAW | `CAPTOK:grant -- n` | Private grant identity projection used only by bounds validation and pooled authority-slot access (`CAP-MASK@` / `BUDGET@` / `AUTHORIZES?` / `ATTENUATE`); no public raw conversion is exported. | `maki/db/capability-test.f` | maki/db/capability.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-CAND-PROOF | `-- cand-proof` | Private promotion-typestate mint: the class-private Candidate proof token, minted only by `PROMOTE:CANDIDATE`; its existence downstream of the seed is the seal, so a raw n cannot forge a `PROMOTE:candidate` (dot habu-v2-evidence-promotion-f8312ebe). | `maki/db/promotion-test.f` | maki/db/promotion.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-VER-PROOF | `-- ver-proof` | Private promotion-typestate mint: the Verified proof token, minted only by `PROMOTE:VERIFY` AFTER the obligation is APPLICABLE (compose `APPLIC:VERDICT`), so a Verified is unconstructible without applicable evidence and a raw n cannot forge one (dot habu-v2-evidence-promotion-f8312ebe). | `maki/db/promotion-test.f` | maki/db/promotion.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-MEAS-PROOF | `-- meas-proof` | Private promotion-typestate mint: the Measured proof token, minted only by `PROMOTE:MEASURE` after the measurement obligation is APPLICABLE, so a raw n cannot forge a `PROMOTE:measured` (dot habu-v2-evidence-promotion-f8312ebe). | `maki/db/promotion-test.f` | maki/db/promotion.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-SAT-PROOF | `-- sat-proof` | Private promotion-typestate mint: the PolicySatisfied proof token, minted only by `PROMOTE:SATISFY` after the policy binds the candidate model, is unexpired, and its digest is bound, so a raw n cannot forge a `PROMOTE:satisfied` (dot habu-v2-evidence-promotion-f8312ebe). | `maki/db/promotion-test.f` | maki/db/promotion.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-PROM-PROOF | `-- prom-proof` | Private promotion-typestate mint: the Promoted proof token, minted only by `PROMOTE:PROMOTE` after the obligation closure is journaled, so a raw n cannot forge a `PROMOTE:promoted` (dot habu-v2-evidence-promotion-f8312ebe). | `maki/db/promotion-test.f` | maki/db/promotion.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-AUTH-PROOF | `-- auth-proof` | Private discharge-authority mint: the sealed authority token, minted only by `DAUTH:SEAL`, so a forged authority cannot silently widen who may discharge an obligation (the `CAPTOK:grant` discipline; dot habu-v2-evidence-promotion-f8312ebe). | `maki/db/promotion-authority-test.f` | maki/db/promotion-authority.f | 2026-07-18 | prim-axiom | habu-epic-model-cad-70b629a9 |
| DIM-REFINE | `n -- CAD-KIND:dim` | Private validated nominal representation boundary for tensor dimensions; tracked by `habu-v2-r3-type-9f89d1e9`. | `maki/tensor-test.f` | maki/tensor.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| DIM-RAW | `CAD-KIND:dim -- n` | Private dimension projection used only by checked shape algebra and numeric execution boundaries. | `maki/tensor-test.f` | maki/tensor.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| ROWS-REFINE | `n -- CAD-KIND:rows` | Private validated row-role refinement; public construction goes through `SHAPE`. | `maki/tensor-test.f` | maki/tensor.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| ROWS-RAW | `CAD-KIND:rows -- n` | Private row projection used by checked shape algebra and numeric execution boundaries. | `maki/tensor-test.f` | maki/tensor.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| COLS-REFINE | `n -- CAD-KIND:cols` | Private validated column-role refinement; public construction goes through `SHAPE`. | `maki/tensor-test.f` | maki/tensor.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| COLS-RAW | `CAD-KIND:cols -- n` | Private column projection used by checked shape algebra and numeric execution boundaries. | `maki/tensor-test.f` | maki/tensor.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| SPACE-REFINE | `n -- CAD-KIND:address-space` | Private validated address-space refinement behind named constructors and `ADDRESS-SPACE-DECODE`. | `maki/tensor-test.f` | maki/tensor.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| SPACE-RAW | `CAD-KIND:address-space -- n` | Private address-space projection used by equality and ABI boundaries. | `maki/tensor-test.f` | maki/tensor.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| IMP-ROWS-N | `CAD-KIND:rows -- n` | Private ONNX-importer raw-extent projection: the importer decodes and indexes wire integers against validated slot/node extents (Model-CAD V2 R3 owner-module refinement rule); no public raw conversion is exported. | `maki/onnx/import-test.f` | maki/onnx/import.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| IMP-COLS-N | `CAD-KIND:cols -- n` | Private ONNX-importer raw-extent projection for column extents (same owner-module boundary as `IMP-ROWS-N`). | `maki/onnx/import-test.f` | maki/onnx/import.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| TYPED-LINEAR | `ptr a ptr a ptr a ptr a CAD-KIND:rows CAD-KIND:cols CAD-KIND:cols --` | Private adapter from nominal tensor descriptors to the legacy native `LINEAR` ABI; the typed caller validates all roles before this boundary. | `maki/tensor-value-test.f`, `maki/plan-compose-test.f` | maki/tensor-value.f | 2026-07-12 | stdlib-boundary | habu-epic-model-cad-70b629a9 |
| RAW>TENSOR | `n -- tensor` | Private tensor-handle refinement after generation and slot packing; no raw constructor is public. | `maki/tensor-value-test.f` | maki/tensor-value.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| TENSOR>RAW | `tensor -- n` | Private tensor-handle projection used only for generation/slot validation and nominal equality. | `maki/tensor-value-test.f` | maki/tensor-value.f | 2026-07-12 | prim-axiom | habu-epic-model-cad-70b629a9 |
| EQ-EXEC | `eq-slot --` | Narrow library-level raw-`execute` shim for the SPEC: einsum equation registry (docs/model-unified.md stage 1): it fetches a generated `<NAME>-RUN` execution token from the slot-indexed `EQ-XT-A` cell array and executes it to run one equation kernel. The token's provenance is a word this file generated and captured, but `execute` of a fetched cell effect is not checker-expressible until the checker can model a typed xt-cell array. First consumer to migrate and retire the boundary when that capability lands. | `maki/spec-test.f` | maki/spec.f | 2026-07-19 | stdlib-boundary | habu-typed-xt-cell-4c8ecc4c |
| MINT-BYTE-LEN | `n -- byte-len` | Private CAD-NUM B5.1 representation mint: no-op cast of a validated nonnegative cell to the nominal `byte-len` role; the only producer is `CAD-NUM:BYTE-LEN`. Confined to its owner by refine-lint; retire the unchecked boundary when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) closes raw-value laundering. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-ITEM-COUNT | `n -- item-count` | Private CAD-NUM B5.1 representation mint: no-op cast of a validated nonnegative cell to the nominal `item-count` role; the only producer is `CAD-NUM:ITEM-COUNT`. Confined by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-CELL-COUNT | `n -- cell-count` | Private CAD-NUM B5.1 representation mint: no-op cast of a validated nonnegative cell to the nominal `cell-count` role; the only producer is `CAD-NUM:CELL-COUNT`. Confined by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-INDEX | `n -- index` | Private CAD-NUM B5.1 representation mint: no-op cast of a validated nonnegative cell to the nominal `index` role; the only producer is `CAD-NUM:INDEX`. Confined by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-BYTE-OFF | `n -- byte-off` | Private CAD-NUM B5.1 representation mint: no-op cast of a validated nonnegative cell to the nominal `byte-off` role; the only producer is `CAD-NUM:BYTE-OFF`. Confined by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-CELL-OFF | `n -- cell-off` | Private CAD-NUM B5.1 representation mint: no-op cast of a validated nonnegative cell to the nominal `cell-off` role; the only producer is `CAD-NUM:CELL-OFF`. Confined by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-ALIGNMENT | `n -- alignment` | Private CAD-NUM B5.1 representation mint: no-op cast of a validated positive power-of-two cell to the nominal `alignment` role; the only producer is `CAD-NUM:ALIGNMENT`. Confined by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-POSITIVE-DIVISOR | `n -- positive-divisor` | Private CAD-NUM B5.1 representation mint: no-op cast of a validated positive cell to the nominal `positive-divisor` role; the only producer is `CAD-NUM:POSITIVE-DIVISOR`. Confined by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-ALLOC-BYTE-LEN | `n -- alloc-byte-len` | Private CAD-NUM B5.1 representation mint: no-op cast of a positive byte extent to the nominal `alloc-byte-len` role; the only producer is `CAD-NUM:AS-ALLOC-BYTE-LEN`. Confined by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-ALLOC-CELL-COUNT | `n -- alloc-cell-count` | Private CAD-NUM B5.1 representation mint: no-op cast of a positive, non-overflowing cell count to the nominal `alloc-cell-count` role; the only producer is `CAD-NUM:AS-ALLOC-CELL-COUNT`. Confined by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| BYTE-LEN>N | `byte-len -- n` | Private CAD-NUM proof-erasure projection: reads a `byte-len`'s raw cell for the `AS-ALLOC-BYTE-LEN` zero test, where no primitive consumes the role directly; no public inverse. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| CELL-COUNT>N | `cell-count -- n` | Private CAD-NUM proof-erasure projection: reads a `cell-count`'s raw cell for the `AS-ALLOC-CELL-COUNT` zero/overflow test, where no primitive consumes the role directly; no public inverse. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-types-test.f` | lib/cad-num-types.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| ITEM-COUNT>N | `item-count -- n` | Private CAD-NUM B5.2 proof-erasure projection: reads an `item-count`'s raw cell so the B5.2 arithmetic kernels can add/subtract/multiply/divide it; no primitive consumes the role directly and there is no public inverse. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-arithmetic-test.f` | lib/cad-num-arithmetic.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| INDEX>N | `index -- n` | Private CAD-NUM B5.2 proof-erasure projection: reads an `index`'s raw cell for the checked advance/retreat/distance kernels; no primitive consumes the role directly and there is no public inverse. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-arithmetic-test.f` | lib/cad-num-arithmetic.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| ITEM-COUNT>N | `CAD-NUM:item-count -- n` | Private VEC (B5.5) proof-erasure projection: reads a validated `item-count`'s raw cell to store the vector capacity/length header cell and to size the one-cell-per-item allocation, where the raw vector header and cell-address arithmetic still consume a bare `n`. VEC-private, no public export; count and index roles cannot swap. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lets the header/address primitives take the nominal role directly. | `lib/vector-test.f` | lib/vector.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| INDEX>N | `CAD-NUM:index -- n` | Private VEC (B5.5) proof-erasure projection: reads a validated `index`'s raw cell for the checked cell-address arithmetic behind VEC:@ / VEC:! / VEC:EACH, where the address primitive still consumes a bare `n`. VEC-private, no public export; count and index roles cannot swap. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/vector-test.f` | lib/vector.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| BYTE-LEN>N | `CAD-NUM:byte-len -- n` | Private STR (B5.5) proof-erasure projection: reads a validated `byte-len`'s raw cell to drive the raw byte-scan words (FIND-SUB / INDEX-OF / SPLIT-NEXT / BUF-APPEND) and their byte pointers, which still consume a bare `n`. STR-private, no public export; length and offset roles cannot swap. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lets the byte-scan primitives take the nominal role directly. | `lib/string-test.f` | lib/string.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| BYTE-OFF>N | `CAD-NUM:byte-off -- n` | Private STR (B5.5) proof-erasure projection: reads a validated `byte-off`'s raw cell for the STR:SPLIT-NEXT start offset passed to the raw scan, where the scan still consumes a bare `n`. STR-private, no public export; length and offset roles cannot swap. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/string-test.f` | lib/string.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| BYTE-OFF>N | `byte-off -- n` | Private CAD-NUM B5.2 proof-erasure projection: reads a `byte-off`'s raw cell for the checked advance/retreat/distance/align kernels; no primitive consumes the role directly and there is no public inverse. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-arithmetic-test.f` | lib/cad-num-arithmetic.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| CELL-OFF>N | `cell-off -- n` | Private CAD-NUM B5.2 proof-erasure projection: reads a `cell-off`'s raw cell for the checked advance/retreat/distance/widen kernels; no primitive consumes the role directly and there is no public inverse. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-arithmetic-test.f` | lib/cad-num-arithmetic.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| ALIGNMENT>N | `alignment -- n` | Private CAD-NUM B5.2 proof-erasure projection: reads an `alignment`'s raw power-of-two cell for the align-up/aligned? kernels; no primitive consumes the role directly and there is no public inverse. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-arithmetic-test.f` | lib/cad-num-arithmetic.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| POSITIVE-DIVISOR>N | `positive-divisor -- n` | Private CAD-NUM B5.2 proof-erasure projection: reads a `positive-divisor`'s raw cell for the total DIV/REM kernels, where the positive role makes division-by-zero statically impossible; no primitive consumes the role directly and there is no public inverse. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/cad-num-arithmetic-test.f` | lib/cad-num-arithmetic.f | 2026-07-15 | prim-axiom | habu-epic-model-cad-70b629a9 |
| MINT-PATH | `n -- path` | Private NOM representation mint: no-op cast of a validated path-node index to the opaque nominal `path` handle; producers are `NOM:ROOT`/`NOM:CONS`. Confined to path.f by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) closes raw-value laundering. | `lib/nominal/nominal-test.f` | lib/nominal/path.f | 2026-07-15 | prim-axiom | habu-epic-type-system-b88c9ecc |
| PATH-IDX | `path -- n` | Private NOM proof-erasure projection: reads a `path` handle's node index for bounds checks and canonical walks; no public inverse. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/nominal/nominal-test.f` | lib/nominal/path.f | 2026-07-15 | prim-axiom | habu-epic-type-system-b88c9ecc |
| MINT-BINDING | `n -- binding` | Private NOM representation mint: no-op cast of a validated binding-pool index to the opaque nominal `binding` handle; the only producer is `NOM:BIND`. Confined to binding.f by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/nominal/nominal-prop-test.f` | lib/nominal/binding.f | 2026-07-15 | prim-axiom | habu-epic-type-system-b88c9ecc |
| BIND-IDX | `binding -- n` | Private NOM proof-erasure projection: reads a `binding` handle's pool index for bounds checks and chunk assembly; no public inverse. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/nominal/nominal-prop-test.f` | lib/nominal/binding.f | 2026-07-15 | prim-axiom | habu-epic-type-system-b88c9ecc |
| MINT-ROW | `n -- row` | Private NOM representation mint: no-op cast of a published record index to the opaque nominal `row` handle; producers are `NOM:FREEZE`/`NOM:UNION`/`NOM:REMAP`/`NOM:DECODE` via row.f's PUBLISH-CHUNK/ROW-BY-IDX. Confined to row.f by refine-lint; retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/nominal/nominal-test.f` | lib/nominal/row.f | 2026-07-15 | prim-axiom | habu-epic-type-system-b88c9ecc |
| ROW-IDX | `row -- n` | Private NOM proof-erasure projection: reads a `row` handle's record index for bounds checks, interning, and canonical access; no public inverse. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/nominal/nominal-test.f` | lib/nominal/row.f | 2026-07-15 | prim-axiom | habu-epic-type-system-b88c9ecc |
| MK-BUILDER | `-- nom-builder` | Private NOM linear-token mint: forges the noncopyable transactional builder token; the only producer is `NOM:NEW`/`NOM:ADD` inside builder.f. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/nominal/nominal-test.f` | lib/nominal/builder.f | 2026-07-15 | prim-axiom | habu-epic-type-system-b88c9ecc |
| BUILDER-DROP | `nom-builder --` | Private NOM linear-token consume: the audited boundary that retires a `nom-builder` on the ADD/FREEZE/ROLLBACK paths where the checker cannot express the linear discard. Retire when TVK-RAW (`habu-nominal-storage-raw-a3430ef2`) lands. | `lib/nominal/nominal-test.f` | lib/nominal/builder.f | 2026-07-15 | prim-axiom | habu-epic-type-system-b88c9ecc |
| CAP-COMPILE-RUN | `--` | Model-CAD capture boundary evaluates the generated checked model definition and invokes its dynamic-arity capture word after the active checker hook certifies the definition. | `maki/cad-test.f`, `maki/test.f` | maki/cad.f | 2026-07-12 | test-metaprog | habu-primitive-effect-axiom-1119f176 |
| CHECK-PASSES? | `ptr u8 n -- bool` | Evaluation harness temporarily suppresses diagnostics, invokes the checker on candidate source, and restores the diagnostic hook; raw checker state mutation is the metaprogramming boundary. | `maki/eval/eval-test.f`, `maki/test.f` | maki/eval/eval.f | 2026-07-12 | test-metaprog | habu-primitive-effect-axiom-1119f176 |
| IX>N | `ix<e> -- n` | Runtime identity cast projecting an extent-typed index value back to a generic cell; the checker cannot infer nominal parametric-family erasure from an empty body (the src/core/roles.f role-cast pattern extended to the parametric `ix<extent>` family). | `maki/extent-test.f`, `maki/test.f` | maki/extent.f | 2026-07-18 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| XG-EVAL | `--` | Audited `evaluate` wrapper for EXTENT:/TENSOR:/ITENSOR: - compiles the generated extent constant, injector, and accessor definitions so the active checker hook certifies each; `evaluate` cannot be checker-typed (the src/core/roles.f DTC-EVAL / maki/cad.f CAP-COMPILE-RUN pattern). | `maki/extent-test.f`, `maki/extent-tensor-test.f`, `maki/test.f` | maki/extent.f | 2026-07-18 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| >RED | `ix<e> -- redx<e>` | BTC-7 contraction-entry cast marking an extent-typed index as a summation (reduction) axis; the checker cannot infer the nominal parametric retype from an empty body (the src/core/roles.f role-cast pattern, like IX>N). It cannot launder a free factor into a contraction: the free-vs-inner legality is the checker's SIG-END-PARAM rule (redx over a free extent or a whole product rejects at load). | `maki/extent-test.f`, `maki/test.f` | maki/extent.f | 2026-07-20 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| JIT-EVALUATE | `ptr u8 n --` | JIT inspection CLI evaluates user-supplied source before resolving and disassembling the requested word; dynamic evaluation cannot be expressed by the checker. | `test/gate-debug.f` | tools/jitdump-core.f | 2026-07-12 | builder-emit | habu-builder-trust-rows-c5d41af6 |
| CHECK! | `ptr u8 n -- n` | Shared lint prelude models the engine checker entrypoint so its fail-closed hook can compile checked before lint sources load. | `tools/trust-lint-test.f`, `test/run.f` | tools/lint/text.f | 2026-07-12 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| CGR-EVALUATE | `ptr u8 n --` | Code-role transformer evaluates normalized generated definitions only through its checked driver and converts failures into the tool's explicit evaluation error. | `tools/codegen-role-test.f`, `test/run.f` | tools/codegen-role.f | 2026-07-12 | test-metaprog | habu-primitive-effect-axiom-1119f176 |
| CGR-CHECK! | `ptr u8 n -- n` | Code-role transformer invokes the engine checker on extracted definitions before any dynamic compilation. | `tools/codegen-role-test.f`, `test/run.f` | tools/codegen-role.f | 2026-07-12 | test-metaprog | habu-primitive-effect-axiom-1119f176 |
| CGR-EVALUATE-UNCHECKED | `ptr u8 n --` | Post-certification compiler briefly disables the hook because the preceding checker pass already published the certified definition record; the hook is restored immediately afterward. | `tools/codegen-role-test.f`, `test/run.f` | tools/codegen-role.f | 2026-07-12 | test-metaprog | cap:checker-hook-identity |
| CGR-HOOK | `ptr u8 n -- n` | Code-role transformer fail-closed checker hook rejects every verdict except certification. | `tools/codegen-role-test.f`, `test/run.f` | tools/codegen-role.f | 2026-07-12 |  |  |
| CGR-HOOK! | `--` | Code-role transformer rearms the canonical compile preflight before reinstalling its fail-closed hook after the audited post-certification compile window. | `tools/codegen-role-test.f`, `test/run.f` | tools/codegen-role.f | 2026-07-17 | test-metaprog | cap:checker-hook-identity |
| CPR-EVAL | `ptr u8 n -- n` | Bootstrap recovery fixture evaluates one controlled definition under `catch` so it can prove a missing compile preflight returns through the language exception path. | `tools/bootstrap.sh` | test/compile-preflight-recovery.f | 2026-07-17 | test-metaprog | habu-primitive-effect-axiom-1119f176 |
| CPR-HOOK | `ptr u8 n -- n` | Bootstrap recovery fixture installs a test-only certifying hook while deliberately leaving compile preflight unarmed; dynamic hook installation is not expressible in checked source. | `tools/bootstrap.sh` | test/compile-preflight-recovery.f | 2026-07-17 | test-metaprog | cap:checker-hook-identity |
| CHECK! | `ptr u8 n -- n` | Check driver models the engine checker entrypoint so its fail-closed source hook compiles checked. | `tools/check-test.f`, `test/run.f` | tools/check-core.f | 2026-07-12 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| TYPE-RESERVED? | `ptr u8 n -- bool` | Check driver models the checker-owned reserved-type predicate used while validating generated source dependencies. | `tools/check-test.f`, `test/run.f` | tools/check-core.f | 2026-07-12 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| CHECKER-DEFLINEAR | `ptr u8 n --` | Check driver models the checker primitive that publishes parsed linearity metadata in the child validation scope. | `tools/check-test.f`, `test/run.f` | tools/check-core.f | 2026-07-12 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| CHECKER-DEFRECORD | `ptr u8 n ptr u8 n --` | Check driver models the checker primitive that publishes one parsed record definition and its source descriptor. | `tools/check-test.f`, `test/run.f` | tools/check-core.f | 2026-07-12 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| CHECKER-SCOPE-START | `--` | Check driver opens the checker transaction that isolates generated dependency effects from the parent session. | `tools/check-test.f`, `test/run.f` | tools/check-core.f | 2026-07-12 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| CHECKER-SCOPE-DONE | `--` | Check driver closes the checker transaction and rolls back generated dependency effects after the checked child verdict. | `tools/check-test.f`, `test/run.f` | tools/check-core.f | 2026-07-12 | prim-axiom | habu-primitive-effect-axiom-1119f176 |
| EVAL | `--` | Forked subject runner dynamically evaluates generated checked test source; the checker cannot express `evaluate`. Confined to the COW child and tracked by the missing typed dynamic-evaluation capability dot `habu-type-isolated-dynamic-244c0e2c`. | `lib/test/subject-test.f`, `test/wide-store-seal.f`, `test/protection-span.f`, `test/lower-txn-protection.f`, `test/top-row-hook-test.f` | lib/test/subject.f | 2026-07-17 | test-metaprog | habu-batch-candidate-valid-517bfb6f |
| STACK-ARM | `--` | Forked subject runner installs its private stack as S0 and clears inherited catch and TTY recovery cells before dynamic evaluation; raw engine recovery-cell roles are outside checker inference. Confined to the COW child and tracked by `habu-type-isolated-dynamic-244c0e2c`. | `lib/test/subject-test.f` | lib/test/subject.f | 2026-07-17 | test-metaprog | habu-batch-candidate-valid-517bfb6f |
| N>HANDLE | `n -- process-pty-handle` | Mint the linear live-handle nominal from a packed slot/generation cell; the checker cannot refine a raw cell into a use-once linear handle. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| HANDLE>N | `process-pty-handle -- n` | Erase the linear live-handle nominal to its packed slot/generation cell for slot arithmetic; role erasure is outside checker inference. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| N>RESERVATION | `n -- process-pty-reservation` | Mint the linear reservation nominal from a packed slot/generation cell before commit; linear refinement is not expressible in an empty checked body. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| RESERVATION>N | `process-pty-reservation -- n` | Erase the linear reservation nominal to its packed slot/generation cell; role erasure is outside checker inference. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| N>TEARDOWN | `n -- process-pty-teardown` | Mint the linear teardown nominal from a packed slot/generation cell after take; linear refinement is not expressible in checked source. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| TEARDOWN>N | `process-pty-teardown -- n` | Erase the linear teardown nominal to its packed slot/generation cell; role erasure is outside checker inference. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| PID>SUP | `pid -- sup-pid` | Refine a process id into the distinct supervisor-pid role so the registry cannot store it where a group or target pid is required. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| SUP>PID | `sup-pid -- pid` | Erase the supervisor-pid role back to a plain process id for syscalls; role erasure is outside checker inference. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| PID>PGRP | `pid -- pgrp` | Refine a process id into the distinct process-group-leader role so it cannot be confused with a supervisor or target pid. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| PGRP>PID | `pgrp -- pid` | Erase the process-group-leader role back to a plain process id; role erasure is outside checker inference. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| PID>TARGET | `pid -- target-pid` | Refine a process id into the distinct target-child role so it cannot be confused with a supervisor or group pid. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| TARGET>PID | `target-pid -- pid` | Erase the target-child role back to a plain process id; role erasure is outside checker inference. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| FD>GROUP-WATCH | `fd -- group-watch` | Refine a file descriptor into the distinct group-watch role so the registry cannot swap it for another watch descriptor. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| GROUP-WATCH>FD | `group-watch -- fd` | Erase the group-watch role back to a plain file descriptor; role erasure is outside checker inference. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| FD>TARGET-WATCH | `fd -- target-watch` | Refine a file descriptor into the distinct target-watch role so it cannot be confused with another watch descriptor. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| TARGET-WATCH>FD | `target-watch -- fd` | Erase the target-watch role back to a plain file descriptor; role erasure is outside checker inference. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| FD>SUP-WATCH | `fd -- sup-watch` | Refine a file descriptor into the distinct supervisor-watch role so it cannot be confused with another watch descriptor. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |
| SUP-WATCH>FD | `sup-watch -- fd` | Erase the supervisor-watch role back to a plain file descriptor; role erasure is outside checker inference. | `lib/process-pty-handle-test.f` | lib/process-pty-handle.f | 2026-07-19 | stdlib-boundary | habu-recover-checked-pty-04fcb611 |

## Ratchet baseline

`tools/trusted-inventory.f` inventories every trust site in checked-in `.f`/`.fs`
sources (skipping `.jj-ws/` and `.dots/`): `TRUSTED:` definitions, `s" name"
s" effect" TRUST rows, `0 set-check` boundaries, `HOOK-INSTALL` — every
`' NAME set-check` / `['] NAME set-check` hook install, named by the installed
hook so a rogue install is visible in the TSV and ratcheted like any other
kind — and `TRUST-BARE`, the fail-closed catch-all for any other `TRUST` or
`set-check` use: a bare `TRUST` call with computed strings (the one known site
is `TRUST-SIGNATURE` in `src/habu/verify-source.f`, which grants trust from
scanned signature text) or a `set-check` whose argument is neither literal `0`
nor a ticked name. The only excluded `set-check` shape is a name reference
(`' set-check` / `['] set-check`), which takes the xt without executing it.
Hook identity is statically policed: `tools/checked-boundary-lint.f`
(`UB-HANDLE-INSTALL`) validates every `' NAME set-check` / `['] NAME set-check`
install against the audited hook list (`HOOK`, `USER-HOOK`, `SNAP-CHECK-HOOK`,
`CHK-CHECK-HOOK`, `LINT-CHECK-HOOK`, `ES-VERDICT-HOOK`, `PROP-CHECK-HOOK`) and
rejects any other installed name with an `E-UNAUDITED-HOOK` finding, so
`' EVIL-HOOK set-check` now fails that lint. Each `HOOK-INSTALL` site is covered
by a `file:name` classification row (below), so the derived ratchet counts every
install site individually; identity policing is the lint-level guard against
rogue names at those sites.
Sites are detected through the shared source lexer, so comment and string
mentions never count.

The ratchet has no separate hand-edited count block; that shared line conflicted
on every parallel merge. Its ceiling is *derived* from the classification block
below: a `file:name` row covers exactly one site (override with an explicit
trailing count when a name appears at more than one trust site), and a
`file class dot N` file-level row carries its own site count `N`. Verify it with
`bin/hb --load tools/trusted-inventory.f -- baseline TRUSTED.md`: the ratchet
fails if any scanned site is uncovered (a new trust site added without an audited
row), if a file-level or multi-site row's committed count grew or shrank against
the live tree, if a file-level row is missing its count, or if a mapping key is
duplicated. Adding an audited trust site is a new `file:name` row (a distinct,
mergeable line) with no shared count to bump, so parallel branches that each add
one site merge with zero baseline edits. Discharging a covered site lowers that
row's count in the same change; the ratchet stays fail-closed both ways.

### Build-time-generated trust (explicit exemption)

The inventory counts checked-in sources only. The build emitters MAY generate
trust sites as string literals into *generated* stage2/fixpoint source, which
the lexer correctly skips in the emitter itself. As of 2026-07-07 that
generated set is EMPTY: the former image-writer window (`0 set-check` span +
`' HOOK set-check` reinstall around the target-image emitters) and the five
synthetic TRUST rows (`ASM-CODE`, `BUILD-IMAGE`, `BUILD-SNAP-HDR`,
`SET-SIGID`, `CODESIG2`) were retired — src/os/{linux/elf.f,linux/sign.f,
macos/macho.f,macos/sign2.f} compile checked in stage2, with their effects
coming from the checked definitions themselves. The remaining generated
check-state transitions are the refresh prelude's `BFR-CHECK-OFF` call (a
hide.f TRUSTED word with its own row), `src/core/check-hook.f`'s own
`' HOOK set-check`, and the check-CLI runner prelude that
`tools/check-core.f` `CHK-BUILD-PREFIX` generates into every check child: a
`0 set-check` window followed by the `CHECK-F-HOOK` definition, canonical
`LOWER-CERT-HOOK:INSTALL` preflight rearm, and `' CHECK-F-HOOK set-check`
install, fail-closed via `70 throw`. That generated
install is lexer-invisible to hook-identity policing by design; its shape is
pinned by the `check/prelude-hook-shape` regression
(`tools/check-test-lib.f`), which rejects any other installed hook name or a
missing re-arm in the generated text.

Reintroducing generated trust requires updating the build-fixpoint
source-shape regressions (`tools/build-fixpoint-test.f` asserts stage2
contains NO bare `0 set-check` line and none of the retired TRUST rows),
which is the review point. One related edge: a TRUST row written with
escaped-string literals (`s\" name"`) is not the plain two-literal shape, so
the inventory counts it as `TRUST-BARE` rather than `TRUST` — never silently.

## Inventory classification

Every trust site carries a class and an owner in the inventory TSV, and this
block is also the single source of truth for the ratchet ceiling (there is no
separate count block). Each row is `file[:name] class owner [count]`: a bare `file`
row classifies every site in that file no named row owns and carries an explicit
site count `N`; a `file:name` row overrides the file row for the site(s) called
`name` and implies count 1 unless it carries an explicit count (a name that
appears at more than one trust site, e.g. a definition plus its install). Valid
classes: `builder-emit` (engine/image/build
emitters and raw layout boundaries), `stdlib-boundary` (library-level trusted
boundaries), `test-metaprog` (test-owned fixtures and metaprogramming
harnesses), `prim-axiom` (nominal identity casts and primitive models the
checker treats as axioms), `discharge-candidate` (sites believed checkable
today). Sites without a row report class `-` and count as unclassified. An owner
is either a live dot id or a declared permanent capability id. Live dot ids must
exist in `.dots/` as `<id>.md` or `<id>/<id>.md`. Permanent ids use canonical
lower-kebab `cap:<name>` syntax and must appear once, in sorted order, in the
registry below together with a repository-relative Markdown path and explicit
anchor. `strict` verifies the file and exact anchor, so a misspelled or deleted
owner document fails closed. Move rows from a dot to a permanent capability only
after the implementation and its owning tests are complete; open residual work
remains dot-owned. `bin/hb --load tools/trusted-inventory.f -- strict` fails on
every violation. `strict` also prints a `by-file` line per source
with its non-zero per-class site counts, so classification drift is visible per
file, not just as a repo total.
The block is being refined from file granularity to `file:name` row granularity
and reassigned from the `habu-audit-trusted-inventory-3a950436` placeholder to
each site's real capability/discharge owner: `src/core/roles.f` (all 34
nominal-cast axioms) and `test/prop-test-core.f` (test-metaprog fixtures) carry
per-site rows, and the whole `prim-axiom` class — the nominal-cast axioms plus
the engine-primitive TRUST rows in `src/core/cell-effects.f`,
`src/core/pointer-storage-effects.f`, `src/core/structures-effects.f`,
`tools/check-core.f`, and `src/core/include.f`
— is now owned by its real owner
`habu-primitive-effect-axiom-1119f176` (the audited axiom table).
`test/prop-test-core.f` keeps a file-level row because its `0 set-check`
boundaries have no nameable key. The self-contained `stdlib-boundary` files are
reassigned per-site: `lib/ffi-abi.f` (`P>N` pointer erasure plus sealed
dynamic-loader calls),
`lib/memory.f` (`MEM-ALLOC-PTR` mmap provenance mint), and `lib/task.f`
(`TASK-NULL`/`TASK-N>PTR`/`TASK-CELL>PTR-SLOT` mints plus the `TASK`/`+USER`/
`FACILITY` `create`/`does>` defining words) go to
`habu-typed-defining-words-aa224eb5`, which enumerates exactly those mints and
the typed-defining-word family; `lib/build.f` (`BUILD-CHECK-RAW` wrapping the
`CHECK!` engine entrypoint) goes to `habu-primitive-effect-axiom-1119f176`; and
`lib/ptx/cg-matmul.f`'s old `MM-A/B/C-REG` + `MM-STATE` kernel wrappers were
DELETED by `habu-re-express-tiled-9cc4a73a` (stage 3): `EMIT-MATMUL` now ships
the certified typed KERNEL body over the tile-pipe vocabulary, and the file's
one remaining trusted word is the launch-ABI mint `MM-ABI` (permanent
`from_raw_parts` class, owned by `habu-ptx-phantom-preserving-3df9db92` like
the other fixed-ABI minters). `lib/task.f` is mixed-class: its `TASK-PATCH`
code-emission wrapper carries a `file:name` row owned by
`habu-checker-capability-gate-14022ba9` (patch32 gated PRIM-TRUSTED-ONLY so
checked code cannot forge the seal; that dot is owner-of-record for the patch32
boundary, incl. `ES-PATCH32` still folded in `test/engine-suite.f`'s
test-metaprog file-level count), while the file-level row (reduced to the
remaining cast/mint/defining sites) carries the typed-defining owner. The four
remaining `lib/ptx` files (`cg.f`, `collective.f`, `tile.f`, `tile-v4.f`) and
`lib/engine-id.f` stay on the placeholder: the PTX files need per-site
mint-vs-phantom classification (`habu-ptx-phantom-preserving-3df9db92`'s
17-mint-vs-~70-wrapper split), and `engine-id.f`'s two sites
(`ENGINE-SELF-MACOS`/`ENGINE-SELF-LINUX`) are raw startup-image/syscall
self-path reads (apple[] pointer walk + NULL tests / `/proc/self/exe`
readlink) — the same boundary class as `src/os/env-base.f`'s startup-image
reads, which are themselves still on the placeholder; no existing capability
dot discharges these raw-syscall reads (`habu-checker-capability-ptr-113a95e9`
covers pointer-arithmetic byte-views, not the reads), so assigning them is a
separate increment, not a file-level guess. Reassigning the remaining
`builder-emit`, `test-metaprog`, `discharge-candidate`, and those
PTX/engine-id `stdlib-boundary` rows to their real owners is the rest of
`habu-audit-trusted-inventory-3a950436`.

Row granularity (2026-07-11): every SEPARABLE file-level fold — one whose
covered sites all carry nameable word names — is split into `file:name` rows
(same class, same owner: finer attribution only, ownership reassignment stays
per-increment). Two folds remain by design and are allowed by the
`fold-baseline 2` directive at the head of the block: `src/habu/habu2.f` and
`test/type-layout-lower-pending.f`, both contested under the wide-ADT stack
(splitting them now would go stale on that merge). The six `0 set-check`
boundary sites (no nameable key: `src/habu/build.f`, `hide.f`,
`maker.f`, `snap.f`, `test/engine-suite.f`, `tools/codegen-role.f`) keep
count-1 residual file rows, like `test/prop-test-core.f`. (`src/habu/aot-lib.f`
retired its window: its relocation core now compiles checked with the named
`MAP-IN-BLOB` boundary, dot habu-checked-image-writers-229ae789.) `strict` counts the
separable folds, prints `separable fold(s) N (baseline M)`, and fails when N
exceeds the committed baseline — so a new coarse row cannot creep in, and
splitting a remaining fold prompts lowering the baseline in the same change
(decrease-only, the PARSE-COUNT ratchet shape). A missing `fold-baseline` row
is itself a strict failure (fail-closed).

<a id="permanent-capability-owners"></a>
### Permanent capability owners

The machine-readable registry binds stable capability identity to its canonical
audit documentation. The anchors are explicit rather than inferred from heading
rendering, so the checker can validate them without a Markdown implementation.

| Owner | Completed semantic boundary | Owning evidence |
|---|---|---|
| <a id="cap-checker-hook-identity"></a>`cap:checker-hook-identity` | Audited hook-identity gate only: checked-boundary lint rejects installed names outside the landed canonical allowlist across engine, AOT, snapshot, lint, and tests. It does not claim post-seal compiler/friend-latch authorization; that residual remains owned by live dot `habu-seal-set-check-b3676b33`. | `test/engine-suite.f`, `test/prop-test-core.f`, `tools/codegen-role.f` |
| <a id="cap-checker-registry-whitebox"></a>`cap:checker-registry-whitebox` | Test-only leaves expose pre-hook checker registry layout metadata, marks, and individual unmodeled mutations so checked orchestration proves growth and exact rollback. | `test/engine-suite.f` |
| <a id="cap-fetched-adt-validation"></a>`cap:fetched-adt-validation` | Test-only hostile whitebox access constructs malformed fetched ADT layouts and proves growth, guard, product, and width-one rejection. | `test/layout-valid-growth.f`, `test/layout-valid-guard-base.f`, `test/layout-valid-product-bad.f`, `test/layout-valid-w1-bad.f` |
| <a id="cap-qualified-family-payloads"></a>`cap:qualified-family-payloads` | Test-only nominal payload casts construct and inspect qualified family values after schema resolution. | `test/type-decl-suite.f` |
| <a id="cap-sealed-family-pointers"></a>`cap:sealed-family-pointers` | Test-only representation accessor verifies that layout-buffer family pointers remain sealed behind the typed buffer API. | `test/layout-buffer.f` |
| <a id="cap-wide-memory-lowering"></a>`cap:wide-memory-lowering` | Test-only native bootstrap probes inspect wide PRODUCT representation and raw image memory after lowering. | `test/bootstrap-wide-memory-src.f`, `test/bootstrap-wide-memory.f` |

<!-- trusted-inventory-owners
cap:checker-hook-identity TRUSTED.md#cap-checker-hook-identity
cap:checker-registry-whitebox TRUSTED.md#cap-checker-registry-whitebox
cap:fetched-adt-validation TRUSTED.md#cap-fetched-adt-validation
cap:qualified-family-payloads TRUSTED.md#cap-qualified-family-payloads
cap:sealed-family-pointers TRUSTED.md#cap-sealed-family-pointers
cap:wide-memory-lowering TRUSTED.md#cap-wide-memory-lowering
-->

<!-- trusted-inventory-classes
fold-baseline 2
src/habu/build.f builder-emit habu-builder-trust-rows-c5d41af6 1
src/habu/habu2.f builder-emit habu-builder-trust-rows-c5d41af6 138
src/habu/hide.f builder-emit habu-builder-trust-rows-c5d41af6 1
src/habu/maker.f builder-emit habu-builder-trust-rows-c5d41af6 1
src/habu/verify-source.f:TRUST builder-emit habu-builder-trust-rows-c5d41af6
test/compile-preflight-recovery.f test-metaprog habu-seal-set-check-b3676b33 1
test/bootstrap-wide-memory-src.f:BWM-UN2 test-metaprog cap:wide-memory-lowering
test/bootstrap-wide-memory-src.f:BWM-UN4 test-metaprog cap:wide-memory-lowering
test/bootstrap-wide-memory-src.f:BWM-XT test-metaprog cap:wide-memory-lowering
test/bootstrap-wide-memory-src.f:BWM-W32 test-metaprog cap:wide-memory-lowering
test/bootstrap-wide-memory-src.f:BWM-DEF-A test-metaprog cap:wide-memory-lowering
test/bootstrap-wide-memory-src.f:BWM-DEF-B test-metaprog cap:wide-memory-lowering
test/bootstrap-wide-memory-src.f:BWM-CALL-DEF test-metaprog cap:wide-memory-lowering
test/layout-buffer.f:LB-UN test-metaprog cap:sealed-family-pointers
test/layout-buffer.f:N>LBTK test-metaprog habu-epic-type-system-b88c9ecc
test/layout-buffer.f:LBTK>N test-metaprog habu-epic-type-system-b88c9ecc
test/layout-defer.f:N>DTK test-metaprog habu-seal-set-check-b3676b33
test/layout-defer.f:DTK>N test-metaprog habu-seal-set-check-b3676b33
test/layout-defer.f:DTK-ADDR test-metaprog habu-seal-set-check-b3676b33
test/typed-storage-test.f:N>TSK test-metaprog habu-checker-seal-nominal-0b2eaece
test/typed-storage-test.f:TSK>N test-metaprog habu-checker-seal-nominal-0b2eaece
test/typed-storage-test.f:TSRES-UN test-metaprog habu-checker-seal-nominal-0b2eaece
test/typed-storage-test.f:RES-K-UN test-metaprog habu-checker-seal-nominal-0b2eaece
test/layout-valid-growth.f:NAME$ test-metaprog cap:fetched-adt-validation
test/layout-valid-growth.f:BUILD test-metaprog cap:fetched-adt-validation
test/layout-valid-guard-base.f:RAW test-metaprog cap:fetched-adt-validation
test/layout-valid-guard-base.f:SET test-metaprog cap:fetched-adt-validation
test/layout-valid-guard-base.f:LVG-TFAM-ACTIVE-PKG$ test-metaprog habu-seal-set-check-b3676b33
test/layout-valid-guard-base.f:LVG-TFAM-DECL test-metaprog habu-seal-set-check-b3676b33
test/layout-valid-guard-base.f:LVG-SCHEMA-APP test-metaprog habu-seal-set-check-b3676b33
test/layout-valid-guard-base.f:LVG-SCHEMA-CON test-metaprog habu-seal-set-check-b3676b33
test/layout-valid-guard-base.f:LVG-SCHEMA-ROOT+ test-metaprog habu-seal-set-check-b3676b33
test/layout-valid-guard-base.f:LVG-SUMV-ADD test-metaprog habu-seal-set-check-b3676b33
test/layout-valid-guard-base.f:LVG-TFAM-SLOTS! test-metaprog habu-seal-set-check-b3676b33
test/layout-valid-guard-base.f:LVG-TFAM-VAR-RANGE! test-metaprog habu-seal-set-check-b3676b33
test/lower-cert.f:LCT-MULTI-ERR-BEGIN test-metaprog habu-seal-set-check-b3676b33
test/lower-cert.f:LCT-MULTI-ERR-END test-metaprog habu-seal-set-check-b3676b33
test/layout-valid-product-bad.f:RAW test-metaprog cap:fetched-adt-validation
test/layout-valid-w1-bad.f:RAW test-metaprog cap:fetched-adt-validation
test/rigid-region-suite.f test-metaprog habu-add-bounded-host-b40b048f 1
test/rigid-region-suite.f:RR-BOXG test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-BOXM test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-BOXR test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-MK1 test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-OWN test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-SHARE test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-SHARE3 test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-SHM test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-U3R test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-UBOX test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-UEQ test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-UEQ3 test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-UONE test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-XEXT test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-XEXT3 test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-XGEN test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-XGEN3 test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-XRGN test-metaprog habu-add-bounded-host-b40b048f
test/rigid-region-suite.f:RR-XRGN3 test-metaprog habu-add-bounded-host-b40b048f
test/type-layout-lower-pending.f test-metaprog habu-interpret-wide-gate-1d70acf7 4
test/type-layout-lower-pending.f:TWX-TFAM-FIND-IN test-metaprog habu-seal-set-check-b3676b33
test/type-match-suite.f:FREE-MTOK test-metaprog habu-tfam-11b-open-ee9c72c6
test/engine-suite.f:T-CHECK-PASSES test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-RDF test-metaprog cap:checker-hook-identity 2
test/engine-suite.f:TR-SYM-LAYOUT-RAW test-metaprog cap:checker-registry-whitebox
test/engine-suite.f:TR-EFF-REC-LAYOUT-RAW test-metaprog cap:checker-registry-whitebox
test/engine-suite.f:TR-EFF-NODE-LAYOUT-RAW test-metaprog cap:checker-registry-whitebox
test/engine-suite.f:TR-PE-LAYOUT-RAW test-metaprog cap:checker-registry-whitebox
test/engine-suite.f:TR-DFER-LAYOUT-RAW test-metaprog cap:checker-registry-whitebox
test/engine-suite.f:TR-NORET-LAYOUT-RAW test-metaprog cap:checker-registry-whitebox
test/engine-suite.f:TR-CORE-MARKS@ test-metaprog cap:checker-registry-whitebox
test/engine-suite.f:TR-VREC-MARKS@ test-metaprog cap:checker-registry-whitebox
test/engine-suite.f:TR-SYM-ADD test-metaprog cap:checker-registry-whitebox
test/engine-suite.f:TR-USIG-ADD test-metaprog cap:checker-registry-whitebox
test/engine-suite.f:TR-NORET-ADD test-metaprog cap:checker-registry-whitebox
test/engine-suite.f:T-SCV test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-CTV test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-SCOPED-W test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-PRESO test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-V14 test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:a:b:c test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:a:b: test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:x: test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:::x test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:tq:tail test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-GROW-PAIR test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-PHASE-ID test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-ASM-CODE test-metaprog habu-builder-trust-rows-c5d41af6
test/engine-suite.f:T-BUILD-IMAGE test-metaprog habu-builder-trust-rows-c5d41af6
test/engine-suite.f:T-CODESIG2 test-metaprog habu-builder-trust-rows-c5d41af6
test/engine-suite.f:T-BUILD-SNAP-HDR test-metaprog habu-builder-trust-rows-c5d41af6
test/engine-suite.f:T-SNAP-EXTRA-PTR test-metaprog habu-builder-trust-rows-c5d41af6
test/engine-suite.f:T-SNAP-EXTRA-SIZE test-metaprog habu-builder-trust-rows-c5d41af6
test/engine-suite.f:T-PTX-LOAD test-metaprog habu-ptx-phantom-preserving-3df9db92
test/engine-suite.f:T-PTX-ADD test-metaprog habu-ptx-phantom-preserving-3df9db92
test/engine-suite.f:T-PTX-GRID test-metaprog habu-ptx-phantom-preserving-3df9db92
test/engine-suite.f:T-PTX-MLOAD test-metaprog habu-ptx-phantom-preserving-3df9db92
test/engine-suite.f:T-PTX-MADD test-metaprog habu-ptx-phantom-preserving-3df9db92
test/engine-suite.f:T-MK-SPAN test-metaprog habu-ptx-phantom-preserving-3df9db92
test/engine-suite.f:T-MK-SPAN= test-metaprog habu-ptx-phantom-preserving-3df9db92
test/engine-suite.f:T-PTX-SAME-EXTENT test-metaprog habu-ptx-phantom-preserving-3df9db92
test/engine-suite.f:T-BIG6-MK test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-SCQ-MK test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-NEED-I64 test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-NEED-U32 test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-NEED-U16 test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-NEED-U8 test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-GIVE-U16 test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-GIVE-U8 test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-GIVE-I64 test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T->NODE test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-NODE>N test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-NEED-NODE test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-MAKE-OWN test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-FREE-OWN test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:T-LINUX-DUP2-FD test-metaprog habu-builder-trust-rows-c5d41af6
test/engine-suite.f:T-LINUX-SPAWN test-metaprog habu-builder-trust-rows-c5d41af6
test/engine-suite.f:T-SPAWN-DUP2-ACTION test-metaprog habu-builder-trust-rows-c5d41af6
test/engine-suite.f:T-SPAWN-DARWIN-FINISH test-metaprog habu-builder-trust-rows-c5d41af6
test/engine-suite.f:ES-TI test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:TP test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:TPN2 test-metaprog habu-primitive-effect-axiom-1119f176
test/immediate-model-test.f:IMT-PASSES test-metaprog habu-primitive-effect-axiom-1119f176
test/engine-suite.f:ES-PATCH32 test-metaprog habu-checker-capability-gate-14022ba9
test/engine-suite.f:ES-FFI-CALL test-metaprog habu-checker-capability-gate-14022ba9
test/engine-suite.f:set-check test-metaprog cap:checker-hook-identity
test/engine-suite.f test-metaprog habu-seal-set-check-b3676b33 7
test/engine-suite.f:ES-REND-SIG$ test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:ES-JSON-DIAGS! test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TG-RESET test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TG-USIGS test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TG-POW2 test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TG-COPY test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TG-RESTORE-END test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TG-TV-RESET test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TG-ARENA-RESET test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TG-TVT test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TR-TRAIL-RESET test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TR-ME-BEGIN test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TR-ME-END test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TR-ME? test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TR-ME-ORIGIN! test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TR-SGBAD-ARITY? test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TR-SGBAD-BAREPTR? test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TR-SGBAD-SYNTAX? test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TR-SGBAD-UNKNOWN? test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TR-TFAM-REG test-metaprog habu-seal-set-check-b3676b33
test/engine-suite.f:TR-VREC-PERSIST test-metaprog habu-seal-set-check-b3676b33
test/type-export-suite.f:TWX-CAND-START test-metaprog habu-seal-set-check-b3676b33
test/type-export-suite.f:TWX-CAND-DONE test-metaprog habu-seal-set-check-b3676b33
test/type-export-suite.f:TWX-FIND-DEFER test-metaprog habu-seal-set-check-b3676b33
test/type-export-suite.f:TWX-FIND-USIG test-metaprog habu-seal-set-check-b3676b33
test/type-export-suite.f:TWX-USIG-ADD test-metaprog habu-seal-set-check-b3676b33
test/type-export-suite.f:TWX-CTL-FLAGS test-metaprog habu-seal-set-check-b3676b33
test/type-export-suite.f:TWX-NORET-ADD test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-CAND-START test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-CAND-DONE test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-FIND-DEFER test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-FIND-USIG test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-USIG-ADD test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-CTL-FLAGS test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-TFAM-RESET test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SCHEMA-RESET test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-TFAM-DECL test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SUMV-ADD test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-TFAM-FIND-IN test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SCHEMA-PARAM test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SCHEMA-CON test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SCHEMA-ROOT+ test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SCHEMA-ROOT@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SCHEMA-ROW test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SCHEMA-QUOT test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SCHEMA-TAG@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SCHEMA-A@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SCHEMA-B@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-SCHEMA-C@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-LAY-N@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-LAY-ADD test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-PF-BEGIN test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-PF-ADD test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-PF-COMMIT test-metaprog habu-seal-set-check-b3676b33
test/type-family-rollback-suite.f:TWX-PF-RAW@ test-metaprog habu-seal-set-check-b3676b33
test/decl-event-suite.f:TWX-TFAM-RESET test-metaprog habu-seal-set-check-b3676b33
test/decl-event-suite.f:TWX-SCHEMA-RESET test-metaprog habu-seal-set-check-b3676b33
test/decl-event-suite.f:TWX-TFAM-DECL test-metaprog habu-seal-set-check-b3676b33
test/decl-event-suite.f:TWX-SCHEMA-PARAM test-metaprog habu-seal-set-check-b3676b33
test/decl-event-suite.f:TWX-SCHEMA-ROOT+ test-metaprog habu-seal-set-check-b3676b33
test/decl-event-suite.f:TWX-CAND-START test-metaprog habu-seal-set-check-b3676b33
test/decl-event-suite.f:TWX-CAND-DONE test-metaprog habu-seal-set-check-b3676b33
test/structure-make-suite.f:TWX-TFAM-RESET test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-SCHEMA-RESET test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-TFAM-DECL test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-SUMV-ADD test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-SCHEMA-CON test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-SCHEMA-PTR test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-SCHEMA-APP test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-SCHEMA-PARAM test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-SCHEMA-ROOT+ test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-TFAM-SLOTS! test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-TFAM-SLOTS@ test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-TFAM-FLD-RANGE! test-metaprog habu-structure-generate-make-872a6e75
test/structure-make-suite.f:TWX-TFAM-VAR-RANGE! test-metaprog habu-structure-generate-make-872a6e75
test/type-ctor-suite.f:TWX-CHECKER-RECORD-SYM test-metaprog habu-seal-set-check-b3676b33
test/type-ctor-suite.f:TWX-FRESH test-metaprog habu-seal-set-check-b3676b33
test/type-ctor-suite.f:TWX-MULTI-ERR-BEGIN test-metaprog habu-seal-set-check-b3676b33
test/type-ctor-suite.f:TWX-MULTI-ERR-END test-metaprog habu-seal-set-check-b3676b33
test/type-ctor-suite.f:TWX-NEW test-metaprog habu-seal-set-check-b3676b33
test/type-ctor-suite.f:TWX-SUMV-CTOR-SYM@ test-metaprog habu-seal-set-check-b3676b33
test/type-ctor-suite.f:TWX-SUMV-PAYCELLS@ test-metaprog habu-seal-set-check-b3676b33
test/type-ctor-suite.f:TWX-SYMS test-metaprog habu-seal-set-check-b3676b33
test/type-ctor-suite.f:TWX-TFAM-FIND-IN test-metaprog habu-seal-set-check-b3676b33
test/type-ctor-suite.f:TWX-TFAM-VIS@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TQX<QS test-metaprog cap:qualified-family-payloads
test/type-decl-suite.f:TQX>QS test-metaprog cap:qualified-family-payloads
test/type-decl-suite.f:TWX-CAND-DONE test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-CAND-START test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-CHECKER-FIND-USIG test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-CON-OF test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-FRESH test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-HIDDEN-PARAM? test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-HIDDEN-SLOT@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-LAYOUT-PUSH-FIELDS test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-MK-CON test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-MK-HIDDEN test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-MK-PARAM test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-MK-ROW test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-MK-VAR test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-MULTI-ERR-BEGIN test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-MULTI-ERR-END test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-NEW test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-P>REST test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-P>TYPE test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-PAIR test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-PARAM-SCR+ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-PARAM>FAM test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-PARAM>HID test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-PUSH-LOGICAL test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-R-RES test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-SCHEMA-A@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-SCHEMA-APP? test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-SCHEMA-CON? test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-SCHEMA-PARAM? test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-SCHEMA-PTR? test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-SCHEMA-ROOT@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-SUMV-FAM@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-SUMV-PAYCELLS@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-SUMV-SCH-COUNT@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-SUMV-SCH-START@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-SUMV-TAG@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TAG test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TDECL-POLICY test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TDECL-THROW test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-CELL? test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-DECL test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-ENUM? test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-FIND-IN test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-FLD-COUNT@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-FLD-START@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-LAYOUT-POLICY@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-LAYOUT? test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-PKG$ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-PRODUCT? test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-SLOTS@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-SUM? test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-TFAM-VIS@ test-metaprog habu-seal-set-check-b3676b33
test/type-decl-suite.f:TWX-UNIFY test-metaprog habu-seal-set-check-b3676b33
test/deftype-suite.f:TWX-SNAP-PREP test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-CHECKER-SNAPSHOT-PREPARE test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-T-WIDTH test-metaprog habu-epic-type-system-b88c9ecc
test/type-family-suite.f:TWX-MK-NULLARY test-metaprog habu-epic-type-system-b88c9ecc
test/type-family-suite.f:TWX-MK-UNARY test-metaprog habu-epic-type-system-b88c9ecc
test/type-family-suite.f:TWX-FRESH test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-LAY-ADD test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-LAY-ALIGN@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-LAY-FAM@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-LAY-FIND test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-LAY-POLICY@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-LAY-SIZE@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-LAY-TAGW@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-PACKED-DESC test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-PACKED-NARROW test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-PF-BEGIN test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-PF-ADD test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-PF-COMMIT test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-PF-ROLLBACK test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-A@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-APP test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-APP? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-C@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-CON test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-CON? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-NEW test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-PARAM test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-PARAM? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-PTR test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-PTR? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-QUOT test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-QUOT-DIN@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-QUOT-DOUT@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-QUOT-HASR@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-QUOT-RIN@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-QUOT-ROUT@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-QUOT? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-ROW test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-ROW? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-ROW-START@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-ROW-COUNT@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-ROW-ELEM@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-ROW-OK? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-RESET test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-ROOT+ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-ROOT@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-SNAPSHOT-PERSIST test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SCHEMA-TAG@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SUMV-ADD test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SUMV-CTOR-PKG! test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SUMV-FAM@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SUMV-FIND test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SUMV-PAYCELLS@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-SUMV-TAG@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TF-CANON? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TF-CTOR-PKG$ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TF-HIDDEN? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TF-INTERN test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TF-OFF$ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-CELL? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-DECL test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-ENUM? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-FIND-IN test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-FIND-PUBLIC test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-FLD-COUNT@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-FLD-RANGE! test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-FLD-START@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-LAYOUT! test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-LAYOUT-POLICY@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-LAYOUT? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-PK! test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-PK@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-PKG$ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-PRODUCT? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-RESET test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-RESOLVE test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-SCHEMA-ROOT! test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-SCHEMA-ROOT@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-SLOTS! test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-SLOTS@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-SNAPSHOT-PERSIST test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-SPAN! test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-SPAN@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-SUM? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-TAGW! test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-TAGW@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-VAR-RANGE! test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFAM-VIS@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFL-CON-FAM? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFL-CON? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFL-CVAR? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFL-MATCH-FAM? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFL-VAR? test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-TFL-VPADS test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-PF-RAW@ test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-PF-RAW! test-metaprog habu-seal-set-check-b3676b33
test/type-family-suite.f:TWX-PF-CAP test-metaprog habu-seal-set-check-b3676b33
src/core/internal-mark.f stdlib-boundary habu-seal-set-check-b3676b33 1
src/core/checker.f:TRUST stdlib-boundary habu-seal-set-check-b3676b33
test/gate-common-lib.f:UEND test-metaprog habu-primitive-effect-axiom-1119f176
test/gate-common-lib.f:USIGS-RESTORE-END test-metaprog habu-primitive-effect-axiom-1119f176
test/gate-common-lib.f:UTERM! test-metaprog habu-primitive-effect-axiom-1119f176
test/gate-common-lib.f:JSON-DIAGS test-metaprog habu-primitive-effect-axiom-1119f176
test/gate-common-lib.f:GE-EVAL-SOURCE-ACT test-metaprog habu-primitive-effect-axiom-1119f176
test/gate-common-lib.f:GE-EVAL-SOURCE test-metaprog habu-primitive-effect-axiom-1119f176
test/prop-test-core.f test-metaprog habu-seal-set-check-b3676b33 2
test/effect-read-api-test.f test-metaprog habu-checker-self-typing-9ff8ba86 1
test/prop-test-core.f:PROP-INSTALL-HOOK test-metaprog cap:checker-hook-identity
test/prop-test-core.f:CLEAR-MEAS test-metaprog habu-typed-depth-introspection-18f0efda
test/prop-test-core.f:ERR@ test-metaprog habu-typed-depth-introspection-18f0efda
test/prop-test-core.f:MARK test-metaprog habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:FORGET test-metaprog habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:SMARK test-metaprog habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:SFORGET test-metaprog habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:CHK-MARK test-metaprog habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:CHK-FORGET test-metaprog habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:CHK-HOOK test-metaprog habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:CHK-COMPILE-CERT test-metaprog habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:CHK test-metaprog habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:RUN-MEAS test-metaprog habu-typed-depth-introspection-18f0efda
test/prop-test-core.f:REND-SIG$ test-metaprog habu-typed-depth-introspection-18f0efda
test/prop-test-core.f:CONFIRM-FR? test-metaprog habu-typed-depth-introspection-18f0efda
tools/codegen-role.f test-metaprog habu-seal-set-check-b3676b33 1
src/core/check-hook.f:HOOK stdlib-boundary cap:checker-hook-identity
src/habu/aot.f:USER-HOOK builder-emit cap:checker-hook-identity
src/habu/snap-lib.f:SNAP-CHECK-HOOK builder-emit cap:checker-hook-identity 2
tools/check-core.f:CHK-CHECK-HOOK stdlib-boundary cap:checker-hook-identity
tools/lint/text.f:LINT-CHECK-HOOK stdlib-boundary cap:checker-hook-identity
test/engine-suite.f:ES-VERDICT-HOOK test-metaprog cap:checker-hook-identity 2
test/prop-test-core.f:PROP-CHECK-HOOK test-metaprog cap:checker-hook-identity 2
tools/codegen-role.f:CGR-HOOK test-metaprog cap:checker-hook-identity 2
-->

## Primitive-effect inventory

`tools/primitive-effect-inventory.f` (package `PEINV`) ratchets the authoritative
primitive-effect axiom rows -- every `PRIM: name eff... PRIM;` and
`PPRIM: pkg name eff... PPRIM;` in the checker's PES table -- independently of the
`prim-axiom` trust-site class above. That class counts the checker's axiom-model
`TRUSTED` sites (nominal casts, the census/inventory readers); it does NOT count
the axiom rows themselves. This inventory does, so permanent trust owners and the
primitive rows they read stay distinct quantities.

Each row's identity is the canonical tuple
`<kind> <defining-package> <word-spelling> <flags> <normalized-effect-tokens>`
(kind `prim`|`pprim`; package `-` for a bare `PRIM:`; spelling and effect tokens
folded lowercase; flags `trusted-only` when `PRIM-TRUSTED-ONLY!` marks the row).
Identity never depends on a path, line, ordinal, or PES address, so
case/whitespace/comment-only source edits preserve it; an added, deleted,
duplicated, or reordered identity does not. `strict` additionally cross-checks the
parsed rows against the live `#PE` registry (package/name, arity, trusted-only)
row-for-row, proving the source parse is faithful to the in-image table.

The block below is the committed manifest: one canonical identity per row, in
manifest (load) order -- deliberately ordered, NOT sorted, so a pure reorder is
detectable and the exact row can be named. `baseline TRUSTED.md` fails closed on
any add/delete/duplicate/reorder; regenerate the block with
`bin/hb --load tools/primitive-effect-inventory.f -- manifest` as the explicit
migration when the axiom set legitimately changes. Identical axioms may repeat
legitimately (e.g. `path0`/`PATH0` -- the same case-insensitive symbol with an
identical effect is declared in two checker.f sections); the manifest records the
repeat and the ratchet enforces its exact multiplicity.

<!-- primitive-effect-inventory-manifest
prim - dup - pe-a pe-in pe-a pe-out pe-a pe-out
prim - drop - pe-a pe-in
prim - swap - pe-a pe-in pe-b pe-in pe-b pe-out pe-a pe-out
prim - over - pe-a pe-in pe-b pe-in pe-a pe-out pe-b pe-out pe-a pe-out
prim - nip - pe-a pe-in pe-b pe-in pe-b pe-out
prim - tuck - pe-a pe-in pe-b pe-in pe-b pe-out pe-a pe-out pe-b pe-out
prim - rot - pe-a pe-in pe-b pe-in pe-c pe-in pe-b pe-out pe-c pe-out pe-a pe-out
prim - -rot - pe-a pe-in pe-b pe-in pe-c pe-in pe-c pe-out pe-a pe-out pe-b pe-out
prim - 2dup - pe-a pe-in pe-b pe-in pe-a pe-out pe-b pe-out pe-a pe-out pe-b pe-out
prim - 2drop - pe-a pe-in pe-b pe-in
prim - 2swap - pe-a pe-in pe-b pe-in pe-c pe-in pe-d pe-in pe-c pe-out pe-d pe-out pe-a pe-out pe-b pe-out
prim - 2over - pe-a pe-in pe-b pe-in pe-c pe-in pe-d pe-in pe-a pe-out pe-b pe-out pe-c pe-out pe-d pe-out pe-a pe-out pe-b pe-out
prim - + - pe-n pe-in pe-n pe-in pe-n pe-out
prim - + - pe-ptr-a pe-in pe-n pe-in pe-ptr-a pe-out
prim - + - pe-n pe-in pe-ptr-a pe-in pe-ptr-a pe-out
prim - - - pe-n pe-in pe-n pe-in pe-n pe-out
prim - - - pe-ptr-a pe-in pe-n pe-in pe-ptr-a pe-out
prim - - - pe-ptr-a pe-in pe-ptr-a pe-in pe-n pe-out
prim - * - pe-n pe-in pe-n pe-in pe-n pe-out
prim - and - pe-n pe-in pe-n pe-in pe-n pe-out
prim - and - pe-f pe-in pe-f pe-in pe-f pe-out
prim - or - pe-n pe-in pe-n pe-in pe-n pe-out
prim - or - pe-f pe-in pe-f pe-in pe-f pe-out
prim - xor - pe-n pe-in pe-n pe-in pe-n pe-out
prim - xor - pe-f pe-in pe-f pe-in pe-f pe-out
prim - 1+ - pe-n pe-in pe-n pe-out
prim - 1+ - pe-ptr-a pe-in pe-ptr-a pe-out
prim - 1- - pe-n pe-in pe-n pe-out
prim - 1- - pe-ptr-a pe-in pe-ptr-a pe-out
prim - negate - pe-n pe-in pe-n pe-out
prim - invert - pe-n pe-in pe-n pe-out
prim - 0= - pe-a pe-in pe-f pe-out
prim - 0< - pe-n pe-in pe-f pe-out
prim - = - pe-n pe-in pe-n pe-in pe-f pe-out
prim - = - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
prim - < - pe-n pe-in pe-n pe-in pe-f pe-out
prim - < - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
prim - > - pe-n pe-in pe-n pe-in pe-f pe-out
prim - > - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
prim - <> - pe-n pe-in pe-n pe-in pe-f pe-out
prim - <> - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
prim - <= - pe-n pe-in pe-n pe-in pe-f pe-out
prim - <= - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
prim - >= - pe-n pe-in pe-n pe-in pe-f pe-out
prim - >= - pe-ptr-a pe-in pe-ptr-a pe-in pe-f pe-out
prim - / - pe-n pe-in pe-n pe-in pe-n pe-out
prim - mod - pe-n pe-in pe-n pe-in pe-n pe-out
prim - /mod - pe-n pe-in pe-n pe-in pe-n pe-out pe-n pe-out
prim - abs - pe-n pe-in pe-n pe-out
prim - min - pe-n pe-in pe-n pe-in pe-n pe-out
prim - max - pe-n pe-in pe-n pe-in pe-n pe-out
prim - lshift - pe-n pe-in pe-n pe-in pe-n pe-out
prim - rshift - pe-n pe-in pe-n pe-in pe-n pe-out
prim - cells - pe-n pe-in pe-n pe-out
prim - cell+ - pe-ptr-a pe-in pe-ptr-a pe-out
prim - cell+ - pe-n pe-in pe-n pe-out
prim - chars - pe-n pe-in pe-n pe-out
prim - char+ - pe-ptr-a pe-in pe-ptr-a pe-out
prim - char+ - pe-n pe-in pe-n pe-out
prim - @ - pe-ptr-a pe-in pe-a pe-out
prim - ! - pe-a pe-in pe-ptr-a pe-in
prim - ptr-field - pe-ptr-a pe-in pe-n pe-in pe-ptr-ptr-b pe-out
prim - +! - pe-n pe-in pe-ptr-n pe-in
prim - c@ - pe-ptr-u8 pe-in pe-u8 pe-out
prim - c! - pe-u8 pe-in pe-ptr-u8 pe-in
prim - atomic@ - pe-ptr-a pe-in pe-a pe-out
prim - atomic! - pe-a pe-in pe-ptr-a pe-in
prim - atomic-add - pe-n pe-in pe-ptr-n pe-in pe-n pe-out
prim - atomic-cas - pe-a pe-in pe-a pe-in pe-ptr-a pe-in pe-a pe-out
prim - fence - -
prim - run-in-stack - pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
prim - count - pe-ptr-u8 pe-in pe-ptr-u8 pe-out pe-n pe-out
prim - . - pe-n pe-in
prim - .s - -
prim - depth - pe-n pe-out
prim - here - pe-ptr-a-raw pe-out
prim - allot - pe-n pe-in
prim - , - pe-n pe-in
prim - c, - pe-n pe-in
prim - type - pe-ptr-u8 pe-in pe-n pe-in
prim - script-argc - pe-n pe-out
prim - script-argv$ - pe-n pe-in pe-ptr-u8 pe-out pe-n pe-out
prim - throw - pe-n pe-in
prim - die - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in
prim - open - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - read - pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
prim - ioctl - pe-n pe-in pe-n pe-in pe-ptr-a pe-in pe-n pe-out
prim - mmap - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - path0 - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-out
prim - open-rd - pe-ptr-u8 pe-in pe-n pe-out
prim - access - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
prim - unlink - pe-ptr-u8 pe-in pe-n pe-out
prim - rename - pe-ptr-u8 pe-in pe-ptr-u8 pe-in pe-n pe-out
prim - chmod - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
prim - symlink - pe-ptr-u8 pe-in pe-ptr-u8 pe-in pe-n pe-out
prim - readlink - pe-ptr-u8 pe-in pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
prim - mkdir - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
prim - rmdir - pe-ptr-u8 pe-in pe-n pe-out
prim - stat64 - pe-ptr-u8 pe-in pe-ptr-u8 pe-in pe-n pe-out
prim - lstat64 - pe-ptr-u8 pe-in pe-ptr-u8 pe-in pe-n pe-out
prim - getdirentries64 - pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-ptr-n pe-in pe-n pe-out
prim - pipe - pe-n pe-out pe-n pe-out pe-n pe-out
prim - dup2 - pe-n pe-in pe-n pe-in pe-n pe-out
prim - fcntl - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - poll - pe-ptr-a pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - kill - pe-n pe-in pe-n pe-in pe-n pe-out
prim - setpgid - pe-n pe-in pe-n pe-in pe-n pe-out
prim - spawn-io - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - spawn-argv-io - pe-ptr-u8 pe-in pe-ptr-a pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - spawn-argv-env-io - pe-ptr-u8 pe-in pe-ptr-a pe-in pe-ptr-a pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - spawn-argv-env-cwd-io - pe-ptr-u8 pe-in pe-ptr-a pe-in pe-ptr-a pe-in pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - fork - pe-n pe-out
prim - wait-rc - pe-n pe-in pe-n pe-out
prim - wait-status - pe-n pe-in pe-n pe-out
prim - patch32 trusted-only pe-n pe-in pe-n pe-in
prim - snap-rebase - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in
prim - write - pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
prim - close - pe-n pe-in
prim - close-rc - pe-n pe-in pe-n pe-out
prim - epoch-seconds - pe-n pe-out
prim - mono-ns - pe-n pe-out
prim - prof-on - pe-n pe-in
prim - prof-report - -
prim - rbase - pe-n pe-out
prim - cp@ - pe-n pe-out
prim - cp! - pe-n pe-in
prim - dbase@ - pe-n pe-out
prim - check@ - pe-n pe-out
prim - ndict@ - pe-n pe-out
prim - ndict! - pe-n pe-in
prim - seal-capture - -
prim - seal-friend - -
prim - drain-pretrust - -
prim - data-base - pe-ptr-a pe-out
prim - prot-wid-add - pe-n pe-in
prim - owner-wid-preflight? - pe-n pe-in pe-n pe-in pe-n pe-in pe-f pe-out
prim - owner-wid-public? - pe-n pe-in pe-f pe-out
prim - owner-wid-private? - pe-n pe-in pe-f pe-out
prim - owner-wid? - pe-n pe-in pe-f pe-out
prim - tfam-ctor-word? - pe-ptr-u8 pe-in pe-n pe-in pe-f pe-out
prim - wordlist - pe-n pe-out
prim - get-current - pe-n pe-out
prim - set-current - pe-n pe-in
prim - search-wl - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - parse-name - pe-ptr-u8 pe-out pe-n pe-out
prim - core-str= - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-f pe-out
prim - core-str=ci - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-f pe-out
prim - pathz - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in
prim - path0 - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-out
prim - rd32 - pe-ptr-u8 pe-in pe-n pe-out
prim - diag-file! - pe-ptr-u8 pe-in pe-n pe-in
prim - diag-origin! - pe-n pe-in pe-n pe-in pe-n pe-in
prim - diag-json! - pe-f pe-in
prim - diag-buffer! - pe-ptr-u8 pe-in pe-n pe-in
prim - diag-buffer-off - -
prim - diag-buffer$ - pe-ptr-u8 pe-out pe-n pe-out
prim - checker-scope-start - -
prim - checker-scope-done - -
prim - check-candidate! - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
prim - check - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
prim - check! - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
prim - checker-candidate-scope-start - -
prim - checker-candidate-scope-done - -
prim - checker-usigs-truncate-from - pe-ptr-u8 pe-in pe-n pe-in
prim - checker-usigs-truncate-from-raw - pe-ptr-u8 pe-in pe-n pe-in
prim - checker-undefine - pe-ptr-u8 pe-in pe-n pe-in
prim - checker-undefine-guard - pe-ptr-u8 pe-in pe-n pe-in
prim - checker-export - pe-ptr-u8 pe-in pe-n pe-in
prim - checker-package-active? - pe-f pe-out
prim - checker-deflinear - pe-ptr-u8 pe-in pe-n pe-in
prim - checker-defrecord - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
prim - checker-deffamily - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
prim - checker-defsum - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
prim - checker-defsum-noend - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
prim - checker-defenum - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
prim - checker-defproduct - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
prim - checker-layout-info - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out pe-n pe-out pe-f pe-out
prim - checker-storage-info - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out pe-f pe-out
prim - checker-deflayout-buffer - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
prim - checker-deftyped-buffer - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
prim - checker-deftyped-variable - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
prim - checker-lbuf-name-guard - pe-ptr-u8 pe-in pe-n pe-in
prim - checker-defined? - pe-ptr-u8 pe-in pe-n pe-in pe-f pe-out
prim - cast-pend! - pe-ptr-u8 pe-in pe-n pe-in
prim - trust - pe-ptr-u8 pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in
prim - ptx-barrier! - pe-ptr-u8 pe-in pe-n pe-in
prim - tfam-n@ - pe-n pe-out
prim - tfam-width@ - pe-n pe-in pe-n pe-out
prim - tfam-name$ - pe-n pe-in pe-ptr-u8 pe-out pe-n pe-out
prim - tfam-arity@ - pe-n pe-in pe-n pe-out
prim - tfam-kind@ - pe-n pe-in pe-n pe-out
prim - tfam-public? - pe-n pe-in pe-f pe-out
prim - tfam-derive-eq? - pe-n pe-in pe-f pe-out
prim - tfam-derive-hash? - pe-n pe-in pe-f pe-out
prim - tfam-var-start@ - pe-n pe-in pe-n pe-out
prim - tfam-var-count@ - pe-n pe-in pe-n pe-out
prim - sumv-name$ - pe-n pe-in pe-ptr-u8 pe-out pe-n pe-out
prim - sumv-ctor-pkg$ - pe-n pe-in pe-ptr-u8 pe-out pe-n pe-out
pprim type-field count - pe-n pe-out
pprim type-field no-variant - pe-n pe-out
pprim type-field find - pe-n pe-in pe-n pe-in pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out pe-f pe-out
pprim type-field each - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out pe-f pe-out
pprim type-field family@ - pe-n pe-in pe-n pe-out
pprim type-field variant@ - pe-n pe-in pe-n pe-out
pprim type-field name$ - pe-n pe-in pe-ptr-u8 pe-out pe-n pe-out
pprim type-field schema@ - pe-n pe-in pe-n pe-out
pprim type-field slot@ - pe-n pe-in pe-n pe-out
pprim type-field cells@ - pe-n pe-in pe-n pe-out
pprim type-field byte-off@ - pe-n pe-in pe-n pe-out
pprim type-field bytes@ - pe-n pe-in pe-n pe-out
pprim type-field align@ - pe-n pe-in pe-n pe-out
pprim type-field flags@ - pe-n pe-in pe-n pe-out
prim - wf-n@ - pe-n pe-out
prim - wf-off@ - pe-n pe-in pe-n pe-out
prim - wf-pos@ - pe-n pe-in pe-n pe-out
prim - wf-fam@ - pe-n pe-in pe-n pe-out
prim - wf-width@ - pe-n pe-in pe-n pe-out
prim - wf-term@ - pe-n pe-in pe-n pe-out
prim - wf-flags@ - pe-n pe-in pe-n pe-out
prim - wf-wide? - pe-f pe-out
prim - wf-needs-p2? - pe-f pe-out
prim - wf-w-at - pe-n pe-in pe-n pe-in pe-n pe-out
prim - wide-mark - -
prim - rec-wide-publish - -
prim - rec-min-in@ - pe-n pe-out
prim - locw-hw@ - pe-n pe-in pe-n pe-out
prim - locw-hw-n@ - pe-n pe-out
pprim lower-cert magic - pe-n pe-out
pprim lower-cert version - pe-n pe-out
pprim lower-cert header-cells - pe-n pe-out
pprim lower-cert magic-cell - pe-n pe-out
pprim lower-cert version-cell - pe-n pe-out
pprim lower-cert total-bytes-cell - pe-n pe-out
pprim lower-cert needs-cell - pe-n pe-out
pprim lower-cert wf-count-cell - pe-n pe-out
pprim lower-cert bind-count-cell - pe-n pe-out
pprim lower-cert fetch-count-cell - pe-n pe-out
pprim lower-cert fetch-data-cells-cell - pe-n pe-out
pprim lower-cert wf-cells - pe-n pe-out
pprim lower-cert fetch-cells - pe-n pe-out
pprim lower-cert check-cells - pe-n pe-out
pprim lower-cert guard-cells - pe-n pe-out
pprim lower-cert fetch-flag - pe-n pe-out
pprim lower-cert store-flag - pe-n pe-out
pprim lower-cert xpad-flag - pe-n pe-out
pprim lower-cert body-len-cell - pe-n pe-out
pprim lower-cert body-hash-cell - pe-n pe-out
pprim lower-cert fnv-offset - pe-n pe-out
pprim lower-cert fnv-prime - pe-n pe-out
pprim lower-cert cell-count - pe-n pe-out
pprim lower-cert cell@ - pe-n pe-in pe-n pe-out
pprim lower-cert bytes trusted-only pe-ptr-u8 pe-out pe-n pe-out
pprim lower-cert-hook hook - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
pprim checker-cert install - pe-n pe-in
pprim checker-cert produce - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-in
prim - p2-locseq-reset - -
prim - p2-carve-w - pe-n pe-in pe-n pe-out
prim - p2-live-w@ - pe-n pe-in pe-n pe-out
prim - p2-live-cum@ - pe-n pe-in pe-n pe-out
prim - sumv-n@ - pe-n pe-out
prim - tf-str-u@ - pe-n pe-out
prim - tf-pk-n@ - pe-n pe-out
prim - schema-n@ - pe-n pe-out
prim - schema-root-n@ - pe-n pe-out
prim - checker-defer - pe-ptr-u8 pe-in pe-n pe-in
prim - checker-package - pe-ptr-u8 pe-in pe-n pe-in
prim - checker-using - pe-ptr-u8 pe-in pe-n pe-in
prim - checker-public - -
prim - checker-private - -
prim - checker-end-package - -
prim - ffi-call trusted-only pe-ptr-a pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - ffi-call-n trusted-only pe-ptr-a pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - ffi-call-bounded trusted-only pe-ptr-a pe-in pe-ptr-b pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - ffi-call-abi-bounded trusted-only pe-ptr-a pe-in pe-ptr-b pe-in pe-ptr-c pe-in pe-ptr-d pe-in pe-ptr-e pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - ffi-call-abi-r-bounded trusted-only pe-ptr-a pe-in pe-ptr-b pe-in pe-ptr-c pe-in pe-ptr-d pe-in pe-ptr-e pe-in pe-n pe-in pe-n pe-in pe-r pe-out
prim - ffi-call-abi trusted-only pe-ptr-a pe-in pe-ptr-b pe-in pe-ptr-c pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-out
prim - ffi-call-abi-r trusted-only pe-ptr-a pe-in pe-ptr-b pe-in pe-ptr-c pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-r pe-out
prim - f+ - pe-r pe-in pe-r pe-in pe-r pe-out
prim - f- - pe-r pe-in pe-r pe-in pe-r pe-out
prim - f* - pe-r pe-in pe-r pe-in pe-r pe-out
prim - f/ - pe-r pe-in pe-r pe-in pe-r pe-out
prim - fnegate - pe-r pe-in pe-r pe-out
prim - fabs - pe-r pe-in pe-r pe-out
prim - fsqrt - pe-r pe-in pe-r pe-out
prim - f< - pe-r pe-in pe-r pe-in pe-f pe-out
prim - f> - pe-r pe-in pe-r pe-in pe-f pe-out
prim - f= - pe-r pe-in pe-r pe-in pe-f pe-out
prim - f0< - pe-r pe-in pe-f pe-out
prim - f0= - pe-r pe-in pe-f pe-out
prim - s>f - pe-n pe-in pe-r pe-out
prim - f>s - pe-r pe-in pe-n pe-out
prim - f. - pe-r pe-in
prim - s" - pe-ptr-u8 pe-out pe-n pe-out
prim - c" - pe-ptr-u8 pe-out
prim - ." - -
prim - s\" - pe-ptr-u8 pe-out pe-n pe-out
prim - c\" - pe-ptr-u8 pe-out
prim - .\" - -
prim - ['] - pe-n pe-out
prim - char - pe-n pe-out
prim - [char] - pe-n pe-out
prim - emit - pe-n pe-in
prim - cr - -
prim - space - -
prim - u. - pe-n pe-in
prim - create - pe-ptr-a pe-out
prim - variable - pe-ptr-a pe-out
prim - constant - pe-a pe-out
prim - getpid - pe-n pe-out
prim - proc-watch-open - pe-n pe-in pe-n pe-out
prim - kill-errno - pe-n pe-in pe-n pe-in pe-n pe-out
prim - execve - pe-ptr-u8 pe-in pe-ptr-a pe-in pe-ptr-a pe-in pe-n pe-out
prim - munmap - pe-ptr-u8 pe-in pe-n pe-in pe-n pe-out
prim - ext-mark-free-tail - pe-ptr-u8 pe-in pe-n pe-in
prim - typefamily - -
prim - sumtype - -
prim - enum - -
prim - product - -
prim - layout-buffer - pe-n pe-in
prim - ldefer-bind - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in
prim - ldefer-grow - pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in pe-n pe-in
prim - defer-layout-buffer - -
prim - typed-buffer - pe-n pe-in
prim - typed-variable - -
-->
