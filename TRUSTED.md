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

| Word | Effect | Reason | Tests | Site | Last audited |
|------|--------|--------|-------|------|--------------|
| STDIN? | `-- ptr n` | Engine-builder mode cell that checked drivers set before emitting stdin or file-backed startup behavior. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/habu1.f:7 | 2026-06-24 |
| RPD@ | `-- ptr u8` | Reads the primitive-name pool cursor stored in a raw variable; audited accessor preserves byte-pointer type across native `@`. | `test/run.f` | src/habu/habu1.f:25 | 2026-06-24 |
| fprim | `ptr u8 n n --` | Raw-asm prim emitter: lays a REG-PRIM frame + `xt execute`s a code-emitting handler; no Forth effect to infer. | `test/run.f` | src/habu/habu1.f:47 | 2026-06-24 |
| fprim-l | `ptr u8 n n --` | Leaf variant of FPRIM (no x30 frame); same `xt execute` of a code emitter. | `test/run.f` | src/habu/habu1.f:54 | 2026-06-24 |
| linux-spawn-fail | `n --` | Linux child-side spawn failure reporter: emits raw `write` to the exec-error pipe and exits the child without returning to Forth. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:114 | 2026-06-24 |
| linux-dup2-fd | `n n n --` | Linux child-side raw syscall emitter for conditional `dup2` with exec-error-pipe reporting; label/register code and direct process exit path are not inferable as a Forth data transform. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:126 | 2026-06-24 |
| linux-chdir-fd | `n n --` | Linux child-side raw syscall emitter for optional `chdir` with exec-error-pipe reporting; label/register code and direct process exit path are not inferable as a Forth data transform. | `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f:137 | 2026-06-24 |
| linux-spawn-close-r | `--` | Linux spawn emitter helper that closes the parent/child error-pipe read fd from the raw stack frame. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:141 | 2026-06-24 |
| linux-spawn-close-w | `--` | Linux spawn emitter helper that closes the parent/child error-pipe write fd from the raw stack frame. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:145 | 2026-06-24 |
| linux-spawn-close-pipe | `--` | Linux spawn emitter helper that closes both error-pipe fds on parent-side setup failure. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:150 | 2026-06-24 |
| linux-spawn-prep-w | `--` | Linux spawn emitter helper that keeps the child failure-report fd close-on-exec and duplicates it above stdio when needed. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:167 | 2026-06-24 |
| linux-spawn-wait-stored | `--` | Linux spawn emitter helper that reaps the stored child pid after setup or exec failure so failed spawns leave no waitable child behind. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:173 | 2026-06-24 |
| linux-spawn-parent | `--` | Linux parent-side spawn handshake: reads the exec-error pipe and returns pid or `-1` through x9; raw fd/syscall/register effects are outside Forth inference. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:191 | 2026-06-24 |
| linux-spawn-child | `--` | Linux child-side spawn setup: applies cwd/stdio setup, performs raw `execve`, and reports setup/exec failure through the error pipe. | `lib/process-test.f`, `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f:207 | 2026-06-24 |
| linux-spawn | `n n n n n n n --` | Linux spawn emitter spills child exec parameters across `clone`, uses an exec-error pipe handshake, applies cwd/fd setup, and performs raw `execve`; syscall/control-flow effects are not inferable as a Forth data transform. | `lib/process-test.f`, `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f:234 | 2026-06-24 |
| linux-ignore-sigpipe | `--` | Linux raw `rt_sigaction` emitter for SIGPIPE ignore used to implement the no-SIGPIPE process fd abstraction. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:335 | 2026-06-24 |
| spawn-dup2-action | `n n --` | Build-side helper that emits one raw XNU `PSFA_DUP2` file-action record append; label/register code is not inferable as a Forth data transform. | `test/proc-pty.f`, `test/engine-suite.f` | src/habu/habu1.f:447 | 2026-06-24 |
| spawn-chdir-action | `n n --` | Build-side helper that emits one raw XNU `PSFA_CHDIR` file-action record append, bounded NUL-path copy, and branch to a caller failure label; label/register code is not inferable as a Forth data transform. | `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f:471 | 2026-06-24 |
| linux-stat-fix | `n --` | Linux stat syscall layout shim copies the kernel `mode` and `size` fields into the engine's portable `stat64` offsets; raw field writes are outside checker inference. | `lib/fs-test.f`, `test/run.f` | src/habu/habu1.f:952 | 2026-06-24 |
| emit-prims | `--` | Emits the engine's whole primitive table as raw ARM64. | `test/run.f` | src/habu/habu1.f:1172 | 2026-06-24 |
| emit-fp-prims | `--` | Emits the floating-point prim table as raw asm via FPRIM-L. | `test/run.f` | src/habu/habu1.f:1246 | 2026-06-24 |
| DATAB | `-- ptr a` | Baked REPL/debug support needs the live DATA base before checked layout accessors are loaded; fixed header access is a native engine boundary. | `test/proc-pty.f`, `test/run.f` | src/habu/repl.f:14 | 2026-06-25 |
| BPW-TAB | `-- ptr ptr n` | Watch-table storage is dictionary data whose cells hold watched DATA pointers; the checker cannot infer this created table's pointee role. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug-watch.f:10 | 2026-06-25 |
| BPW-PRINT-ADDR | `ptr n --` | Debug watch printer intentionally displays a raw cell address; formatting a pointer through `.` is a REPL/debug boundary. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug-watch.f:14 | 2026-06-25 |
| BPW-DATA-CELL | `n -- ptr n` | Converts a fixed DATA cell offset to a typed numeric-cell address for watch registration. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug-watch.f:17 | 2026-06-25 |
| BP-SLOT-ADDR | `n -- ptr ptr u8` | Breakpoint slots live in fixed DATA and store code pointers; slot field typing is outside arithmetic inference. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f:22 | 2026-06-25 |
| BP-SLOT-INSTR | `n -- ptr n` | Breakpoint slots store the saved 32-bit instruction word in fixed DATA. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f:25 | 2026-06-25 |
| BP-SLOT-HITS | `n -- ptr n` | Breakpoint slots store hit counters in fixed DATA. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f:28 | 2026-06-25 |
| BP-SLOT-CTRL | `n -- ptr n` | Breakpoint slots store packed skip/persistent control bits in fixed DATA. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f:31 | 2026-06-25 |
| BP-NULL | `-- ptr u8` | Debug slot zero is used as a null code pointer sentinel; the checker has no null-pointer literal role. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f:34 | 2026-06-25 |
| BP-PRINT-ADDR | `ptr u8 --` | Breakpoint listing intentionally prints raw code pointers through the numeric printer. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f:37 | 2026-06-25 |
| BP-PATCH32 | `n ptr u8 --` | Breakpoint installation patches executable code with a 32-bit BRK/restored instruction; code mutation is a native debug boundary. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f:40 | 2026-06-25 |
| BP-XT>PTR | `n -- ptr u8` | A ticked xt is represented as the target code address; the checker cannot refine the cell to a code pointer. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f:43 | 2026-06-25 |
| c-crash-entry | `--` | Target signal entry register shuffle is raw ABI-specific ARM64; it only mutates generated registers. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f:60 | 2026-06-25 |
| c-crash-mctx>r21 | `--` | Target ucontext-to-mcontext addressing is ABI-specific raw register code. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f:65 | 2026-06-25 |
| c-crash-xreg>r9 | `--` | Crash dump register extraction walks target mcontext layout in generated registers. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f:75 | 2026-06-25 |
| c-crash-pc>r9 | `--` | Crash dump PC extraction reads target-specific mcontext fields in generated registers. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f:80 | 2026-06-25 |
| c-crash-print-regs | `--` | Crash handler emits target-specific FP/LR/SP/PC fields through raw register/syscall code. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f:94 | 2026-06-25 |
| c-crash-pc-word | `n --` | Crash diagnostics bounds-check a saved-PC-relative word against the fixed code mapping before raw instruction loads. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f:105 | 2026-06-25 |
| c-crash-pc-8 | `--` | Crash diagnostics request the guarded instruction word at saved PC minus 8. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f:109 | 2026-06-25 |
| c-crash-pc-4 | `--` | Crash diagnostics request the guarded instruction word at saved PC minus 4. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f:113 | 2026-06-25 |
| c-crash-pc0 | `--` | Crash diagnostics request the guarded instruction word at the saved PC. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f:117 | 2026-06-25 |
| c-crash-pc+4 | `--` | Crash diagnostics request the guarded instruction word at saved PC plus 4. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f:121 | 2026-06-25 |
| c-trap-mctx>r9 | `--` | SIGTRAP handler target ucontext-to-mcontext addressing is raw ABI-specific ARM64. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:80 | 2026-06-25 |
| c-mctx-pc>r10 | `--` | SIGTRAP handler reads target-specific PC fields from mcontext in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:85 | 2026-06-25 |
| c-mctx-x19>r12 | `--` | SIGTRAP handler reads the target data-stack register from mcontext in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:90 | 2026-06-25 |
| c-mctx-sp-16! | `--` | Breakpoint resume emulates the compiled prologue by mutating target mcontext SP. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:97 | 2026-06-25 |
| c-mctx-pc+4! | `--` | Breakpoint resume skips the BRK instruction by mutating target mcontext PC. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:104 | 2026-06-25 |
| c-bp-hit-save | `--` | Breakpoint hit handling saves handler scratch registers and updates fixed DATA slot hit counters in generated code. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:112 | 2026-06-25 |
| c-bp-print-hit | `--` | Breakpoint hit reporting prints raw PC and stack-top values from target mcontext/register state. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:120 | 2026-06-25 |
| c-bp-stack-range | `--` | Breakpoint stack dumping derives raw stack bounds from fixed DATA and mcontext state. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:126 | 2026-06-25 |
| c-bp-watch-head | `--` | Breakpoint watch dumping reads fixed DATA watch metadata and emits a signal-safe header. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:132 | 2026-06-25 |
| c-bp-watch-row | `--` | Breakpoint watch dumping reads one raw watched pointer/value pair in generated code. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:139 | 2026-06-25 |
| c-bp-restore-oneshot | `--` | One-shot breakpoint restore mutates executable code, flips page permissions, and flushes I-cache. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:147 | 2026-06-25 |
| c-bp-emulate | `--` | Persistent/skip breakpoint resume emulates a compiled-word entry prologue in target mcontext. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:153 | 2026-06-25 |
| c-bp-scan | `n n n n --` | Breakpoint table scan emits branches to caller-provided raw labels and leaves hit-slot state in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:164 | 2026-06-25 |
| c-bp-stack-dump | `n n --` | Breakpoint stack dump emits a caller-labelled loop over raw data-stack cells. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:173 | 2026-06-25 |
| c-bp-watch-dump | `n n --` | Breakpoint watch dump emits a caller-labelled loop over raw watched cells. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:184 | 2026-06-25 |
| c-emit-tty-probe | `--` | Startup source selection emits target-specific tty ioctl setup; the syscall/register effects are not Forth stack effects. | `test/proc-pty.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/habu2.f:300 | 2026-06-25 |
| cf-entry | `n ptr a n n --` | Control-flow keyword case: spills the VS then `hxt execute`s a code emitter; keyword label cell is a pointer. | `test/run.f` | src/habu/habu2.f:1317 | 2026-06-25 |
| cfn-entry | `n ptr a n n --` | Like CF-ENTRY, no spill (loop words manage the VS); keyword label cell is a pointer. | `test/run.f` | src/habu/habu2.f:1327 | 2026-06-25 |
| cfb-entry | `n ptr a n n n --` | Branch-keyword case (if/until/while) with a reg-aware condition path; asm + two `hxt execute` handlers. | `test/run.f` | src/habu/habu2.f:1354 | 2026-06-25 |
| cfbn-entry | `n ptr a n n n --` | Like CFB-ENTRY, no-spill register path; raw asm + indirect xts. | `test/run.f` | src/habu/habu2.f:1375 | 2026-06-25 |
| c-local-ref | `n n --` | Compile-mode local-reference emitter: branches to the caller's not-local continuation or emits local loads, and rejects quotation-local captures with raw exit code 75. | `test/engine-suite.f`, `test/run.f` | src/habu/habu2.f:1403 | 2026-06-25 |
| em-interpret-colon | `n --` | Emits interpreter-mode colon-definition setup and jumps to the caller-provided not-colon label on non-definitions. | `test/run.f` | src/habu/habu2.f:1673 | 2026-06-25 |
| em-interpret-words | `--` | Emits interpreter-mode defining-word, literal, lookup, and execute dispatch. | `test/run.f` | src/habu/habu2.f:1693 | 2026-06-25 |
| em-interpret | `--` | Chains the factored interpreter-mode colon and word-dispatch emitters. | `test/run.f` | src/habu/habu2.f:1699 | 2026-06-25 |
| em-compile-drop-locals | `--` | Emits optional locals-frame teardown before a compiled definition returns. | `test/run.f` | src/habu/habu2.f:1706 | 2026-06-25 |
| em-compile-ret | `--` | Emits the raw return epilogue for a compiled definition. | `test/run.f` | src/habu/habu2.f:1712 | 2026-06-25 |
| em-compile-flush-pend | `--` | Finalizes the pending dictionary entry length and flips/flushed the generated code region. | `test/run.f` | src/habu/habu2.f:1718 | 2026-06-25 |
| em-compile-publish-trusted | `--` | Emits checked/trusted publication for declarations, DOES> signatures, and trust metadata. | `test/run.f` | src/habu/habu2.f:1737 | 2026-06-25 |
| em-compile-publish-hooked | `--` | Emits hook-based publication for ordinary compiled definitions. | `test/run.f` | src/habu/habu2.f:1752 | 2026-06-25 |
| em-compile-publish | `--` | Selects trusted-signature or hook publication for a closed compiled definition. | `test/run.f` | src/habu/habu2.f:1760 | 2026-06-25 |
| em-compile-semi | `n --` | Emits semicolon close handling and binds the caller-provided not-semi continuation label. | `test/run.f` | src/habu/habu2.f:1772 | 2026-06-25 |
| em-compile-keywords | `--` | Emits compile-mode keyword dispatch cases for control flow, strings, locals, quotations, and loop words. | `test/run.f` | src/habu/habu2.f:1807 | 2026-06-25 |
| em-compile-local | `--` | Emits compile-mode local-reference lookup and fallthrough. | `test/run.f` | src/habu/habu2.f:1813 | 2026-06-25 |
| em-compile-literal | `--` | Emits compile-mode numeric literal handling for integer and float literals. | `test/run.f` | src/habu/habu2.f:1824 | 2026-06-25 |
| em-compile-ops | `--` | Emits compile-mode optimized arithmetic, comparison, stack-shuffle, unary, and float operators. | `test/run.f` | src/habu/habu2.f:1854 | 2026-06-25 |
| em-compile-call | `--` | Emits compile-mode lookup, immediate execution, and call generation. | `test/run.f` | src/habu/habu2.f:1870 | 2026-06-25 |
| em-reset-compile-state | `--` | Emits reset of compile/repl/evaluate state cells after rollback or recovery. | `test/run.f` | src/habu/habu2.f:1885 | 2026-06-25 |
| em-eval-undef-rollback | `--` | Emits evaluate-frame rollback for undefined-word failures. | `test/run.f` | src/habu/habu2.f:1898 | 2026-06-25 |
| em-repl-recover | `--` | Emits REPL recovery after errors, restoring line-start compile state and stacks. | `test/run.f` | src/habu/habu2.f:1910 | 2026-06-25 |
| em-compile-undef | `--` | Emits undefined-word diagnostics and evaluate/REPL recovery routing. | `test/run.f` | src/habu/habu2.f:1922 | 2026-06-25 |
| em-eval-clean-exit | `--` | Emits clean evaluate end-of-buffer return path. | `test/run.f` | src/habu/habu2.f:1932 | 2026-06-25 |
| em-repl-read | `--` | Emits REPL line-state save, read callback call, EOF handling, and input reset. | `test/run.f` | src/habu/habu2.f:1945 | 2026-06-25 |
| em-compile-exit | `--` | Emits interpreter end-of-input handling for evaluate, REPL ok/read, and process exit. | `test/run.f` | src/habu/habu2.f:1957 | 2026-06-25 |
| em-compile | `--` | Chains the factored compile-mode dispatch, call, undefined, and exit emitters. | `test/run.f` | src/habu/habu2.f:1970 | 2026-06-25 |
| emit-main | `--` | Allocates main-loop labels and chains EM-STARTUP/COMMENT/INTERPRET/COMPILE. | `test/run.f` | src/habu/habu2.f:1975 | 2026-06-25 |
| SRCA@ | `-- ptr u8` | Reads EMIT-FORTH's saved source pointer from a raw variable for the final `BYTES,` copy. | `test/run.f` | src/habu/habu2.f:1978 | 2026-06-25 |
| emit-forth | `ptr u8 n --` | Top-level engine builder: consumes source bytes, allocates every forward-ref label, and emits the complete image. | `test/run.f` | src/habu/habu2.f:2062 | 2026-06-25 |
| IMAGE-TEXT-SIZE-OFF | `-- n` | Linux executable header offset where the snapshot writer reads the mapped text size. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:10 | 2026-06-25 |
| IMAGE-TEXT-CONTENT-ADJ | `-- n` | Linux text-size field adjustment from segment size to code-content size for snapshot streaming. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:11 | 2026-06-25 |
| IMAGE-TEXT-TRAILER-ADJ | `-- n` | Linux trailer address adjustment for snapshot restore when the text-size field includes the code offset. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:12 | 2026-06-25 |
| DATA-VA | `-- ptr a` | Linux fixed DATA virtual address used by snapshot and AOT startup writers as both cell-address and byte-span base. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:13 | 2026-06-25 |
| DATA-SIZE | `-- n` | Linux fixed DATA mapping size used by snapshot validation and image inspection. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:14 | 2026-06-25 |
| MBUF | `-- ptr u8` | Target image-builder output buffer; checked drivers write the finalized executable bytes through this audited byte span. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/elf.f:9 | 2026-06-24 |
| MLEN | `-- ptr n` | Target image-builder output length cell; checked drivers read the finalized executable length after `BUILD-IMAGE`/`CODESIG2`. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/elf.f:10 | 2026-06-24 |
| CODE-OFF | `-- n` | Linux executable code offset used by checked snapshot streaming code. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:15 | 2026-06-25 |
| ASM-CODE | `--` | Target image-builder bridge that snapshots the current assembler code length before executable wrapping. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/elf.f:51 | 2026-06-24 |
| BUILD-IMAGE | `--` | Target image-builder boundary that wraps emitted ARM64 code in the selected executable container. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/elf.f:91 | 2026-06-24 |
| BUILD-SNAP-HDR | `n -- n` | Target snapshot-header builder; consumes text payload length and returns padded executable text size. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/elf.f:101 | 2026-06-24 |
| SET-SIGID | `ptr u8 n --` | Target signing interface stores or ignores the executable identity while preserving the shared checked driver contract. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/sign.f:4 | 2026-06-24 |
| CODESIG2 | `--` | Target signing pass boundary; Linux is a no-op and macOS mutates the image buffer in place. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/sign.f:7 | 2026-06-24 |
| IMAGE-TEXT-SIZE-OFF | `-- n` | macOS executable header offset where the snapshot writer reads the mapped text size. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:10 | 2026-06-25 |
| IMAGE-TEXT-CONTENT-ADJ | `-- n` | macOS text-size field adjustment from section size to code-content size for snapshot streaming. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:11 | 2026-06-25 |
| IMAGE-TEXT-TRAILER-ADJ | `-- n` | macOS trailer address adjustment because the section size excludes the code offset. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:12 | 2026-06-25 |
| DATA-VA | `-- ptr a` | macOS fixed DATA virtual address used by snapshot and AOT startup writers as both cell-address and byte-span base. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:13 | 2026-06-25 |
| DATA-SIZE | `-- n` | macOS fixed DATA mapping size used by snapshot validation and image inspection. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:14 | 2026-06-25 |
| MBUF | `-- ptr u8` | Target image-builder output buffer; checked drivers write the finalized executable bytes through this audited byte span. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/macho.f:10 | 2026-06-24 |
| MLEN | `-- ptr n` | Target image-builder output length cell; checked drivers read the finalized executable length after `BUILD-IMAGE`/`CODESIG2`. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/macho.f:11 | 2026-06-24 |
| CODE-OFF | `-- n` | macOS executable code offset used by checked snapshot streaming code. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:15 | 2026-06-25 |
| ASM-CODE | `--` | Target image-builder bridge that snapshots the current assembler code length before executable wrapping. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/macho.f:52 | 2026-06-24 |
| BUILD-IMAGE | `--` | Target image-builder boundary that wraps emitted ARM64 code in the selected executable container. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/macho.f:120 | 2026-06-24 |
| BUILD-SNAP-HDR | `n -- n` | Target snapshot-header builder; consumes text payload length and returns padded executable text size. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/macho.f:135 | 2026-06-24 |
| SET-SIGID | `ptr u8 n --` | Target signing interface stores or ignores the executable identity while preserving the shared checked driver contract. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/sign2.f:11 | 2026-06-24 |
| CODESIG2 | `--` | Target signing pass boundary; Linux is a no-op and macOS mutates the image buffer in place. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/sign2.f:97 | 2026-06-24 |
| fold-entry | `n ptr a n n --` | JIT constant-fold case: emits the keyword guard then `fxt execute`s a fold handler + raw branches. | `test/run.f` | src/habu/jit.f:103 | 2026-06-16 |
| vop-entry | `n ptr a n n n --` | JIT binop case: fold-vs-emit split with two indirect xts and raw asm. | `test/run.f` | src/habu/jit.f:324 | 2026-06-16 |
| vopi-entry | `n ptr a n n n n --` | JIT binop-immediate case: fold/register/immediate split with three indirect xts and raw asm. | `test/run.f` | src/habu/jit.f:323 | 2026-06-16 |
| vshuf-entry | `n ptr a n n n --` | JIT reg-aware stack-shuffle case (dup/over/swap/drop/nip as register moves); `sxt execute` + raw asm. | `test/run.f` | src/habu/jit.f:724 | 2026-06-16 |
| vun-entry | `n ptr a n n n --` | JIT unary-op case: con-fold vs in-place reg op via `foldxt`/`emitxt` and raw asm. | `test/run.f` | src/habu/jit.f:757 | 2026-06-16 |
| c-prof-mctx>r21 | `--` | Profiler SIGALRM handler derives the target mcontext address from raw signal-entry registers. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:41 | 2026-06-25 |
| c-prof-pc>r9 | `--` | Profiler SIGALRM handler reads the target-specific saved PC field from mcontext. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:46 | 2026-06-25 |
| c-prof-sigaction-frame | `--` | Profiler builds the target kernel sigaction record directly on the generated stack. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:79 | 2026-06-25 |
| c-prof-sigaction | `--` | Profiler installs SIGALRM through the target raw sigaction syscall ABI. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:85 | 2026-06-25 |
| c-prof-sigaction-done | `--` | Profiler releases the generated sigaction stack scratch frame. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:89 | 2026-06-25 |
| c-prof-timer-frame | `--` | Profiler builds the target itimerval record directly on the generated stack. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:95 | 2026-06-25 |
| c-prof-timer | `--` | Profiler arms the interval timer through the raw setitimer syscall ABI. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:99 | 2026-06-25 |
| c-prof-timer-done | `--` | Profiler releases the generated timer stack scratch frame. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:103 | 2026-06-25 |
| emit-prof-prims | `--` | Emits the sampling-profiler prims as raw asm via FPRIM-L. | `test/run.f`, `test/gate-debug.f` | src/habu/prof.f:124 | 2026-06-25 |
| DIP | `R a [ R -- S ] -- S a` | Body checks, but TRUST pins the public higher-order scheme in the baked image instead of relying on build-time inference. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:5 | 2026-06-16 |
| KEEP | `R a [ R a -- S ] -- S a` | Body checks, but TRUST pins the public higher-order scheme in the baked image instead of relying on build-time inference. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:9 | 2026-06-16 |
| BI | `R a [ R a -- R b ] [ R b a -- R b c ] -- R b c` | Preserves one quotation while executing another; expressing that directly would require recursive quotation types. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:18 | 2026-06-16 |
| TRI | `R a [ R a -- R b ] [ R b a -- R b c ] [ R b c a -- R b c d ] -- R b c d` | Preserves later quotations while executing earlier ones; expressing that directly would require recursive quotation types. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:22 | 2026-06-16 |
| TIMES | `R i64 [ R -- R ] -- R` | Counted loop keeps the quotation available across repeated `execute`; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:26 | 2026-06-16 |
| EACH | `R ptr a i64 [ R a -- R ] -- R` | Array iterator keeps the quotation across element calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:30 | 2026-06-16 |
| MAP | `R ptr a i64 [ R a -- R a ] -- R` | Array map keeps the quotation across element calls and mutates cells in place; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:34 | 2026-06-16 |
| FOLD | `R ptr a i64 b [ R b a -- R b ] -- R b` | Array fold keeps the quotation across accumulator calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:38 | 2026-06-16 |
| VEC-EACH | `R ptr a [ R idx a -- R ] -- R` | Body checks, but TRUST pins the public higher-order callback scheme because the recorder does not persist this inferred quotation effect for later callers. | `lib/vector-test.f`, `test/run.f` | lib/vector.f:153 | 2026-06-24 |
| HB-TARGET-LINUX? | `-- bool` | Target selector is defined before the checker hook is installed; this pins the bool effect used by checked multi-platform source lists. | `test/run.f`, `tools/build-fixpoint-test.f` | src/core/roles.f:9 | 2026-06-24 |
| HB-TARGET-MACOS? | `-- bool` | Target selector is defined before the checker hook is installed; this pins the bool effect used by fail-closed multi-platform source lists. | `test/run.f`, `tools/build-fixpoint-test.f` | src/core/roles.f:10 | 2026-06-25 |
| HB-TARGET-KNOWN? | `-- bool` | Target selector aggregate is defined before the checker hook is installed; checked tools use it to reject unsupported targets instead of defaulting. | `test/run.f`, `tools/build-fixpoint-test.f` | src/core/roles.f:11 | 2026-06-25 |
| >IDX | `n -- idx` | Runtime identity cast from a generic cell to the nominal index role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:14 | 2026-06-22 |
| IDX>N | `idx -- n` | Runtime identity cast from the nominal index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:16 | 2026-06-22 |
| >LEN | `n -- len` | Runtime identity cast from a generic cell to the nominal length role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:19 | 2026-06-22 |
| LEN>N | `len -- n` | Runtime identity cast from the nominal length role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:21 | 2026-06-22 |
| >COUNT | `n -- count` | Runtime identity cast from a generic cell to the nominal count role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:24 | 2026-06-22 |
| COUNT>N | `count -- n` | Runtime identity cast from the nominal count role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:26 | 2026-06-22 |
| >OFF | `n -- off` | Runtime identity cast from a generic cell to the nominal offset role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:29 | 2026-06-22 |
| OFF>N | `off -- n` | Runtime identity cast from the nominal offset role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:31 | 2026-06-22 |
| >FD | `n -- fd` | Runtime identity cast from a generic cell to the nominal file-descriptor role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:34 | 2026-06-22 |
| FD>N | `fd -- n` | Runtime identity cast from the nominal file-descriptor role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:36 | 2026-06-22 |
| >RC | `n -- rc` | Runtime identity cast from a generic cell to the nominal return-code role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:39 | 2026-06-22 |
| RC>N | `rc -- n` | Runtime identity cast from the nominal return-code role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:41 | 2026-06-22 |
| >PID | `n -- pid` | Runtime identity cast from a generic cell to the nominal process-id role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:44 | 2026-06-22 |
| PID>N | `pid -- n` | Runtime identity cast from the nominal process-id role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:46 | 2026-06-22 |
| >MS | `n -- ms` | Runtime identity cast from a generic cell to the nominal millisecond role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:49 | 2026-06-22 |
| MS>N | `ms -- n` | Runtime identity cast from the nominal millisecond role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:51 | 2026-06-22 |
| >NS | `n -- ns` | Runtime identity cast from a generic cell to the nominal nanosecond role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:54 | 2026-06-22 |
| NS>N | `ns -- n` | Runtime identity cast from the nominal nanosecond role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:56 | 2026-06-22 |
| >TOK | `n -- tok` | Runtime identity cast from a generic cell to the nominal token-index role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:59 | 2026-06-22 |
| TOK>N | `tok -- n` | Runtime identity cast from the nominal token-index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:61 | 2026-06-22 |
| TTHROWS-RAW | `a n --` | Top-level test assertion boundary around execution-token `catch`; checked colon definitions should use `TTHROWSQ`, but top-level scripts cannot push `[: ;]` quotations. | `lib/test-test.f`, `test/run.f` | lib/test.f:65 | 2026-06-22 |
| FS-BYTE-OFFSET | `ptr u8 n -- ptr u8` | Byte-pointer offset refinement for filesystem record readers; runtime is raw pointer addition, but the checker currently models `+` on input pointers as numeric. Tracked by `habu-add-typed-byte-b25e923e`. | `lib/fs-test.f`, `test/run.f` | lib/fs.f:67 | 2026-06-24 |
| T{ | `--` | Array-test stack assertion DSL snapshots the live data-stack depth; the checker cannot express an arbitrary test stack tail in a reusable assertion word. | `lib/array-test.f`, `test/run.f` | lib/array-test.f:35 | 2026-06-22 |
| -> | `R --` | Array-test stack assertion DSL drains an arbitrary expected stack tail into a scratch buffer for later comparison. | `lib/array-test.f`, `test/run.f` | lib/array-test.f:38 | 2026-06-22 |
| }T | `R --` | Array-test stack assertion DSL compares an arbitrary actual stack tail against the expected scratch buffer. | `lib/array-test.f`, `test/run.f` | lib/array-test.f:44 | 2026-06-22 |
| BUILD-CHECK-RAW | `ptr u8 n -- n` | Build helper boundary around `CHECK!`; the checker cannot certify a source definition by evaluating its own checker recursively. | `lib/build-test.f`, `test/run.f` | lib/build.f:140 | 2026-06-18 |
| T-CHECK-REJECTS | `ptr u8 n --` | Engine-suite negative checker assertion temporarily suppresses diagnostics and calls `CHECK!`; recursive checker invocation and raw `DIAGXT` mutation are a tested boundary. | `test/engine-suite.f`, `test/run.f` | test/engine-suite.f:114 | 2026-06-24 |
| P5 | `-- i64` | Engine-suite trusted immediate around `POSTPONE`; the compile-time body emits `IM5`, while the declared effect is the runtime value compiled into `TP`. | `test/engine-suite.f`, `test/run.f` | test/engine-suite.f:133 | 2026-06-24 |
| PROP-CHECK-HOOK | `ptr u8 n -- n` | Property-test fail-closed source hook wraps `CHECK!`; recursive checker invocation cannot be certified by the checked source it protects. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:9 | 2026-06-24 |
| PROP-INSTALL-HOOK | `--` | Property-test installer sets the fail-closed checker hook; mutating the hook is a named trusted boundary instead of a top-level mutation. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:11 | 2026-06-24 |
| CLEAR-MEAS | `R n -- n` | Property-test oracle drains the arbitrary residual data-stack tail left by a generated program while preserving the measured count; this is exactly the value-agnostic depth boundary. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:17 | 2026-06-22 |
| ERR@ | `-- n` | Reads the engine `evaluate` recovery cell from the live `data-base` header so the in-process property oracle can distinguish clean execution from recovered traps. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:21 | 2026-06-24 |
| MARK | `--` | Property-test checkpoint captures code, dictionary, and user-signature cursors; these raw interpreter stores are outside the checker model. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:140 | 2026-06-24 |
| FORGET | `--` | Property-test rollback restores code, dictionary, and user-signature cursors after a generated program; raw interpreter-state mutation is the boundary. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:141 | 2026-06-24 |
| SMARK | `--` | Nested property-test checkpoint for shrink/metamorphic probes captures code, dictionary, and user-signature cursors. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:142 | 2026-06-24 |
| SFORGET | `--` | Nested property-test rollback restores code, dictionary, and user-signature cursors after shrink/metamorphic probes. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:143 | 2026-06-24 |
| CHK-MARK | `--` | Candidate-check checkpoint captures interpreter state before evaluating one generated definition under the verdict hook. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:144 | 2026-06-24 |
| CHK-FORGET | `--` | Candidate-check rollback removes a generated definition when the checker verdict was not certified. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:145 | 2026-06-24 |
| CHK-HOOK | `ptr u8 n -- n` | Candidate verdict hook records `CHECK!` result but returns success so rejected generated definitions can be rolled back in-process. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:151 | 2026-06-24 |
| CHK | `ptr u8 n --` | Installs the candidate verdict hook, evaluates generated source, restores the fail-closed hook, and rolls back non-certified candidates. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:153 | 2026-06-24 |
| RUN-MEAS | `n n --` | Builds and evaluates the generated measurement program, records `LAST-MEAS` or `LAST-TRAP`, and normalizes dynamic evaluation paths that the checker cannot express directly. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:159 | 2026-06-24 |
| REND-SIG$ | `-- ptr u8 n` | Reads the checker's last rendered signature buffer for the property round-trip amplifier; renderer state is internal checker state. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:198 | 2026-06-24 |
| CONFIRM-FR? | `-- bool` | False-reject oracle deliberately compiles one generated program with checking disabled, restores the hook, and measures runtime behavior to prove a rejection was real incompleteness. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f:239 | 2026-06-24 |
| MEM-ALLOC-PTR | `n -- ptr u8` | Refines a raw anonymous `mmap` result into a typed byte pointer after size validation and `-1` failure checking; the checker cannot express this syscall-result refinement yet. | `lib/memory-test.f`, `test/run.f` | lib/memory.f:50 | 2026-06-21 |
| IMG-MMAP-PTR | `n -- ptr u8` | Refines a raw file-backed `mmap` result into a typed byte pointer after checking the `-1` failure result; the checker cannot express syscall-result refinement yet. | `tools/imgdump-test.f`, `test/run.f` | tools/imgdump.f:34 | 2026-06-25 |
| JW-BUF | `-- ptr u8` | Reads the JSON writer's OS-backed output buffer pointer stored in a raw variable after capacity allocation. | `lib/json-write-test.f`, `test/run.f` | lib/json-write.f:32 | 2026-06-22 |
| EP@ | `-- ptr u8` | Reads the current byte-emission cursor stored in a raw variable; preserves pointer type for byte stores. | `test/run.f` | src/arch/arm64/icode.f:18 | 2026-06-16 |
| BYP@ | `-- ptr u8` | Reads the byte-copy cursor stored in a raw variable during `BYTES,`. | `test/run.f` | src/arch/arm64/icode.f:93 | 2026-06-24 |
| ENV-DATA | `-- ptr n` | Returns the fixed engine data-region header pointer used for argc/argv/envp cells. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:7 | 2026-06-25 |
| ENV-DATA | `-- ptr n` | Returns the fixed engine data-region header pointer used for argc/argv/envp cells. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:7 | 2026-06-25 |
| ARGC | `-- n` | Reads the process argc value captured by the native startup entry. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:10 | 2026-06-25 |
| ARGC | `-- n` | Reads the process argc value captured by the native startup entry. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:10 | 2026-06-25 |
| ARGV-BASE | `-- ptr n` | Reads the raw argv vector pointer from the engine startup cell. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:14 | 2026-06-25 |
| ARGV-BASE | `-- ptr n` | Reads the raw argv vector pointer from the engine startup cell. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:14 | 2026-06-25 |
| ARGV | `n -- ptr u8` | Reads a NUL-terminated argv entry from the raw argv vector. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:17 | 2026-06-25 |
| ARGV | `n -- ptr u8` | Reads a NUL-terminated argv entry from the raw argv vector. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:17 | 2026-06-25 |
| ENVP-BASE | `-- ptr n` | Reads the raw envp vector pointer from the engine startup cell. | `test/run.f`, `test/engine-suite.f` | src/os/macos/env.f:21 | 2026-06-25 |
| ENVP-BASE | `-- ptr n` | Reads the raw envp vector pointer from the engine startup cell. | `test/run.f`, `test/engine-suite.f` | src/os/linux/env.f:21 | 2026-06-25 |
| ENVP | `n -- ptr u8` | Reads a NUL-terminated envp entry from the raw envp vector. | `test/run.f`, `test/engine-suite.f` | src/os/macos/env.f:24 | 2026-06-25 |
| ENVP | `n -- ptr u8` | Reads a NUL-terminated envp entry from the raw envp vector. | `test/run.f`, `test/engine-suite.f` | src/os/linux/env.f:24 | 2026-06-25 |
| ZLEN | `ptr u8 -- n` | Measures a NUL-terminated byte string read from argv or envp. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:28 | 2026-06-24 |
| ZLEN | `ptr u8 -- n` | Measures a NUL-terminated byte string read from argv or envp. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:28 | 2026-06-24 |
| ARGV$ | `n -- ptr u8 n` | Converts one argv c-string pointer to a counted byte string. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:31 | 2026-06-24 |
| ARGV$ | `n -- ptr u8 n` | Converts one argv c-string pointer to a counted byte string. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:31 | 2026-06-24 |
| ENV-FALSE | `-- bool` | Produces a typed false flag for pre-hook env parsing helpers. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:34 | 2026-06-24 |
| ENV-FALSE | `-- bool` | Produces a typed false flag for pre-hook env parsing helpers. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:34 | 2026-06-24 |
| SCRIPT-LOAD-Z? | `ptr u8 -- bool` | Recognizes the `--load` argv marker in a raw argv c-string. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:44 | 2026-06-24 |
| SCRIPT-LOAD-Z? | `ptr u8 -- bool` | Recognizes the `--load` argv marker in a raw argv c-string. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:44 | 2026-06-24 |
| SCRIPT-LOAD? | `-- bool` | Detects source-list mode from captured process argv. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:49 | 2026-06-24 |
| SCRIPT-LOAD? | `-- bool` | Detects source-list mode from captured process argv. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:49 | 2026-06-24 |
| SCRIPT-SEP? | `n -- bool` | Recognizes the `--` argv separator in source-list mode. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:56 | 2026-06-24 |
| SCRIPT-SEP? | `n -- bool` | Recognizes the `--` argv separator in source-list mode. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:56 | 2026-06-24 |
| SCRIPT-ARG-START | `-- n` | Computes the first user script argument after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:65 | 2026-06-24 |
| SCRIPT-ARG-START | `-- n` | Computes the first user script argument after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:65 | 2026-06-24 |
| SCRIPT-ARGC | `-- n` | Returns user script argument count after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:69 | 2026-06-24 |
| SCRIPT-ARGC | `-- n` | Returns user script argument count after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:69 | 2026-06-24 |
| SCRIPT-ARGV | `n -- ptr u8` | Returns one user script argv c-string after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:72 | 2026-06-24 |
| SCRIPT-ARGV | `n -- ptr u8` | Returns one user script argv c-string after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:72 | 2026-06-24 |
| SCRIPT-ARGV$ | `n -- ptr u8 n` | Returns one user script argument as counted bytes. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:75 | 2026-06-24 |
| SCRIPT-ARGV$ | `n -- ptr u8 n` | Returns one user script argument as counted bytes. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:75 | 2026-06-24 |
| ENV=? | `ptr u8 ptr u8 n -- bool` | Compares one envp c-string against a counted variable name and following `=`. | `lib/process-env-test.f`, `test/run.f` | src/os/macos/env.f:81 | 2026-06-24 |
| ENV=? | `ptr u8 ptr u8 n -- bool` | Compares one envp c-string against a counted variable name and following `=`. | `lib/process-env-test.f`, `test/run.f` | src/os/linux/env.f:80 | 2026-06-24 |
| NULL$ | `-- ptr u8 n` | Returns a typed empty string pair used for absent environment values. | `test/run.f`, `test/engine-suite.f` | src/os/macos/env.f:85 | 2026-06-25 |
| NULL$ | `-- ptr u8 n` | Returns a typed empty string pair used for absent environment values. | `test/run.f`, `test/engine-suite.f` | src/os/linux/env.f:84 | 2026-06-25 |
| GETENV | `ptr u8 n -- ptr u8 n` | Returns a counted environment value by scanning the raw envp vector captured at startup. | `lib/process-env-test.f`, `test/run.f` | src/os/macos/env.f:95 | 2026-06-25 |
| GETENV | `ptr u8 n -- ptr u8 n` | Returns a counted environment value by scanning the raw envp vector captured at startup. | `lib/process-env-test.f`, `test/run.f` | src/os/linux/env.f:92 | 2026-06-25 |
| TPP@ | `-- ptr u8` | Reads the temporary-path scratch cursor from a raw variable. | `test/run.f` | src/os/macos/env.f:103 | 2026-06-25 |
| TPP@ | `-- ptr u8` | Reads the temporary-path scratch cursor from a raw variable. | `test/run.f` | src/os/linux/env.f:99 | 2026-06-25 |
| TMP-PATH-CHECK | `n --` | Validates the fixed target path scratch capacity before raw byte copies. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/env.f:107 | 2026-06-25 |
| TMP-PATH-CHECK | `n --` | Validates the fixed target path scratch capacity before raw byte copies. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/env.f:103 | 2026-06-25 |
| TMP-PATH | `ptr u8 n -- ptr u8 n` | Builds `$HB_TMP` or `/tmp` child paths in pre-hook engine build drivers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/env.f:116 | 2026-06-25 |
| TMP-PATH | `ptr u8 n -- ptr u8 n` | Builds `$HB_TMP` or `/tmp` child paths in pre-hook engine build drivers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/env.f:112 | 2026-06-25 |
| SHK-A@ | `-- ptr u8` | Reads the treeshaker source-buffer pointer stored in a raw variable. | `test/run.f` | src/habu/treeshake.f:10 | 2026-06-16 |
| TA@ | `-- ptr u8` | Reads the current treeshaker token pointer stored in a raw variable. | `test/run.f` | src/habu/treeshake.f:45 | 2026-06-16 |
| MP@ | `-- ptr u8` | Reads the Mach-O output cursor stored in a raw variable. | `test/run.f` | src/os/macos/macho.f:13 | 2026-06-16 |
| MP@ | `-- ptr u8` | Reads the ELF output cursor stored in a raw variable. | `test/run.f` | src/os/linux/elf.f:12 | 2026-06-24 |
| PHP@ | `-- ptr u8` | Reads the Mach-O header patch cursor stored in a raw variable. | `test/run.f` | src/os/macos/macho.f:93 | 2026-06-16 |
| SIGA@ | `-- ptr u8` | Reads the code-signing identifier pointer stored in a raw variable. | `test/run.f` | src/os/macos/sign2.f:8 | 2026-06-16 |
| HLP@ | `-- ptr u8` | Reads the code-signing header patch cursor stored in a raw variable. | `test/run.f` | src/os/macos/sign2.f:32 | 2026-06-16 |
| HB@ | `-- ptr u8` | Reads the stdin-engine baked-source buffer pointer stored in a raw variable. | `test/run.f` | src/habu/stdin.f:19 | 2026-06-16 |
| BLD-PB@ | `-- ptr u8` | Reads the standalone-build source buffer pointer stored in a raw variable. | `test/run.f`, `tools/hb-build.f` | src/habu/build.f:22 | 2026-06-24 |
| V-TRUST-SIG | `ptr u8 n ptr u8 n --` | hb-build pre-verifier records source-order defining-word signatures for parsed names; the checker cannot infer a dynamic mutation of its signature table from scanner state. | `tools/hb-build-test.f`, `test/run.f` | src/habu/build.f:128 | 2026-06-24 |
| AOT-PB@ | `-- ptr u8` | Reads the AOT build source buffer pointer stored in a raw variable. | `test/run.f`, `tools/hb-build.f` | src/habu/aot.f:19 | 2026-06-24 |
| AOT-DBASE@ | `-- ptr a` | Reads the runtime dictionary base pointer for AOT dictionary-record scans; record fields are mixed, so callers specialize the pointee type at each access. | `test/run.f`, `tools/hb-build.f` | src/habu/aot.f:21 | 2026-06-24 |
| AOT-PTR@ | `ptr a -- ptr a` | Reads a dictionary long-name pointer field whose pointee is another address; the checker cannot express this pointer-to-pointer load yet. | `test/run.f`, `tools/hb-build.f` | src/habu/aot.f:24 | 2026-06-24 |
| MK-SBUF@ | `-- ptr u8` | Reads the hb-build maker source buffer pointer stored in a raw variable while compiling the separate maker image. | `tools/hb-build-test.f`, `test/run.f` | src/habu/maker.f:25 | 2026-06-24 |
| STB@ | `-- ptr u8` | Reads the snapshot source text base pointer stored in a raw variable. | `test/run.f`, `tools/build-fixpoint.f snap` | src/habu/snap.f:19 | 2026-06-16 |
| STB-CELL@ | `-- ptr n` | Reads the snapshot source text base pointer as a cell-address for executable-header size lookup. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/snap.f:21 | 2026-06-24 |
| SDB@ | `-- ptr u8` | Reads the snapshot dictionary/data-region pointer stored in a raw variable. | `test/run.f`, `tools/build-fixpoint.f snap` | src/habu/snap.f:23 | 2026-06-16 |
| SNAP-CHECK-HOOK | `ptr u8 n -- n` | Snapshot image installs the fail-closed checker hook into the emitted image; recursive `CHECK!` hook bodies are trusted boundaries. | `test/run.f`, `test/gate-debug.f` | src/habu/snap.f:61 | 2026-06-24 |
| SNAP-INSTALL-HOOK | `--` | Snapshot image mutates the checker hook cell only through a named trusted installer. | `test/run.f`, `test/gate-debug.f` | src/habu/snap.f:63 | 2026-06-24 |
| SBUF@ | `-- ptr u8` | Reads the stage2 source buffer pointer stored in a raw variable. | `test/run.f` | src/habu/stage2.f:12 | 2026-06-16 |
| IMGD-MMAP-PTR | `n -- ptr u8` | Converts the raw image mmap result into a typed byte pointer after checking mmap failure; OS mapping pointers are outside checker inference. | `tools/imagedisasm-test.f`, `test/run.f` | tools/imagedisasm.f:45 | 2026-06-25 |
| MR-LINE! | `ptr u8 n --` | Stores the current model-registry line pointer and length in raw variables across row-field parsing. | `bench/llm/model-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/model.f:38 | 2026-06-21 |
| MR-LINE$ | `-- ptr u8 n` | Reads the current model-registry line pointer and length from raw variables. | `bench/llm/model-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/model.f:42 | 2026-06-21 |
| MRUN-OUT-BUF | `-- ptr u8` | Reads the OS-backed model stdout buffer pointer stored in a raw variable after capacity allocation. | `bench/llm/model-run-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/model-run.f:32 | 2026-06-21 |
| MRUN-ERR-BUF | `-- ptr u8` | Reads the OS-backed model stderr buffer pointer stored in a raw variable after capacity allocation. | `bench/llm/model-run-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/model-run.f:35 | 2026-06-21 |
| MRUN-TEXT-BUF | `-- ptr u8` | Reads the OS-backed parsed model-text buffer pointer stored in a raw variable after capacity allocation. | `bench/llm/model-run-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/model-run.f:38 | 2026-06-21 |
| MRUN-PROMPT$ | `-- ptr u8 n` | Reads the saved model prompt pointer and length from raw variables so `catch` can call a no-input wrapper without closing over locals. | `bench/llm/model-run-test.f`, `test/run.f` | bench/llm/model-run.f:41 | 2026-06-22 |
| LR-SET$ | `ptr u8 n ptr n ptr n --` | Stores live-row string pointer and length cells through raw address parameters; checker lacks typed field references. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:88 | 2026-06-22 |
| LR-FILE-BUF | `-- ptr u8` | Reads the live-row artifact buffer pointer stored in a raw variable after OS-backed capacity allocation. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:92 | 2026-06-22 |
| LR-RUN-ID$ | `-- ptr u8 n` | Reads the live-row run-id pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:147 | 2026-06-22 |
| LR-NAME$ | `-- ptr u8 n` | Reads the live-row task-name pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:150 | 2026-06-22 |
| LR-MODEL-ID$ | `-- ptr u8 n` | Reads the live-row model-id pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:153 | 2026-06-22 |
| LR-MODEL$ | `-- ptr u8 n` | Reads the live-row model-label pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:156 | 2026-06-22 |
| LR-ARM$ | `-- ptr u8 n` | Reads the live-row arm pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:159 | 2026-06-22 |
| LR-SEED$ | `-- ptr u8 n` | Reads the live-row seed pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:162 | 2026-06-22 |
| LR-OUTCOME$ | `-- ptr u8 n` | Reads the live-row outcome pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:165 | 2026-06-22 |
| LR-FAMILY$ | `-- ptr u8 n` | Reads the live-row family pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:168 | 2026-06-22 |
| LR-MODEL-VERSION$ | `-- ptr u8 n` | Reads the live-row model-version pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:171 | 2026-06-22 |
| LR-MODEL-DATE$ | `-- ptr u8 n` | Reads the live-row model-date pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:174 | 2026-06-22 |
| LR-FIRST-CHECKER$ | `-- ptr u8 n` | Reads the live-row first-checker-status pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:177 | 2026-06-22 |
| LR-RUNTIME-STATUS$ | `-- ptr u8 n` | Reads the live-row runtime-status pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:180 | 2026-06-22 |
| LR-REPAIR-CLASS$ | `-- ptr u8 n` | Reads the live-row repair-class pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:183 | 2026-06-22 |
| DS-SET$ | `ptr u8 n ptr n ptr n --` | Stores driver string pointer and length cells through raw address parameters; checker lacks typed field references. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:128 | 2026-06-24 |
| DS-NAME$ | `-- ptr u8 n` | Reads the driver task-name pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:150 | 2026-06-24 |
| DS-SIG$ | `-- ptr u8 n` | Reads the driver signature pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:153 | 2026-06-24 |
| DS-CATEGORY$ | `-- ptr u8 n` | Reads the driver category pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:156 | 2026-06-24 |
| DS-TESTS$ | `-- ptr u8 n` | Reads the driver tests-description pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:159 | 2026-06-24 |
| DS-SPEC$ | `-- ptr u8 n` | Reads the driver task-spec pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:162 | 2026-06-24 |
| DS-SEED$ | `-- ptr u8 n` | Reads the driver seed pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:165 | 2026-06-24 |
| DS-LINE! | `ptr u8 n --` | Stores the current manifest row pointer and length in raw variables during driver row scanning. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:341 | 2026-06-24 |
| DS-LINE$ | `-- ptr u8 n` | Reads the current manifest row pointer and length from raw variables during driver row scanning. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:345 | 2026-06-24 |
| FTL-TASK-BUF | `-- ptr u8` | Reads the OS-backed task manifest buffer pointer stored in a raw variable. | `bench/llm/forth-task-lines-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/forth-task-lines-lib.f:16 | 2026-06-21 |
| FTL-OUT-BUF | `-- ptr u8` | Reads the OS-backed emitted task-row buffer pointer stored in a raw variable. | `bench/llm/forth-task-lines-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/forth-task-lines-lib.f:19 | 2026-06-21 |
| AS-LINE! | `ptr u8 n --` | Stores the current attempt-solution line pointer and length in raw variables during TSV scanning. | `bench/llm/attempt-solutions-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/attempt-solutions-lib.f:47 | 2026-06-21 |
| AS-LINE$ | `-- ptr u8 n` | Reads the current attempt-solution line pointer and length from raw variables during TSV scanning. | `bench/llm/attempt-solutions-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/attempt-solutions-lib.f:50 | 2026-06-21 |
| AS-TASK-BUF | `-- ptr u8` | Reads the OS-backed task TSV buffer pointer stored in a raw variable. | `bench/llm/attempt-solutions-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/attempt-solutions-lib.f:53 | 2026-06-21 |
| AS-SOL-BUF | `-- ptr u8` | Reads the OS-backed solution source buffer pointer stored in a raw variable. | `bench/llm/attempt-solutions-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/attempt-solutions-lib.f:56 | 2026-06-21 |
| DFH-FEEDBACK$ | `-- ptr u8 n` | Reads the Forth driver feedback-mode pointer and length stored in raw variables. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:43 | 2026-06-21 |
| DFH-ARM$ | `-- ptr u8 n` | Reads the Forth driver arm pointer and length stored in raw variables. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:46 | 2026-06-21 |
| DFH-TASK-BUF | `-- ptr u8` | Reads the OS-backed Forth task buffer pointer stored in a raw variable. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:52 | 2026-06-21 |
| DFH-BUNDLE-BUF$ | `-- ptr u8 n` | Reads the OS-backed Forth bundle buffer pointer and capacity from raw variables. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:61 | 2026-06-21 |
| DFH-SCRATCH-BUF$ | `-- ptr u8 n` | Reads the OS-backed Forth scratch buffer pointer and capacity from raw variables. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:64 | 2026-06-21 |
| DFH-BUNDLE$ | `-- ptr u8 n` | Reads the emitted Forth bundle pointer and current length from raw variables. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:67 | 2026-06-21 |
| DFHT-SRC-BUF | `-- ptr u8` | Reads the OS-backed large source fixture buffer pointer stored in a raw variable. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-test.f:10 | 2026-06-21 |
| BMA-LINE! | `ptr u8 n --` | Stores the current manifest-audit line pointer and length in raw variables during TSV scanning. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:31 | 2026-06-21 |
| BMA-LINE$ | `-- ptr u8 n` | Reads the current manifest-audit line pointer and length from raw variables during TSV scanning. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:34 | 2026-06-21 |
| BMA-TAG! | `ptr u8 n --` | Stores the current manifest-audit tag pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:37 | 2026-06-21 |
| BMA-TAG$ | `-- ptr u8 n` | Reads the current manifest-audit tag pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:40 | 2026-06-21 |
| BMA-ID! | `ptr u8 n --` | Stores the current manifest-audit task-id pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:43 | 2026-06-21 |
| BMA-ID$ | `-- ptr u8 n` | Reads the current manifest-audit task-id pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:46 | 2026-06-21 |
| BMA-NAME! | `ptr u8 n --` | Stores the current manifest-audit task-name pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:49 | 2026-06-21 |
| BMA-NAME$ | `-- ptr u8 n` | Reads the current manifest-audit task-name pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:52 | 2026-06-21 |
| BMA-CAT! | `ptr u8 n --` | Stores the current manifest-audit category pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:55 | 2026-06-21 |
| BMA-CAT$ | `-- ptr u8 n` | Reads the current manifest-audit category pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:58 | 2026-06-21 |
| BMA-HARNESS! | `ptr u8 n --` | Stores the current manifest-audit harness pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:61 | 2026-06-21 |
| BMA-HARNESS$ | `-- ptr u8 n` | Reads the current manifest-audit harness pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:64 | 2026-06-21 |
| BMA-CONV! | `ptr u8 n --` | Stores the current manifest-audit convention pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:67 | 2026-06-21 |
| BMA-CONV$ | `-- ptr u8 n` | Reads the current manifest-audit convention pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:70 | 2026-06-21 |
| BMA-TAGS! | `ptr u8 n --` | Stores the current manifest-audit tag-list pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:73 | 2026-06-21 |
| BMA-TAGS$ | `-- ptr u8 n` | Reads the current manifest-audit tag-list pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:76 | 2026-06-21 |
| LBB-SRC-BUF | `-- ptr u8` | Reads the OS-backed large-buffer fixture source pointer stored in a raw variable. | `bench/llm/large-buffer-bundle-test.f`, `test/run.f` | bench/llm/large-buffer-bundle-test.f:19 | 2026-06-21 |
| DAH-CONV$ | `-- ptr u8 n` | Reads the Habu array driver convention pointer and length stored in raw variables. | `bench/llm/drive-array-habu-test.f`, `bench/llm/drive-array-habu-repair-test.f`, `test/run.f` | bench/llm/drive-array-habu-lib.f:45 | 2026-06-21 |
| DAH-VECTORS$ | `-- ptr u8 n` | Reads the Habu array driver vector-spec pointer and length stored in raw variables. | `bench/llm/drive-array-habu-test.f`, `bench/llm/drive-array-habu-repair-test.f`, `test/run.f` | bench/llm/drive-array-habu-lib.f:48 | 2026-06-21 |
| DAH-ARM$ | `-- ptr u8 n` | Reads the Habu array driver arm pointer and length stored in raw variables. | `bench/llm/drive-array-habu-test.f`, `bench/llm/drive-array-habu-repair-test.f`, `test/run.f` | bench/llm/drive-array-habu-lib.f:51 | 2026-06-21 |
