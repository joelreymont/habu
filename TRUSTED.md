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
| STDIN? | `-- ptr bool` | Engine-builder mode cell that checked drivers set before emitting stdin or file-backed startup behavior. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/habu1.f:7 | 2026-06-26 |
| fprim | `ptr u8 n n --` | Raw-asm prim emitter: lays a REG-PRIM frame + `xt execute`s a code-emitting handler; no Forth effect to infer. | `test/run.f` | src/habu/habu1.f:67 | 2026-06-24 |
| fprim-l | `ptr u8 n n --` | Leaf variant of FPRIM (no x30 frame); same `xt execute` of a code emitter. | `test/run.f` | src/habu/habu1.f:75 | 2026-06-24 |
| linux-spawn-fail | `reg --` | Linux child-side spawn failure reporter: consumes the target register holding the exec-error pipe, emits raw `write`, and exits the child without returning to Forth. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:135 | 2026-06-26 |
| linux-dup2-fd | `reg fd reg --` | Linux child-side raw syscall emitter for conditional `dup2`: source fd register, destination fd immediate, and exec-error-pipe register are role-typed; raw label/syscall code remains the boundary. | `lib/process-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f:147 | 2026-06-26 |
| linux-chdir-fd | `reg reg --` | Linux child-side raw syscall emitter for optional `chdir`: cwd pointer register and exec-error-pipe register are role-typed; raw label/syscall code remains the boundary. | `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f:158 | 2026-06-26 |
| linux-spawn-close-r | `--` | Linux spawn emitter helper that closes the parent/child error-pipe read fd from the raw stack frame. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:162 | 2026-06-24 |
| linux-spawn-close-w | `--` | Linux spawn emitter helper that closes the parent/child error-pipe write fd from the raw stack frame. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:166 | 2026-06-24 |
| linux-spawn-close-pipe | `--` | Linux spawn emitter helper that closes both error-pipe fds on parent-side setup failure. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:171 | 2026-06-24 |
| linux-spawn-prep-w | `--` | Linux spawn emitter helper that keeps the child failure-report fd close-on-exec and duplicates it above stdio when needed. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:188 | 2026-06-24 |
| linux-spawn-wait-stored | `--` | Linux spawn emitter helper that reaps the stored child pid after setup or exec failure so failed spawns leave no waitable child behind. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:194 | 2026-06-24 |
| linux-spawn-parent | `--` | Linux parent-side spawn handshake: reads the exec-error pipe and returns pid or `-1` through x9; raw fd/syscall/register effects are outside Forth inference. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:212 | 2026-06-24 |
| linux-spawn-child | `--` | Linux child-side spawn setup: applies cwd/stdio setup, performs raw `execve`, and reports setup/exec failure through the error pipe. | `lib/process-test.f`, `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f:228 | 2026-06-24 |
| linux-spawn | `reg reg reg reg reg reg reg --` | Linux spawn emitter consumes target registers for path, argv, envp, cwd, stdin, stdout, and stderr; syscall/control-flow effects remain the boundary. | `lib/process-test.f`, `lib/process-cwd-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f:255 | 2026-06-26 |
| linux-ignore-sigpipe | `--` | Linux raw `rt_sigaction` emitter for SIGPIPE ignore used to implement the no-SIGPIPE process fd abstraction. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:356 | 2026-06-24 |
| spawn-dup2-action | `reg fd --` | Build-side helper that emits one raw XNU `PSFA_DUP2` action from a target fd register to a destination fd immediate; raw record layout remains the boundary. | `tools/spawn-emitter-test.f`, `test/proc-pty.f`, `test/engine-suite.f` | src/habu/habu1.f:488 | 2026-06-26 |
| spawn-chdir-action | `reg label --` | Build-side helper that emits one raw XNU `PSFA_CHDIR` action from a cwd pointer register and branches to a caller failure label; raw record layout remains the boundary. | `tools/spawn-emitter-test.f`, `lib/process-cwd-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f:519 | 2026-06-26 |
| spawn-darwin-frame3-enter | `--` | Build-side helper that emits the shared three-action Darwin spawn runtime stack-frame allocation. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:523 | 2026-06-25 |
| spawn-darwin-frame3-leave | `--` | Build-side helper that emits the shared three-action Darwin spawn runtime stack-frame release. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:527 | 2026-06-25 |
| spawn-darwin-frame4-enter | `--` | Build-side helper that emits the extended Darwin spawn runtime stack-frame allocation used by cwd actions. | `tools/spawn-emitter-test.f`, `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f:533 | 2026-06-25 |
| spawn-darwin-frame4-leave | `--` | Build-side helper that emits the extended Darwin spawn runtime stack-frame release used by cwd actions. | `tools/spawn-emitter-test.f`, `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f:539 | 2026-06-25 |
| spawn-darwin-actions-reset | `count --` | Build-side helper that initializes the XNU file-action blob header at x13 for the requested action count and zero used-count. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f:546 | 2026-06-26 |
| spawn-darwin-stdio-actions | `--` | Build-side helper that appends the three conditional stdio `PSFA_DUP2` actions through the audited dup2 action emitter. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:552 | 2026-06-25 |
| spawn-darwin-zero-adesc | `--` | Build-side helper that emits zeroing stores for the Darwin spawn descriptor area; raw descriptor layout is outside Forth inference. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:561 | 2026-06-25 |
| spawn-darwin-fill-adesc | `--` | Build-side helper that emits the XNU spawn descriptor file-action size and pointer fields from the runtime action count. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:570 | 2026-06-25 |
| spawn-darwin-nullable-adesc | `label --` | Build-side helper that emits the XNU empty-action rule and binds the caller label used after a non-null descriptor decision. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f:579 | 2026-06-26 |
| spawn-darwin-use-adesc | `--` | Build-side helper that emits the non-null descriptor pointer for cwd spawn, whose `PSFA_CHDIR` action is mandatory. | `tools/spawn-emitter-test.f`, `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f:583 | 2026-06-25 |
| spawn-darwin-pid-path | `reg --` | Build-side helper that emits common `posix_spawn` pid-out and path register setup. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f:589 | 2026-06-26 |
| spawn-darwin-argv-envp | `reg reg --` | Build-side helper that emits common argv/envp register setup when both vectors are runtime input registers. | `tools/spawn-emitter-test.f`, `lib/process-env-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f:595 | 2026-06-26 |
| spawn-darwin-default-argv-envp | `reg --` | Build-side helper that emits the default argv/envp runtime stack vectors for path-only spawn. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:603 | 2026-06-26 |
| spawn-darwin-default-envp | `--` | Build-side helper that emits the default empty envp runtime stack vector for argv spawn. | `tools/spawn-emitter-test.f`, `lib/process-argv-test.f`, `test/run.f` | src/habu/habu1.f:608 | 2026-06-25 |
| spawn-darwin-use-default-argv-envp | `--` | Build-side helper that emits `posix_spawn` argv/envp argument registers for the path-only default vectors. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f:613 | 2026-06-25 |
| spawn-darwin-argv-default-envp | `reg --` | Build-side helper that emits `posix_spawn` argv input plus default empty envp argument registers. | `tools/spawn-emitter-test.f`, `lib/process-argv-test.f`, `test/run.f` | src/habu/habu1.f:619 | 2026-06-26 |
| spawn-darwin-finish | `label label --` | Build-side helper that emits shared Darwin `posix_spawn` syscall, carry/error handling, pid load, failure join, and data-stack push using explicit success/failure labels. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `lib/process-cwd-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f:630 | 2026-06-26 |
| linux-stat-fix | `n --` | Linux stat syscall layout shim copies the kernel `mode` and `size` fields into the engine's portable `stat64` offsets; raw field writes are outside checker inference. | `lib/fs-test.f`, `test/run.f` | src/habu/habu1.f:1084 | 2026-06-24 |
| emit-prims | `--` | Emits the engine's whole primitive table as raw ARM64. | `test/run.f` | src/habu/habu1.f:1309 | 2026-06-24 |
| emit-fp-prims | `--` | Emits the floating-point prim table as raw asm via FPRIM-L. | `test/run.f` | src/habu/habu1.f:1383 | 2026-06-24 |
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
| XREF-N>REC | `n -- ptr a` | Converts a numeric live dictionary-record address into an opaque record pointer for checked xref helpers; the record base comes from `dbase@` plus `DREC` arithmetic. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f:14 | 2026-06-27 |
| XREF-A>U8 | `ptr a -- ptr u8` | Treats the inline-name bytes inside a dictionary record as a byte string; fixed raw record byte offsets are outside pointer-role inference. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f:15 | 2026-06-27 |
| XREF-N>U8 | `n -- ptr u8` | Converts a numeric long-name address fetched from a dictionary record into a byte pointer; the record stores mixed numeric and pointer cells. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f:16 | 2026-06-27 |
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
| c-trap-mctx>r9 | `--` | SIGTRAP handler target ucontext-to-mcontext addressing is raw ABI-specific ARM64. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:139 | 2026-06-25 |
| c-mctx-pc>r10 | `--` | SIGTRAP handler reads target-specific PC fields from mcontext in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:144 | 2026-06-25 |
| c-mctx-x19>r12 | `--` | SIGTRAP handler reads the target data-stack register from mcontext in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:149 | 2026-06-25 |
| c-mctx-sp-16! | `--` | Breakpoint resume emulates the compiled prologue by mutating target mcontext SP. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:156 | 2026-06-25 |
| c-mctx-pc+4! | `--` | Breakpoint resume skips the BRK instruction by mutating target mcontext PC. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:163 | 2026-06-25 |
| c-bp-hit-save | `--` | Breakpoint hit handling saves handler scratch registers and updates fixed DATA slot hit counters in generated code. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:171 | 2026-06-25 |
| c-bp-print-hit | `--` | Breakpoint hit reporting prints raw PC and stack-top values from target mcontext/register state. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:179 | 2026-06-25 |
| c-bp-stack-range | `--` | Breakpoint stack dumping derives raw stack bounds from fixed DATA and mcontext state. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:185 | 2026-06-25 |
| c-bp-watch-head | `--` | Breakpoint watch dumping reads fixed DATA watch metadata and emits a signal-safe header. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:191 | 2026-06-25 |
| c-bp-watch-row | `--` | Breakpoint watch dumping reads one raw watched pointer/value pair in generated code. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:198 | 2026-06-25 |
| c-bp-restore-oneshot | `--` | One-shot breakpoint restore mutates executable code, flips page permissions, and flushes I-cache. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:206 | 2026-06-25 |
| c-bp-emulate | `--` | Persistent/skip breakpoint resume emulates a compiled-word entry prologue in target mcontext. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:212 | 2026-06-25 |
| c-bp-scan | `n n n n --` | Breakpoint table scan emits branches to caller-provided raw labels and leaves hit-slot state in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:223 | 2026-06-25 |
| c-bp-stack-dump | `n n --` | Breakpoint stack dump emits a caller-labelled loop over raw data-stack cells. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:232 | 2026-06-25 |
| c-bp-watch-dump | `n n --` | Breakpoint watch dump emits a caller-labelled loop over raw watched cells. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f:243 | 2026-06-25 |
| c-emit-tty-probe | `--` | Startup source selection emits target-specific tty ioctl setup; the syscall/register effects are not Forth stack effects. | `test/proc-pty.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/habu2.f:391 | 2026-06-25 |
| EM-HXT-EXECUTE | `n --` | Narrow higher-order emitter boundary: checked dispatcher words pass one build-time emitter xt through this raw `execute` shim. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/habu2.f:1427 | 2026-06-26 |
| c-local-ref | `n n --` | Compile-mode local-reference emitter: branches to the caller's not-local continuation or emits local loads, and rejects quotation-local captures with raw exit code 75. | `test/engine-suite.f`, `test/run.f` | src/habu/habu2.f:1528 | 2026-06-25 |
| EM-DATA-VA>N | `-- n` | Engine-builder raw emitter boundary: exposes the fixed DATA-VA pointer as the numeric immediate needed by `LIT64,` when emitting the startup mmap check. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/habu2.f:1579 | 2026-06-26 |
| em-interpret-colon | `n --` | Emits interpreter-mode colon-definition setup and jumps to the caller-provided not-colon label on non-definitions. | `test/run.f` | src/habu/habu2.f:1791 | 2026-06-25 |
| em-interpret-define-keywords | `--` | Emits interpreter-mode defining-word dispatch cases grouped separately from literal and lookup fallback. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:1801 | 2026-06-25 |
| em-interpret-string-keywords | `--` | Emits interpreter-mode string parsing-word dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:1807 | 2026-06-25 |
| em-interpret-number | `n --` | Emits interpreter-mode number parsing and branches to the caller's not-number label on failure. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:1812 | 2026-06-25 |
| em-interpret-find | `--` | Emits interpreter-mode dictionary lookup, undefined routing, and execute dispatch. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:1818 | 2026-06-25 |
| em-interpret-words | `--` | Chains the factored interpreter-mode defining, string, number, and lookup dispatch emitters. | `test/run.f` | src/habu/habu2.f:1827 | 2026-06-25 |
| em-interpret | `--` | Chains the factored interpreter-mode colon and word-dispatch emitters. | `test/run.f` | src/habu/habu2.f:1833 | 2026-06-25 |
| em-compile-drop-locals | `--` | Emits optional locals-frame teardown before a compiled definition returns. | `test/run.f` | src/habu/habu2.f:1840 | 2026-06-25 |
| em-compile-ret | `--` | Emits the raw return epilogue for a compiled definition. | `test/run.f` | src/habu/habu2.f:1846 | 2026-06-25 |
| em-compile-flush-pend | `--` | Finalizes the pending dictionary entry length and flips/flushed the generated code region. | `test/run.f` | src/habu/habu2.f:1852 | 2026-06-25 |
| em-compile-publish-trusted | `--` | Emits checked/trusted publication for declarations, DOES> signatures, and trust metadata. | `test/run.f` | src/habu/habu2.f:1871 | 2026-06-25 |
| em-compile-publish-hooked | `--` | Emits hook-based publication for ordinary compiled definitions. | `test/run.f` | src/habu/habu2.f:1886 | 2026-06-25 |
| em-compile-publish | `--` | Selects trusted-signature or hook publication for a closed compiled definition. | `test/run.f` | src/habu/habu2.f:1894 | 2026-06-25 |
| em-compile-semi | `n --` | Emits semicolon close handling and binds the caller-provided not-semi continuation label. | `test/run.f` | src/habu/habu2.f:1906 | 2026-06-25 |
| em-compile-control-keywords | `--` | Emits compile-mode control-flow keyword dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:1917 | 2026-06-25 |
| em-compile-string-keywords | `--` | Emits compile-mode string parsing-word dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:1923 | 2026-06-25 |
| em-compile-meta-keywords | `--` | Emits compile-mode meta/parsing keyword dispatch cases such as tick, postpone, DOES>, and quotations. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:1932 | 2026-06-25 |
| em-compile-loop-keywords | `--` | Emits compile-mode loop, return-stack, recursion, and locals keyword dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:1949 | 2026-06-25 |
| em-compile-keywords | `--` | Chains factored compile-mode keyword dispatch groups. | `test/run.f` | src/habu/habu2.f:1957 | 2026-06-25 |
| em-compile-local | `--` | Emits compile-mode local-reference lookup and fallthrough. | `test/run.f` | src/habu/habu2.f:1963 | 2026-06-25 |
| em-compile-literal | `--` | Emits compile-mode numeric literal handling for integer and float literals. | `test/run.f` | src/habu/habu2.f:1974 | 2026-06-25 |
| em-compile-arith-ops | `--` | Emits arithmetic and bitwise optimized operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:1983 | 2026-06-25 |
| em-compile-shuffle-ops | `--` | Emits optimized stack-shuffle operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:1991 | 2026-06-25 |
| em-compile-compare-ops | `--` | Emits optimized comparison operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:2000 | 2026-06-25 |
| em-compile-unary-ops | `--` | Emits optimized unary numeric operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:2009 | 2026-06-25 |
| em-compile-float-ops | `--` | Emits optimized floating-point operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f:2016 | 2026-06-25 |
| em-compile-ops | `--` | Chains factored optimized arithmetic, shuffle, comparison, unary, and float operator groups. | `test/run.f` | src/habu/habu2.f:2024 | 2026-06-25 |
| em-compile-call | `--` | Emits compile-mode lookup, immediate execution, and call generation. | `test/run.f` | src/habu/habu2.f:2040 | 2026-06-25 |
| em-reset-compile-state | `--` | Emits reset of compile/repl/evaluate state cells after rollback or recovery. | `test/run.f` | src/habu/habu2.f:2055 | 2026-06-25 |
| em-eval-undef-rollback | `--` | Emits evaluate-frame rollback for undefined-word failures. | `test/run.f` | src/habu/habu2.f:2068 | 2026-06-25 |
| em-repl-recover | `--` | Emits REPL recovery after errors, restoring line-start compile state and stacks. | `test/run.f` | src/habu/habu2.f:2080 | 2026-06-25 |
| em-compile-undef | `--` | Emits undefined-word diagnostics and evaluate/REPL recovery routing. | `test/run.f` | src/habu/habu2.f:2092 | 2026-06-25 |
| em-eval-clean-exit | `--` | Emits clean evaluate end-of-buffer return path. | `test/run.f` | src/habu/habu2.f:2102 | 2026-06-25 |
| em-repl-read | `--` | Emits REPL line-state save, read callback call, EOF handling, and input reset. | `test/run.f` | src/habu/habu2.f:2115 | 2026-06-25 |
| em-compile-exit | `--` | Emits interpreter end-of-input handling for evaluate, REPL ok/read, and process exit. | `test/run.f` | src/habu/habu2.f:2127 | 2026-06-25 |
| em-compile | `--` | Chains the factored compile-mode dispatch, call, undefined, and exit emitters. | `test/run.f` | src/habu/habu2.f:2140 | 2026-06-25 |
| emit-main | `--` | Allocates main-loop labels and chains EM-STARTUP/COMMENT/INTERPRET/COMPILE. | `test/run.f` | src/habu/habu2.f:2145 | 2026-06-25 |
| SRCA@ | `-- ptr u8` | Reads EMIT-FORTH's saved source pointer from a raw variable for the final `BYTES,` copy. | `test/run.f` | src/habu/habu2.f:2148 | 2026-06-25 |
| emit-forth | `ptr u8 n --` | Top-level engine builder: consumes source bytes, allocates every forward-ref label, and emits the complete image. | `test/run.f` | src/habu/habu2.f:2264 | 2026-06-25 |
| IMAGE-TEXT-SIZE-OFF | `-- n` | Linux executable header offset where the snapshot writer reads the mapped text size. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:14 | 2026-06-26 |
| IMAGE-TEXT-CONTENT-ADJ | `-- n` | Linux text-size field adjustment from segment size to code-content size for snapshot streaming. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:15 | 2026-06-26 |
| IMAGE-TEXT-TRAILER-ADJ | `-- n` | Linux trailer address adjustment for snapshot restore when the text-size field includes the code offset. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:16 | 2026-06-26 |
| DATA-VA | `-- ptr a` | Linux fixed DATA virtual address used by snapshot and AOT startup writers as both cell-address and byte-span base. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:17 | 2026-06-26 |
| DATA-SIZE | `-- n` | Linux fixed DATA mapping size used by snapshot validation and image inspection. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:18 | 2026-06-26 |
| MBUF | `-- ptr u8` | Target image-builder output buffer; checked drivers write the finalized executable bytes through this audited byte span. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/image-bytes.f:21 | 2026-06-26 |
| MLEN | `-- ptr n` | Target image-builder output length cell; checked drivers read the finalized executable length after `ASM-CODE BUILD-IMAGE`/`CODESIG2`. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/image-bytes.f:9 | 2026-06-25 |
| CODE-OFF | `-- n` | Linux executable code offset used by checked snapshot streaming code. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f:19 | 2026-06-26 |
| DLOPEN-SLOT-VA | `-- va` | Linux dynamic ELF fixed GOT virtual address written into `.rela.dyn` for `dlopen`. | `test/run.f`, `test/gate-aot-positive.f` | src/os/linux/layout.f:20 | 2026-06-26 |
| DLSYM-SLOT-VA | `-- va` | Linux dynamic ELF fixed GOT virtual address written into `.rela.dyn` for `dlsym`. | `test/run.f`, `test/gate-aot-positive.f` | src/os/linux/layout.f:21 | 2026-06-26 |
| DLOPEN-SLOT | `-- ptr n` | Linux dynamic ELF fixed GOT cell where ld.so resolves `dlopen` before Habu FFI reads the function pointer. | `test/run.f` | src/os/linux/layout.f:22 | 2026-06-26 |
| DLSYM-SLOT | `-- ptr n` | Linux dynamic ELF fixed GOT cell where ld.so resolves `dlsym` before Habu FFI reads the function pointer. | `test/run.f` | src/os/linux/layout.f:23 | 2026-06-26 |
| SNAP-EXTRA-PTR | `-- ptr u8` | Linux snapshot writer stages the dynamic RW segment after the header buffer and streams it after the padded live text. | `test/run.f`, `test/engine-suite.f` | src/os/linux/elf.f:182 | 2026-06-26 |
| SNAP-EXTRA-SIZE | `-- n` | Linux snapshot writer appends the fixed-size `.dynamic` plus GOT segment after the padded text payload. | `test/run.f`, `test/engine-suite.f` | src/os/linux/elf.f:185 | 2026-06-26 |
| IMAGE-TEXT-SIZE-OFF | `-- n` | macOS executable header offset where the snapshot writer reads the mapped text size. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:10 | 2026-06-25 |
| IMAGE-TEXT-CONTENT-ADJ | `-- n` | macOS text-size field adjustment from section size to code-content size for snapshot streaming. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:11 | 2026-06-25 |
| IMAGE-TEXT-TRAILER-ADJ | `-- n` | macOS trailer address adjustment because the section size excludes the code offset. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:12 | 2026-06-25 |
| DATA-VA | `-- ptr a` | macOS fixed DATA virtual address used by snapshot and AOT startup writers as both cell-address and byte-span base. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:13 | 2026-06-25 |
| DATA-SIZE | `-- n` | macOS fixed DATA mapping size used by snapshot validation and image inspection. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:14 | 2026-06-25 |
| CODE-OFF | `-- n` | macOS executable code offset used by checked snapshot streaming code. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f:15 | 2026-06-25 |
| SNAP-EXTRA-PTR | `-- ptr u8` | macOS snapshot images have no post-text extra segment; the shared snapshot writer still needs a typed zero-length byte span. | `test/run.f`, `test/engine-suite.f` | src/os/macos/macho.f:113 | 2026-06-26 |
| SNAP-EXTRA-SIZE | `-- n` | macOS snapshot images append no target extra bytes after the padded text payload. | `test/run.f`, `test/engine-suite.f` | src/os/macos/macho.f:116 | 2026-06-26 |
| ARGC-CELL | `-- n` | Common DATA header byte offset for the process argc startup cell. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/layout.f:46 | 2026-06-26 |
| ARGV-CELL | `-- n` | Common DATA header byte offset for the process argv vector startup cell. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/layout.f:47 | 2026-06-26 |
| ENVP-CELL | `-- n` | Common DATA header byte offset for the process envp vector startup cell. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/layout.f:48 | 2026-06-26 |
| fold-entry | `n ptr a n n --` | JIT constant-fold case: emits the keyword guard then `fxt execute`s a fold handler + raw branches. | `test/run.f` | src/habu/jit.f:103 | 2026-06-16 |
| vop-entry | `n ptr a n n n --` | JIT binop case: fold-vs-emit split with two indirect xts and raw asm. | `test/run.f` | src/habu/jit.f:370 | 2026-06-16 |
| vopi-entry | `n ptr a n n n n --` | JIT binop-immediate case: fold/register/immediate split with three indirect xts and raw asm. | `test/run.f` | src/habu/jit.f:369 | 2026-06-16 |
| vshuf-entry | `n ptr a n n n --` | JIT reg-aware stack-shuffle case (dup/over/swap/drop/nip as register moves); `sxt execute` + raw asm. | `test/run.f` | src/habu/jit.f:770 | 2026-06-16 |
| vun-entry | `n ptr a n n n --` | JIT unary-op case: con-fold vs in-place reg op via `foldxt`/`emitxt` and raw asm. | `test/run.f` | src/habu/jit.f:803 | 2026-06-16 |
| c-prof-mctx>r21 | `--` | Profiler SIGALRM handler derives the target mcontext address from raw signal-entry registers. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:41 | 2026-06-25 |
| c-prof-pc>r9 | `--` | Profiler SIGALRM handler reads the target-specific saved PC field from mcontext. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:46 | 2026-06-25 |
| c-prof-sigaction-frame | `--` | Profiler builds the target kernel sigaction record directly on the generated stack. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:79 | 2026-06-25 |
| c-prof-sigaction | `--` | Profiler installs SIGALRM through the target raw sigaction syscall ABI. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:85 | 2026-06-25 |
| c-prof-sigaction-done | `--` | Profiler releases the generated sigaction stack scratch frame. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:89 | 2026-06-25 |
| c-prof-timer-frame | `--` | Profiler builds the target itimerval record directly on the generated stack. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:95 | 2026-06-25 |
| c-prof-timer | `--` | Profiler arms the interval timer through the raw setitimer syscall ABI. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:99 | 2026-06-25 |
| c-prof-timer-done | `--` | Profiler releases the generated timer stack scratch frame. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f:103 | 2026-06-25 |
| emit-prof-prims | `--` | Emits the sampling-profiler prims as raw asm via FPRIM-L. | `test/run.f`, `test/gate-debug.f` | src/habu/prof.f:124 | 2026-06-25 |
| BI | `R a [ R a -- R b ] [ R b a -- R b c ] -- R b c` | Preserves one quotation while executing another; expressing that directly would require recursive quotation types. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:16 | 2026-06-16 |
| TRI | `R a [ R a -- R b ] [ R b a -- R b c ] [ R b c a -- R b c d ] -- R b c d` | Preserves later quotations while executing earlier ones; expressing that directly would require recursive quotation types. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:20 | 2026-06-16 |
| TIMES | `R i64 [ R -- R ] -- R` | Counted loop keeps the quotation available across repeated `execute`; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:24 | 2026-06-16 |
| EACH | `R ptr a i64 [ R a -- R ] -- R` | Array iterator keeps the quotation across element calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:28 | 2026-06-16 |
| MAP | `R ptr a i64 [ R a -- R a ] -- R` | Array map keeps the quotation across element calls and mutates cells in place; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:32 | 2026-06-16 |
| FOLD | `R ptr a i64 b [ R b a -- R b ] -- R b` | Array fold keeps the quotation across accumulator calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:36 | 2026-06-16 |
| VEC-EACH | `R ptr a [ R idx a -- R ] -- R` | Body checks, but TRUST pins the public higher-order callback scheme because the recorder does not persist this inferred quotation effect for later callers. | `lib/vector-test.f`, `test/run.f` | lib/vector.f:172 | 2026-06-24 |
| >IDX | `n -- idx` | Runtime identity cast from a generic cell to the nominal index role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:7 | 2026-06-26 |
| IDX>N | `idx -- n` | Runtime identity cast from the nominal index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:8 | 2026-06-26 |
| >LEN | `n -- len` | Runtime identity cast from a generic cell to the nominal length role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:10 | 2026-06-26 |
| LEN>N | `len -- n` | Runtime identity cast from the nominal length role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:11 | 2026-06-26 |
| >COUNT | `n -- count` | Runtime identity cast from a generic cell to the nominal count role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:13 | 2026-06-26 |
| COUNT>N | `count -- n` | Runtime identity cast from the nominal count role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:14 | 2026-06-26 |
| >OFF | `n -- off` | Runtime identity cast from a generic cell to the nominal offset role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:16 | 2026-06-26 |
| OFF>N | `off -- n` | Runtime identity cast from the nominal offset role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:17 | 2026-06-26 |
| >FD | `n -- fd` | Runtime identity cast from a generic cell to the nominal file-descriptor role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:19 | 2026-06-26 |
| FD>N | `fd -- n` | Runtime identity cast from the nominal file-descriptor role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:20 | 2026-06-26 |
| >RC | `n -- rc` | Runtime identity cast from a generic cell to the nominal return-code role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:22 | 2026-06-26 |
| RC>N | `rc -- n` | Runtime identity cast from the nominal return-code role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:23 | 2026-06-26 |
| >PID | `n -- pid` | Runtime identity cast from a generic cell to the nominal process-id role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:25 | 2026-06-26 |
| PID>N | `pid -- n` | Runtime identity cast from the nominal process-id role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:26 | 2026-06-26 |
| >MS | `n -- ms` | Runtime identity cast from a generic cell to the nominal millisecond role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:28 | 2026-06-26 |
| MS>N | `ms -- n` | Runtime identity cast from the nominal millisecond role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:29 | 2026-06-26 |
| >NS | `n -- ns` | Runtime identity cast from a generic cell to the nominal nanosecond role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:31 | 2026-06-26 |
| NS>N | `ns -- n` | Runtime identity cast from the nominal nanosecond role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:32 | 2026-06-26 |
| >TOK | `n -- tok` | Runtime identity cast from a generic cell to the nominal token-index role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:34 | 2026-06-26 |
| TOK>N | `tok -- n` | Runtime identity cast from the nominal token-index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:35 | 2026-06-26 |
| >REG | `n -- reg` | Runtime identity cast from a generic cell to the nominal register role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f:37 | 2026-06-26 |
| REG>N | `reg -- n` | Runtime identity cast from the nominal register role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f:38 | 2026-06-26 |
| >LABEL | `n -- label` | Runtime identity cast from a generic cell to the nominal code-label role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f:40 | 2026-06-26 |
| LABEL>N | `label -- n` | Runtime identity cast from the nominal code-label role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f:41 | 2026-06-26 |
| >VA | `n -- va` | Runtime identity cast from a generic cell to the nominal virtual-address role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f:43 | 2026-06-26 |
| VA>N | `va -- n` | Runtime identity cast from the nominal virtual-address role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f:44 | 2026-06-26 |
| >SYMIDX | `n -- symidx` | Runtime identity cast from a generic cell to the nominal dynamic-symbol-index role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f:46 | 2026-06-26 |
| SYMIDX>N | `symidx -- n` | Runtime identity cast from the nominal dynamic-symbol-index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f:47 | 2026-06-26 |
| >ASM | `n -- asm` | Runtime identity cast from a generic cell to the nominal assembled-code phase token; the checker cannot infer phase-token refinement from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f:49 | 2026-06-26 |
| ASM>N | `asm -- n` | Runtime identity cast from the nominal assembled-code phase token back to a generic cell; the checker cannot infer phase-token erasure from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f:50 | 2026-06-26 |
| >IMG | `n -- img` | Runtime identity cast from a generic cell to the nominal executable-image phase token; the checker cannot infer phase-token refinement from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f:52 | 2026-06-26 |
| IMG>N | `img -- n` | Runtime identity cast from the nominal executable-image phase token back to a generic cell; the checker cannot infer phase-token erasure from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f:53 | 2026-06-26 |
| >SNAP | `n -- snap` | Runtime identity cast from a generic cell to the nominal snapshot-header phase token; the checker cannot infer phase-token refinement from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f:55 | 2026-06-26 |
| SNAP>N | `snap -- n` | Runtime identity cast from the nominal snapshot-header phase token back to a generic cell; the checker cannot infer phase-token erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:56 | 2026-06-26 |
| TTHROWS-RAW | `a n --` | Top-level test assertion boundary around execution-token `catch`; checked colon definitions should use `TTHROWSQ`, but top-level scripts cannot push `[: ;]` quotations. | `lib/test-test.f`, `test/run.f` | lib/test.f:65 | 2026-06-22 |
| P>N | `ptr a -- n` | FFI argument marshalling: reinterpret any pointer as the raw integer cell the AAPCS64 trampoline loads into x0-x7; the checker has no pointer-to-cell coercion. | `lib/ffi-test.f`, `test/gate-stdlib.f` | lib/ffi.f:29 | 2026-06-26 |
| N>P | `n -- ptr u8` | FFI return marshalling: reinterpret an integer return cell (a handle or pointer from dlopen/dlsym or a callee) as a byte pointer. | `lib/ffi-test.f`, `test/gate-stdlib.f` | lib/ffi.f:30 | 2026-06-26 |
| T{ | `--` | Array-test stack assertion DSL snapshots the live data-stack depth; the checker cannot express an arbitrary test stack tail in a reusable assertion word. | `lib/array-test.f`, `test/run.f` | lib/array-test.f:35 | 2026-06-22 |
| -> | `R --` | Array-test stack assertion DSL drains an arbitrary expected stack tail into a scratch buffer for later comparison. | `lib/array-test.f`, `test/run.f` | lib/array-test.f:38 | 2026-06-22 |
| }T | `R --` | Array-test stack assertion DSL compares an arbitrary actual stack tail against the expected scratch buffer. | `lib/array-test.f`, `test/run.f` | lib/array-test.f:44 | 2026-06-22 |
| BUILD-CHECK-RAW | `ptr u8 n -- n` | Build helper boundary around `CHECK!`; the checker cannot certify a source definition by evaluating its own checker recursively. | `lib/build-test.f`, `test/run.f` | lib/build.f:140 | 2026-06-18 |
| T-CHECK-REJECTS | `ptr u8 n --` | Engine-suite negative checker assertion temporarily suppresses diagnostics and calls `CHECK!`; recursive checker invocation and raw `DIAGXT` mutation are a tested boundary. | `test/engine-suite.f`, `test/run.f` | test/engine-suite.f:114 | 2026-06-24 |
| AT-CHECK-REJECTS | `ptr u8 n --` | Array negative checker assertion temporarily suppresses diagnostics and calls `CHECK!` to prove role-confusion fixtures reject. | `lib/array-test.f`, `test/run.f` | lib/array-test.f:66 | 2026-06-26 |
| VECT-CHECK-REJECTS | `ptr u8 n --` | Vector negative checker assertion temporarily suppresses diagnostics and calls `CHECK!` to prove `len`/`idx` and `count`/`len` swaps reject. | `lib/vector-test.f`, `test/run.f` | lib/vector-test.f:16 | 2026-06-26 |
| STR-CHECK-REJECTS | `ptr u8 n --` | String negative checker assertion temporarily suppresses diagnostics and calls `CHECK!` to prove typed byte-pointer and append/copy helpers reject wrong roles. | `lib/string-test.f`, `test/run.f` | lib/string-test.f:39 | 2026-06-26 |
| JWT-CHECK-REJECTS | `ptr u8 n --` | JSON-writer negative checker assertion temporarily suppresses diagnostics and calls `CHECK!` to prove typed raw append rejects offsets as lengths. | `lib/json-write-test.f`, `test/run.f` | lib/json-write-test.f:33 | 2026-06-26 |
| IBT-CHECK-REJECTS | `ptr u8 n --` | Image-byte negative checker assertion temporarily suppresses diagnostics and calls `CHECK!` to prove image offset/length role swaps reject. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f:8 | 2026-06-26 |
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
| ACT-CHECK-REJECTS | `ptr u8 n --` | ARM64 checked-encoder negative fixture suppresses recursive checker diagnostics and asserts role-confusion snippets reject. | `tools/asm-checked-test.f`, `test/run.f` | tools/asm-checked-test.f:10 | 2026-06-26 |
| MEM-ALLOC-PTR | `n -- ptr u8` | Refines a raw anonymous `mmap` result into a typed byte pointer after size validation and `-1` failure checking; the checker cannot express this syscall-result refinement yet. | `lib/memory-test.f`, `test/run.f` | lib/memory.f:50 | 2026-06-21 |
| IMG-MMAP-PTR | `n -- ptr u8` | Refines a raw file-backed `mmap` result into a typed byte pointer after checking the `-1` failure result; the checker cannot express syscall-result refinement yet. | `tools/imgdump-test.f`, `test/run.f` | tools/imgdump.f:34 | 2026-06-25 |
| JW-BUF | `-- ptr u8` | Reads the JSON writer's OS-backed output buffer pointer stored in a raw variable after capacity allocation. | `lib/json-write-test.f`, `test/run.f` | lib/json-write.f:32 | 2026-06-22 |
| CODE | `-- ptr u8` | Lazily maps the assembler output buffer outside DATA and refines the raw mmap result to the byte pointer used by `EMITW`, `BYTES,`, and image writers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/arch/arm64/icode.f:22 | 2026-06-26 |
| ICODE-TABS | `-- ptr n` | Lazily maps the assembler label/fixup table block outside DATA and refines the raw mmap result to the numeric-cell pointer used by `LBLP`/`FXS`/`FXL`/`FXK`. | `test/run.f`, `tools/build-fixpoint-test.f` | src/arch/arm64/icode.f:31 | 2026-06-26 |
| ENV-DATA | `-- ptr n` | Returns the fixed engine data-region header pointer used for argc/argv/envp cells. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:7 | 2026-06-25 |
| ENV-DATA | `-- ptr n` | Returns the fixed engine data-region header pointer used for argc/argv/envp cells. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:7 | 2026-06-25 |
| ENV-DASH | `-- n` | Shared ASCII dash byte constant used by pre-hook argv parsing helpers. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:8 | 2026-06-26 |
| ENV-DASH | `-- n` | Shared ASCII dash byte constant used by pre-hook argv parsing helpers. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:8 | 2026-06-26 |
| ARGC | `-- n` | Reads the process argc value captured by the native startup entry. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:11 | 2026-06-26 |
| ARGC | `-- n` | Reads the process argc value captured by the native startup entry. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:11 | 2026-06-26 |
| ARGV-BASE | `-- ptr ptr u8` | Refines the raw argv vector pointer read from the engine startup byte-offset cell. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:13 | 2026-06-26 |
| ARGV-BASE | `-- ptr ptr u8` | Refines the raw argv vector pointer read from the engine startup byte-offset cell. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:13 | 2026-06-26 |
| ENVP-BASE | `-- ptr ptr u8` | Refines the raw envp vector pointer read from the engine startup byte-offset cell. | `test/run.f`, `test/engine-suite.f` | src/os/macos/env.f:18 | 2026-06-26 |
| ENVP-BASE | `-- ptr ptr u8` | Refines the raw envp vector pointer read from the engine startup byte-offset cell. | `test/run.f`, `test/engine-suite.f` | src/os/linux/env.f:18 | 2026-06-26 |
| ZLEN | `ptr u8 -- n` | Measures a NUL-terminated byte string read from argv or envp. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:25 | 2026-06-26 |
| ZLEN | `ptr u8 -- n` | Measures a NUL-terminated byte string read from argv or envp. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:25 | 2026-06-26 |
| ARGV$ | `n -- ptr u8 n` | Converts one argv c-string pointer to a counted byte string. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:28 | 2026-06-26 |
| ARGV$ | `n -- ptr u8 n` | Converts one argv c-string pointer to a counted byte string. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:28 | 2026-06-26 |
| ENV-FALSE | `-- bool` | Produces a typed false flag for pre-hook env parsing helpers. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:31 | 2026-06-26 |
| ENV-FALSE | `-- bool` | Produces a typed false flag for pre-hook env parsing helpers. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:31 | 2026-06-26 |
| SCRIPT-LOAD-Z? | `ptr u8 -- bool` | Recognizes the `--load` argv marker in a raw argv c-string. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:41 | 2026-06-26 |
| SCRIPT-LOAD-Z? | `ptr u8 -- bool` | Recognizes the `--load` argv marker in a raw argv c-string. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:41 | 2026-06-26 |
| SCRIPT-LOAD? | `-- bool` | Detects source-list mode from captured process argv. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:46 | 2026-06-26 |
| SCRIPT-LOAD? | `-- bool` | Detects source-list mode from captured process argv. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:46 | 2026-06-26 |
| SCRIPT-SEP? | `n -- bool` | Recognizes the `--` argv separator in source-list mode. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:53 | 2026-06-26 |
| SCRIPT-SEP? | `n -- bool` | Recognizes the `--` argv separator in source-list mode. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:53 | 2026-06-26 |
| SCRIPT-ARG-START | `-- n` | Computes the first user script argument after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:62 | 2026-06-26 |
| SCRIPT-ARG-START | `-- n` | Computes the first user script argument after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:62 | 2026-06-26 |
| SCRIPT-ARGC | `-- n` | Returns user script argument count after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:66 | 2026-06-26 |
| SCRIPT-ARGC | `-- n` | Returns user script argument count after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:66 | 2026-06-26 |
| SCRIPT-ARGV | `n -- ptr u8` | Returns one user script argv c-string after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:69 | 2026-06-26 |
| SCRIPT-ARGV | `n -- ptr u8` | Returns one user script argv c-string after source-list handling. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:69 | 2026-06-26 |
| SCRIPT-ARGV$ | `n -- ptr u8 n` | Returns one user script argument as counted bytes. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:72 | 2026-06-26 |
| SCRIPT-ARGV$ | `n -- ptr u8 n` | Returns one user script argument as counted bytes. | `test/run.f`, `tools/argv-test.f` | src/os/linux/env.f:72 | 2026-06-26 |
| ENV=? | `ptr u8 ptr u8 n -- bool` | Compares one envp c-string against a counted variable name and following `=`. | `lib/process-env-test.f`, `test/run.f` | src/os/macos/env.f:78 | 2026-06-26 |
| ENV=? | `ptr u8 ptr u8 n -- bool` | Compares one envp c-string against a counted variable name and following `=`. | `lib/process-env-test.f`, `test/run.f` | src/os/linux/env.f:77 | 2026-06-26 |
| NULL$ | `-- ptr u8 n` | Returns a typed empty string pair used for absent environment values. | `test/run.f`, `test/engine-suite.f` | src/os/macos/env.f:80 | 2026-06-26 |
| NULL$ | `-- ptr u8 n` | Returns a typed empty string pair used for absent environment values. | `test/run.f`, `test/engine-suite.f` | src/os/linux/env.f:79 | 2026-06-26 |
| GETENV | `ptr u8 n -- ptr u8 n` | Returns a counted environment value by scanning the raw envp vector captured at startup. | `lib/process-env-test.f`, `test/run.f` | src/os/macos/env.f:91 | 2026-06-26 |
| GETENV | `ptr u8 n -- ptr u8 n` | Returns a counted environment value by scanning the raw envp vector captured at startup. | `lib/process-env-test.f`, `test/run.f` | src/os/linux/env.f:88 | 2026-06-26 |
| TMP-PATH-CAP | `-- n` | Fixed scratch capacity for target temp-path construction during pre-hook build drivers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/env.f:95 | 2026-06-26 |
| TMP-PATH-CAP | `-- n` | Fixed scratch capacity for target temp-path construction during pre-hook build drivers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/env.f:91 | 2026-06-26 |
| TPP@ | `-- ptr u8` | Reads the temporary-path scratch cursor from a raw variable. | `test/run.f` | src/os/macos/env.f:100 | 2026-06-26 |
| TPP@ | `-- ptr u8` | Reads the temporary-path scratch cursor from a raw variable. | `test/run.f` | src/os/linux/env.f:96 | 2026-06-26 |
| TMP-PATH-CHECK | `n --` | Validates the fixed target path scratch capacity before raw byte copies. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/env.f:104 | 2026-06-26 |
| TMP-PATH-CHECK | `n --` | Validates the fixed target path scratch capacity before raw byte copies. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/env.f:100 | 2026-06-26 |
| TMP-PATH | `ptr u8 n -- ptr u8 n` | Builds `$HB_TMP` or `/tmp` child paths in pre-hook engine build drivers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/env.f:113 | 2026-06-26 |
| TMP-PATH | `ptr u8 n -- ptr u8 n` | Builds `$HB_TMP` or `/tmp` child paths in pre-hook engine build drivers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/env.f:109 | 2026-06-26 |
| SHAKE? | `-- ptr n` | Treeshaker enable flag cell is a raw variable; checked scanner code needs its cell type pinned before using `@`/`!`. | `test/run.f` | src/habu/treeshake.f:9 | 2026-06-26 |
| SHK-A | `-- ptr ptr u8` | Treeshaker source-buffer pointer cell is a raw variable; checked scanner code stores and reads a byte pointer through it. | `test/run.f` | src/habu/treeshake.f:10 | 2026-06-26 |
| SHK-U | `-- ptr n` | Treeshaker source length cell is a raw variable used by checked scanner bounds tests. | `test/run.f` | src/habu/treeshake.f:11 | 2026-06-26 |
| SKP | `-- ptr n` | Treeshaker scan cursor cell is a raw variable used by checked token scanning. | `test/run.f` | src/habu/treeshake.f:12 | 2026-06-26 |
| STS | `-- ptr n` | Treeshaker token-start cell is a raw variable used by checked token scanning. | `test/run.f` | src/habu/treeshake.f:13 | 2026-06-26 |
| SHK-A@ | `-- ptr u8` | Reads the treeshaker source-buffer pointer stored in a raw variable. | `test/run.f` | src/habu/treeshake.f:15 | 2026-06-16 |
| REACHN | `-- ptr n` | Treeshaker reachability-buffer length cell is a raw variable used by checked reachability scanning. | `test/run.f` | src/habu/treeshake.f:58 | 2026-06-26 |
| TKP | `-- ptr n` | Treeshaker tokenizer cursor cell is a raw variable used by checked token scanning. | `test/run.f` | src/habu/treeshake.f:59 | 2026-06-26 |
| CHG | `-- ptr bool` | Treeshaker fixpoint-change flag cell is a raw variable used by checked reachability iteration. | `test/run.f` | src/habu/treeshake.f:60 | 2026-06-26 |
| INDEF | `-- ptr bool` | Treeshaker in-definition flag cell is a raw variable used by checked source scanning. | `test/run.f` | src/habu/treeshake.f:61 | 2026-06-26 |
| XNAME | `-- ptr bool` | Treeshaker expecting-definition-name flag cell is a raw variable used by checked source scanning. | `test/run.f` | src/habu/treeshake.f:62 | 2026-06-26 |
| KEEPCUR | `-- ptr bool` | Treeshaker keep-current-definition flag cell is a raw variable used by checked reachability expansion. | `test/run.f` | src/habu/treeshake.f:63 | 2026-06-26 |
| RSP | `-- ptr n` | Treeshaker reachability scan cursor cell is a raw variable used by checked reachability scanning. | `test/run.f` | src/habu/treeshake.f:64 | 2026-06-26 |
| RTS | `-- ptr n` | Treeshaker reachability-token-start cell is a raw variable used by checked reachability scanning. | `test/run.f` | src/habu/treeshake.f:65 | 2026-06-26 |
| TA | `-- ptr ptr u8` | Treeshaker current-token pointer cell is a raw variable used by checked scanner code. | `test/run.f` | src/habu/treeshake.f:66 | 2026-06-26 |
| TU | `-- ptr n` | Treeshaker current-token length cell is a raw variable used by checked scanner code. | `test/run.f` | src/habu/treeshake.f:67 | 2026-06-26 |
| TA@ | `-- ptr u8` | Reads the current treeshaker token pointer stored in a raw variable. | `test/run.f` | src/habu/treeshake.f:69 | 2026-06-16 |
| MP@ | `-- ptr u8` | Reads the shared image output cursor stored in a raw variable. | `test/run.f` | src/os/image-bytes.f:24 | 2026-06-26 |
| SIGA@ | `-- ptr u8` | Reads the code-signing identifier pointer stored in a raw variable. | `test/run.f` | src/os/macos/sign2.f:8 | 2026-06-16 |
| HB@ | `-- ptr u8` | Reads the stdin-engine baked-source buffer pointer stored in a raw variable. | `test/run.f` | src/habu/stdin.f:19 | 2026-06-16 |
| BLD-PB@ | `-- ptr u8` | Reads the standalone-build source buffer pointer stored in a raw variable. | `test/run.f`, `tools/hb-build.f` | src/habu/build.f:22 | 2026-06-24 |
| V-TRUST-SIG | `ptr u8 n ptr u8 n --` | hb-build pre-verifier records source-order defining-word signatures for parsed names; the checker cannot infer a dynamic mutation of its signature table from scanner state. | `tools/hb-build-test.f`, `test/run.f` | src/habu/build.f:136 | 2026-06-24 |
| AOT-PB@ | `-- ptr u8` | Reads the AOT build source buffer pointer stored in a raw variable. | `test/run.f`, `tools/hb-build.f` | src/habu/aot.f:19 | 2026-06-24 |
| AOT-DBASE@ | `-- ptr a` | Reads the runtime dictionary base pointer for AOT dictionary-record scans; record fields are mixed, so callers specialize the pointee type at each access. | `test/run.f`, `tools/hb-build.f` | src/habu/aot.f:21 | 2026-06-24 |
| AOT-PTR@ | `ptr a -- ptr a` | Reads a dictionary long-name pointer field whose pointee is another address; the checker cannot express this pointer-to-pointer load yet. | `test/run.f`, `tools/hb-build.f` | src/habu/aot.f:24 | 2026-06-24 |
| MK-SBUF@ | `-- ptr u8` | Reads the hb-build maker source buffer pointer stored in a raw variable while compiling the separate maker image. | `tools/hb-build-test.f`, `test/run.f` | src/habu/maker.f:25 | 2026-06-24 |
| STB@ | `-- ptr u8` | Reads the snapshot source text base pointer stored in a raw variable. | `test/run.f`, `tools/build-fixpoint.f snap` | src/habu/snap.f:16 | 2026-06-26 |
| STB-CELL@ | `-- ptr n` | Reads the snapshot source text base pointer as a cell-address for executable-header size lookup. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/snap.f:18 | 2026-06-26 |
| SDB@ | `-- ptr u8` | Reads the snapshot dictionary/data-region pointer stored in a raw variable. | `test/run.f`, `tools/build-fixpoint.f snap` | src/habu/snap.f:20 | 2026-06-26 |
| SNAP-CHECK-HOOK | `ptr u8 n -- n` | Snapshot image installs the fail-closed checker hook into the emitted image; recursive `CHECK!` hook bodies are trusted boundaries. | `test/run.f`, `test/gate-debug.f` | src/habu/snap.f:79 | 2026-06-26 |
| SNAP-INSTALL-HOOK | `--` | Snapshot image mutates the checker hook cell only through a named trusted installer. | `test/run.f`, `test/gate-debug.f` | src/habu/snap.f:81 | 2026-06-26 |
| S2-PATH-CAP | `-- n` | Fixed path-buffer capacity for the stage2 fixpoint driver. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/stage2.f:8 | 2026-06-26 |
| S2-PATH-BUF | `-- ptr u8` | Stage2 fixpoint path scratch buffer used while building private artifact paths. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/stage2.f:10 | 2026-06-26 |
| SBUF@ | `-- ptr u8` | Reads the stage2 source buffer pointer stored in a raw variable. | `test/run.f` | src/habu/stage2.f:36 | 2026-06-26 |
| BFT-READ-BUF | `-- ptr u8` | Reads the build-fixpoint fixture's mapped source-read buffer pointer after allocation. | `tools/build-fixpoint-test.f`, `test/run.f` | tools/build-fixpoint-test.f:28 | 2026-06-26 |
| GT-POOL-OUT-BUFS | `-- ptr u8` | Reads the gate pool's mapped stdout capture slab after allocation. | `test/run.f` | test/gate-pool.f:39 | 2026-06-26 |
| GT-POOL-ERR-BUFS | `-- ptr u8` | Reads the gate pool's mapped stderr capture slab after allocation. | `test/run.f` | test/gate-pool.f:43 | 2026-06-26 |
| IMGD-MMAP-PTR | `n -- ptr u8` | Converts the raw image mmap result into a typed byte pointer after checking mmap failure; OS mapping pointers are outside checker inference. | `tools/imagedisasm-test.f`, `test/run.f` | tools/imagedisasm.f:45 | 2026-06-25 |
