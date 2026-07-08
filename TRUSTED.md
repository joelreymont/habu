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
| STDIN? | `-- ptr bool` | Engine-builder mode cell that checked drivers set before emitting stdin or file-backed startup behavior. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/habu1.f | 2026-06-26 |
| fprim | `ptr u8 n n --` | Raw-asm prim emitter: lays a REG-PRIM frame + `xt execute`s a code-emitting handler; no Forth effect to infer. | `test/run.f` | src/habu/habu1.f | 2026-06-24 |
| fprim-l | `ptr u8 n n --` | Leaf variant of FPRIM (no x30 frame); same `xt execute` of a code emitter. | `test/run.f` | src/habu/habu1.f | 2026-06-24 |
| linux-spawn-fail | `reg --` | Linux child-side spawn failure reporter: consumes the target register holding the exec-error pipe, emits raw `write`, and exits the child without returning to Forth. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-26 |
| linux-dup2-fd | `reg fd reg --` | Linux child-side raw syscall emitter for conditional `dup2`: source fd register, destination fd immediate, and exec-error-pipe register are role-typed; raw label/syscall code remains the boundary. | `lib/process-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f | 2026-06-26 |
| linux-chdir-fd | `reg reg --` | Linux child-side raw syscall emitter for optional `chdir`: cwd pointer register and exec-error-pipe register are role-typed; raw label/syscall code remains the boundary. | `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-26 |
| linux-setpgid-self | `reg --` | Linux child-side raw syscall emitter for fail-closed `setpgid(0,0)` before cwd/stdio/exec setup; the exec-error-pipe register is passed through the failure reporter boundary. | `lib/process-test.f`, `lib/process-env-test.f`, `test/gate-pool-test.f`, `test/run.f` | src/habu/habu1.f | 2026-07-03 |
| linux-spawn-close-r | `--` | Linux spawn emitter helper that closes the parent/child error-pipe read fd from the raw stack frame. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-24 |
| linux-spawn-close-w | `--` | Linux spawn emitter helper that closes the parent/child error-pipe write fd from the raw stack frame. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-24 |
| linux-spawn-close-pipe | `--` | Linux spawn emitter helper that closes both error-pipe fds on parent-side setup failure. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-24 |
| linux-spawn-prep-w | `--` | Linux spawn emitter helper that keeps the child failure-report fd close-on-exec and duplicates it above stdio when needed. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-24 |
| linux-spawn-wait-stored | `--` | Linux spawn emitter helper that reaps the stored child pid after setup or exec failure so failed spawns leave no waitable child behind. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-24 |
| linux-spawn-parent | `--` | Linux parent-side spawn handshake: reads the exec-error pipe and returns pid or `-1` through x9; raw fd/syscall/register effects are outside Forth inference. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-24 |
| linux-spawn-child | `--` | Linux child-side spawn setup: applies cwd/stdio setup, performs raw `execve`, and reports setup/exec failure through the error pipe. | `lib/process-test.f`, `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-24 |
| linux-spawn | `reg reg reg reg reg reg reg --` | Linux spawn emitter consumes target registers for path, argv, envp, cwd, stdin, stdout, and stderr; syscall/control-flow effects remain the boundary. | `lib/process-test.f`, `lib/process-cwd-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f | 2026-06-26 |
| linux-ignore-sigpipe | `--` | Linux raw `rt_sigaction` emitter for SIGPIPE ignore used to implement the no-SIGPIPE process fd abstraction. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-24 |
| spawn-dup2-action | `reg fd --` | Build-side helper that emits one raw XNU `PSFA_DUP2` action from a target fd register to a destination fd immediate; raw record layout remains the boundary. | `tools/spawn-emitter-test.f`, `test/proc-pty.f`, `test/engine-suite.f` | src/habu/habu1.f | 2026-06-26 |
| spawn-chdir-action | `reg label --` | Build-side helper that emits one raw XNU `PSFA_CHDIR` action from a cwd pointer register and branches to a caller failure label; raw record layout remains the boundary. | `tools/spawn-emitter-test.f`, `lib/process-cwd-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f | 2026-06-26 |
| spawn-darwin-frame3-enter | `--` | Build-side helper that emits the shared three-action Darwin spawn runtime stack-frame allocation. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-25 |
| spawn-darwin-frame3-leave | `--` | Build-side helper that emits the shared three-action Darwin spawn runtime stack-frame release. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-25 |
| spawn-darwin-frame4-enter | `--` | Build-side helper that emits the extended Darwin spawn runtime stack-frame allocation used by cwd actions. | `tools/spawn-emitter-test.f`, `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-25 |
| spawn-darwin-frame4-leave | `--` | Build-side helper that emits the extended Darwin spawn runtime stack-frame release used by cwd actions. | `tools/spawn-emitter-test.f`, `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-25 |
| spawn-darwin-actions-reset | `count --` | Build-side helper that initializes the XNU file-action blob header at x13 for the requested action count and zero used-count. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-26 |
| spawn-darwin-stdio-actions | `--` | Build-side helper that appends the three conditional stdio `PSFA_DUP2` actions through the audited dup2 action emitter. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-25 |
| spawn-darwin-zero-adesc | `--` | Build-side helper that emits zeroing stores for the Darwin spawn descriptor area; raw descriptor layout is outside Forth inference. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-25 |
| spawn-darwin-zero-attr | `--` | Build-side helper that emits zeroing stores for the Darwin `posix_spawn` attribute area; the XNU-private layout is outside Forth inference. | `tools/spawn-emitter-test.f`, `lib/process-env-test.f`, `test/run.f` | src/habu/habu1.f | 2026-07-03 |
| spawn-darwin-attr-defaults | `--` | Build-side helper that emits `POSIX_SPAWN_SETPGROUP` plus XNU default attribute fields so each spawned child becomes its own process-group leader before exec. | `tools/spawn-emitter-test.f`, `lib/process-env-test.f`, `test/gate-pool-test.f`, `test/run.f` | src/habu/habu1.f | 2026-07-03 |
| spawn-darwin-fill-adesc | `--` | Build-side helper that emits the XNU spawn descriptor attribute pointer/size and, when present, file-action pointer/size from the runtime action count. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `lib/process-env-test.f`, `test/run.f` | src/habu/habu1.f | 2026-07-03 |
| spawn-darwin-use-adesc | `--` | Build-side helper that emits the non-null descriptor pointer for every Darwin spawn so process-group attributes are applied even when no file actions are needed. | `tools/spawn-emitter-test.f`, `lib/process-cwd-test.f`, `lib/process-env-test.f`, `test/run.f` | src/habu/habu1.f | 2026-07-03 |
| spawn-darwin-pid-path | `reg --` | Build-side helper that emits common `posix_spawn` pid-out and path register setup. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f | 2026-06-26 |
| spawn-darwin-argv-envp | `reg reg --` | Build-side helper that emits common argv/envp register setup when both vectors are runtime input registers. | `tools/spawn-emitter-test.f`, `lib/process-env-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f | 2026-06-26 |
| spawn-darwin-default-argv-envp | `reg --` | Build-side helper that emits the default argv/envp runtime stack vectors for path-only spawn. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-26 |
| spawn-darwin-default-envp | `--` | Build-side helper that emits the default empty envp runtime stack vector for argv spawn. | `tools/spawn-emitter-test.f`, `lib/process-argv-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-25 |
| spawn-darwin-use-default-argv-envp | `--` | Build-side helper that emits `posix_spawn` argv/envp argument registers for the path-only default vectors. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-25 |
| spawn-darwin-argv-default-envp | `reg --` | Build-side helper that emits `posix_spawn` argv input plus default empty envp argument registers. | `tools/spawn-emitter-test.f`, `lib/process-argv-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-26 |
| spawn-darwin-finish | `label label --` | Build-side helper that emits shared Darwin `posix_spawn` syscall, preserves failure errno as a negative pid code, loads the child pid on success, joins failure/success labels, and pushes the raw result. | `tools/spawn-emitter-test.f`, `lib/process-test.f`, `lib/process-cwd-test.f`, `test/run.f`, `test/engine-suite.f` | src/habu/habu1.f | 2026-06-29 |
| linux-stat-fix | `n --` | Linux stat syscall layout shim copies the kernel `mode` and `size` fields into the engine's portable `stat64` offsets; raw field writes are outside checker inference. | `lib/fs-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-27 |
| emit-prims | `--` | Emits the engine's whole primitive table as raw ARM64. | `test/run.f` | src/habu/habu1.f | 2026-06-27 |
| emit-fp-prims | `--` | Emits the floating-point prim table as raw asm via FPRIM-L. | `test/run.f` | src/habu/habu1.f | 2026-06-27 |
| BPW-TAB | `-- ptr ptr n` | Watch-table storage is dictionary data whose cells hold watched DATA pointers; the checker cannot infer this created table's pointee role. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug-watch.f | 2026-06-25 |
| BPW-PRINT-ADDR | `ptr n --` | Debug watch printer intentionally displays a raw cell address; formatting a pointer through `.` is a REPL/debug boundary. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug-watch.f | 2026-06-25 |
| BPW-DATA-CELL | `n -- ptr n` | Converts a fixed DATA cell offset to a typed numeric-cell address for watch registration. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug-watch.f | 2026-06-25 |
| BP-SLOT-ADDR | `n -- ptr ptr u8` | Breakpoint slots live in fixed DATA and store code pointers; slot field typing is outside arithmetic inference. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 |
| BP-SLOT-INSTR | `n -- ptr n` | Breakpoint slots store the saved 32-bit instruction word in fixed DATA. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 |
| BP-SLOT-HITS | `n -- ptr n` | Breakpoint slots store hit counters in fixed DATA. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 |
| BP-SLOT-CTRL | `n -- ptr n` | Breakpoint slots store packed skip/persistent control bits in fixed DATA. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 |
| BP-NULL | `-- ptr u8` | Debug slot zero is used as a null code pointer sentinel; the checker has no null-pointer literal role. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 |
| BP-PRINT-ADDR | `ptr u8 --` | Breakpoint listing intentionally prints raw code pointers through the numeric printer. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 |
| BP-PATCH32 | `n ptr u8 --` | Breakpoint installation patches executable code with a 32-bit BRK/restored instruction; code mutation is a native debug boundary. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 |
| BP-XT>PTR | `n -- ptr u8` | A ticked xt is represented as the target code address; the checker cannot refine the cell to a code pointer. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/debug.f | 2026-06-25 |
| BFR-N>REC | `n -- ptr a` | Refresh prelude converts numeric dictionary-record addresses into opaque record pointers before truncating stale engine definitions; raw dictionary layout is the boundary. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 |
| BFR-A>U8 | `ptr a -- ptr u8` | Refresh prelude treats inline dictionary-name bytes as a byte string while finding the truncation marker. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 |
| BFR-N>U8 | `n -- ptr u8` | Refresh prelude refines the numeric long-name pointer stored in a dictionary record into a byte pointer while finding the truncation marker. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 |
| BFR-USIG-END-PTR | `-- ptr a` | Refresh prelude refines the checker signature-table terminator address so it can rewrite the reset sentinel. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 |
| BFR-UEND! | `n --` | Refresh prelude resets the checker signature cursor before reloading the current checker model; the cursor cell is checker-internal raw state. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 |
| BFR-NDICT! | `n --` | Refresh prelude mutates the live dictionary cursor after locating the reload marker; dictionary truncation is the explicit refresh boundary. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 |
| BFR-CHECK-OFF | `--` | Refresh prelude disables the currently baked checker before reloading the current checker source; the raw `set-check` token is unsafe once strict checking is active. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-28 |
| BFR-A@ | `-- ptr u8` | Refresh prelude reads a byte-string scratch pointer from a generic pointer cell while matching dictionary names. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 |
| BFR-B@ | `-- ptr u8` | Refresh prelude reads a second byte-string scratch pointer from a generic pointer cell while matching dictionary names. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 |
| BFR-SN@ | `-- ptr u8` | Refresh prelude reads the searched-name byte pointer from a generic pointer cell while finding the reload marker. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 |
| BFR-A! | `ptr u8 --` | Refresh prelude stores a byte-string scratch pointer into a generic pointer cell while matching dictionary names. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 |
| BFR-B! | `ptr u8 --` | Refresh prelude stores a second byte-string scratch pointer into a generic pointer cell while matching dictionary names. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 |
| BFR-SN! | `ptr u8 --` | Refresh prelude stores the searched-name byte pointer into a generic pointer cell while finding the reload marker. | `tools/build-fixpoint-test.f`, `tools/check-test.f`, `test/run.f` | src/habu/hide.f | 2026-07-03 |
| XREF-N>REC | `n -- ptr a` | Converts a numeric live dictionary-record address into an opaque record pointer for checked xref helpers; the record base comes from `dbase@` plus `DREC` arithmetic. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f | 2026-06-27 |
| XREF-A>U8 | `ptr a -- ptr u8` | Treats the inline-name bytes inside a dictionary record as a byte string; fixed raw record byte offsets are outside pointer-role inference. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f | 2026-06-27 |
| XREF-N>U8 | `n -- ptr u8` | Converts a numeric long-name address fetched from a dictionary record into a byte pointer; the record stores mixed numeric and pointer cells. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f | 2026-06-27 |
| XREF-PATCH32 | `n ptr a --` | Explicit `undefine` retires dictionary records by patching raw wordlist/status cells inside the live dictionary; the record layout is outside checked pointer inference. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f | 2026-06-30 |
| SEAL-LATCH@ | `-- n` | Reads the friend-arena seal latch from the sealed DATA band by raw `data-base` offset; a raw state cell (0 open / sealed) outside checked pointer/role inference. Used by the FORGET/HIDE truncation guard. | `test/seal.f`, `test/run.f` | src/habu/xref.f | 2026-07-05 |
| SEAL-NDICT@ | `-- n` | Reads the seal-time ndict truncation watermark from the sealed DATA band by raw `data-base` offset; a raw state cell outside checked inference. Used by the FORGET/HIDE truncation guard. | `test/seal.f`, `test/run.f` | src/habu/xref.f | 2026-07-05 |
| c-crash-entry | `--` | Target signal entry register shuffle is raw ABI-specific ARM64; it only mutates generated registers. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| c-crash-mctx>r21 | `--` | Target ucontext-to-mcontext addressing is ABI-specific raw register code. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| c-crash-xreg>r9 | `--` | Crash dump register extraction walks target mcontext layout in generated registers. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| c-crash-pc>r9 | `--` | Crash dump PC extraction reads target-specific mcontext fields in generated registers. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| c-crash-print-regs | `--` | Crash handler emits target-specific FP/LR/SP/PC fields through raw register/syscall code. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| c-crash-pc-word | `n --` | Crash diagnostics bounds-check a saved-PC-relative word against the fixed code mapping before raw instruction loads. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| c-crash-pc-8 | `--` | Crash diagnostics request the guarded instruction word at saved PC minus 8. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| c-crash-pc-4 | `--` | Crash diagnostics request the guarded instruction word at saved PC minus 4. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| c-crash-pc0 | `--` | Crash diagnostics request the guarded instruction word at the saved PC. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| c-crash-pc+4 | `--` | Crash diagnostics request the guarded instruction word at saved PC plus 4. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| c-trap-mctx>r9 | `--` | SIGTRAP handler target ucontext-to-mcontext addressing is raw ABI-specific ARM64. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-mctx-pc>r10 | `--` | SIGTRAP handler reads target-specific PC fields from mcontext in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-mctx-x19>r12 | `--` | SIGTRAP handler reads the target data-stack register from mcontext in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-mctx-sp-16! | `--` | Breakpoint resume emulates the compiled prologue by mutating target mcontext SP. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-mctx-pc+4! | `--` | Breakpoint resume skips the BRK instruction by mutating target mcontext PC. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-bp-hit-save | `--` | Breakpoint hit handling saves handler scratch registers and updates fixed DATA slot hit counters in generated code. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-bp-print-hit | `--` | Breakpoint hit reporting prints raw PC and stack-top values from target mcontext/register state. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-bp-stack-range | `--` | Breakpoint stack dumping derives raw stack bounds from fixed DATA and mcontext state. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-bp-watch-head | `--` | Breakpoint watch dumping reads fixed DATA watch metadata and emits a signal-safe header. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-bp-watch-row | `--` | Breakpoint watch dumping reads one raw watched pointer/value pair in generated code. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-bp-restore-oneshot | `--` | One-shot breakpoint restore mutates executable code, flips page permissions, and flushes I-cache. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-bp-emulate | `--` | Persistent/skip breakpoint resume emulates a compiled-word entry prologue in target mcontext. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-bp-scan | `label label label label --` | Breakpoint table scan emits branches to caller-provided labels and leaves hit-slot state in generated registers. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-bp-stack-dump | `label label --` | Breakpoint stack dump emits a caller-labelled loop over raw data-stack cells. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-bp-watch-dump | `label label --` | Breakpoint watch dump emits a caller-labelled loop over raw watched cells. | `test/proc-pty.f`, `test/gate-debug.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-emit-tty-probe | `--` | Startup source selection emits target-specific tty ioctl setup; the syscall/register effects are not Forth stack effects. | `test/proc-pty.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| c-emit-drop-x12 | `--` | Control-flow local-scope restore emits a raw `add sp, sp, #bytes` teardown from generated register state. | `test/engine-suite.f`, `test/run.f` | src/habu/habu2.f | 2026-06-29 |
| c-dup-def-fail | `--` | Duplicate-definition failure emitter writes the fixed diagnostic and pending definition token, then exits with the duplicate-definition code. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-reject-dup-def | `--` | Definition-publish guard scans the active target wordlist case-insensitively before dictionary mutation and branches to the duplicate-definition failure emitter. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-qualify-def | `--` | Definition-time namespace qualifier emitter: rewrites the pending token to the qualified tail, creates namespace wordlist records, and exits on malformed qualification through raw runtime code. | `test/gate-dictionary.f`, `tools/xref-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-store-def-name | `--` | Stores the pending dictionary name and qualified wordlist after `c-qualify-def`, then restores the full captured token for checker publication. | `test/gate-dictionary.f`, `tools/xref-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-defer-die-token | `n --` | Deferred-word failure emitter writes the current token and exits with the supplied execution-vector code from generated compiler/runtime paths. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| c-defer-find-unset | `--` | Deferred-word creation resolves the shared `DEFER-UNSET` sentinel xt through the raw target dictionary. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| c-defer-cell | `--` | Deferred-word creation allocates the vector cell and seeds it with the unset sentinel xt in raw target data space. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| c-defer-emit-code | `--` | Deferred-word code emitter writes the wrapper that loads the vector cell and branches to the stored implementation xt. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| c-defer-meta-write | `--` | Deferred-word publisher appends the magic marker and vector-cell address used by compile-time `is` validation. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| c-defer-room | `--` | Deferred-word creation emits raw dictionary/code-space capacity checks before mutating compiler state. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| c-defer | `--` | Interpreter `defer` definer consumes the name/signature, creates the wrapper and metadata, and publishes the declared checked effect. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| c-defer-target-meta | `--` | Compile-mode `is` validation resolves the deferred target and rejects non-deferred words before code emission. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| j-is | `--` | Compile-mode `is` emitter spills the typed quotation xt and stores it into the deferred word's vector cell. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| c-lbrace-die | `--` | Locals-placement guard writes the fixed diagnostic and exits when a `{:` local is declared inside a quotation. | `test/gate-diagnostics.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| EM-HXT-EXECUTE | `n --` | Narrow higher-order emitter boundary: checked dispatcher words pass one build-time emitter xt through this raw `execute` shim. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/habu2.f | 2026-06-26 |
| c-local-ref | `label label --` | Compile-mode local-reference emitter: branches to the caller's not-local continuation or emits local loads, and rejects quotation-local captures with raw exit code 75. | `test/engine-suite.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| EM-DATA-VA>N | `-- n` | Engine-builder raw emitter boundary: exposes the fixed DATA-VA pointer as the numeric immediate needed by `LIT64,` when emitting the startup mmap check. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/habu2.f | 2026-06-26 |
| em-interpret-colon | `label --` | Emits interpreter-mode colon handling and kernel-colon setup before falling through to word dispatch. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-find-global | `ptr n n --` | Package checker bridge resolves core checker words from the global wordlist while preserving active package cells, so package-local words cannot shadow the checker state API. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-call-checker-defer | `--` | Deferred-word keyword bridge records the published name in the checker-owned defer-target registry so `is` can reject non-defer targets statically. | `test/gate-dictionary.f`, `tools/hb-build-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| c-call-checker-package | `--` | Package keyword bridge pushes the package token to `CHECKER-PACKAGE`; raw dictionary lookup and generated call setup are outside Forth inference. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-call-checker-public | `--` | Public keyword bridge calls `CHECKER-PUBLIC` so checker signature scope follows runtime package scope. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-call-checker-private | `--` | Private keyword bridge calls `CHECKER-PRIVATE` so checker signature scope follows runtime package scope. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-call-checker-end-package | `--` | End-package keyword bridge calls `CHECKER-END-PACKAGE` before clearing runtime package cells. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-package-fail | `n --` | Package keyword failure emitter prints the current token and exits with the supplied named error code; raw process exit is outside Forth stack inference. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-package-name-guard | `--` | Package namespace-name guard scans the current token in generated registers and rejects embedded namespace separators before dictionary mutation. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-package-new-private-wid | `--` | Package reopen helper allocates one private wordlist id and stores it in the existing namespace record; register and fixed-DATA effects are raw emitter state. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-package-alloc-wids | `--` | Package creation helper allocates paired public/private wordlist ids from the fixed WID counter. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-package-new-record | `--` | Package creation helper emits a namespace dictionary record with public/private wordlist cells and leaves the record/public/private registers for the caller. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-package-existing-private | `label --` | Package reopen helper branches to the caller's done label after ensuring an existing namespace has a private wordlist id. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-package-ensure | `--` | Package keyword dictionary lookup/creation scans namespace records, reuses public wordlists, creates missing private wordlists, and leaves package ids in generated registers. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-package | `--` | Interpreter `package` keyword consumes the following token, rejects nested/malformed packages, opens private scope, syncs checker package state, and saves the parent current wordlist. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-seal-package-fail | `--` | Sealed-system-package failure emitter prints the offending token from the fixed token cells and exits `E-SEAL-PACKAGE`; raw process exit is outside Forth stack inference. | `test/seal-package.f`, `test/run.f` | src/habu/habu2.f | 2026-07-04 |
| c-seal-match | `--` | Sealed-system-package matcher scans the native reserved-name table (`RESTAB`) in generated registers, case-folds the candidate token `TKA[0,x24)`, and calls the seal failure emitter on a match. | `test/seal-package.f`, `test/run.f` | src/habu/habu2.f | 2026-07-04 |
| c-qualify-seal-guard | `--` | Definition-time seal guard: when the friend latch is closed and the pending token is a non-edge `NAME:tail`, matches the prefix against the reserved-name table and fails closed; raw latch/register scan is outside Forth stack inference. | `test/seal-package.f`, `test/run.f` | src/habu/habu2.f | 2026-07-04 |
| c-package-seal-guard | `--` | `package` keyword seal guard: when the friend latch is closed, matches the pending package name against the reserved-name table and fails closed before wordlist allocation. | `test/seal-package.f`, `test/run.f` | src/habu/habu2.f | 2026-07-04 |
| c-public | `--` | Interpreter `public` keyword switches the active package's current wordlist to the exported public wordlist and syncs checker public mode. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-private | `--` | Interpreter `private` keyword switches the active package's current wordlist back to the private wordlist and syncs checker private mode. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| c-end-package | `--` | Interpreter `end-package` keyword restores the saved parent current wordlist and clears both runtime and checker package frames. | `test/gate-dictionary.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| em-interpret-define-keywords | `--` | Emits interpreter-mode defining-word dispatch cases grouped separately from literal and lookup fallback. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-interpret-string-keywords | `--` | Emits interpreter-mode string parsing-word dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-interpret-number | `label --` | Emits interpreter-mode number parsing and branches to the caller's not-number label on failure. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| em-interpret-find | `--` | Emits interpreter-mode dictionary lookup, undefined routing, and execute dispatch. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-interpret-words | `--` | Chains the factored interpreter-mode defining, string, number, and lookup dispatch emitters. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-interpret | `--` | Chains the factored interpreter-mode colon and word-dispatch emitters. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-drop-locals | `--` | Emits optional locals-frame teardown before a compiled definition returns. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-ret | `--` | Emits the raw return epilogue for a compiled definition. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-flush-pend | `--` | Finalizes the pending dictionary entry length and flips/flushed the generated code region. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-publish-trusted | `--` | Emits checked/trusted publication for declarations, DOES> signatures, and trust metadata. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-publish-hooked | `--` | Emits hook-based publication for ordinary compiled definitions. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| p2w-entry | `label ptr a n n n --` | Pass-2 width-aware transport dispatch case (item 12 slice 3b): keyword match, per-operand width query, and the `ext` lowering emitter run through `JIT-XT-EXECUTE`. | `test/run.f` | src/habu/habu2.f | 2026-07-06 |
| em-compile-p2wide | `--` | Emits the pass-2 width dispatch stage: the 18 whole-bundle transport cases between the local-reference and keyword tiers. | `test/run.f` | src/habu/habu2.f | 2026-07-06 |
| em-p2-start | `--` | Emits the pass-2 re-entry: saves the live input, repoints the tokenizer at the captured body, rewinds CP/DP, resets per-definition compile state, and re-emits the prologue. | `test/run.f` | src/habu/habu2.f | 2026-07-06 |
| em-p2-trigger | `--` | Emits the certified-definition width query: any wider-than-cell width fact enters the pass-2 re-run (wide facts inside a does> split body fail closed). | `test/run.f` | src/habu/habu2.f | 2026-07-06 |
| em-p2-check-definer | `--` | Emits the sig'd publish gate: pass 1 runs the hook and the pass-2 trigger; the pass-2 second ';' skips the hook re-check (the pass-1 certify already registered the signature). | `test/run.f` | src/habu/habu2.f | 2026-07-06 |
| em-p2-finish | `--` | Emits the publish-tail pass-2 exit: resumes the saved real input and clears the pass-2 state cells. | `test/run.f` | src/habu/habu2.f | 2026-07-06 |
| em-compile-publish | `--` | Selects trusted-signature or hook publication for a closed compiled definition. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-semi | `label --` | Emits semicolon close handling and binds the caller-provided not-semi continuation label. | `test/run.f` | src/habu/habu2.f | 2026-06-27 |
| em-compile-control-keywords | `--` | Emits compile-mode control-flow keyword dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-string-keywords | `--` | Emits compile-mode string parsing-word dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-meta-keywords | `--` | Emits compile-mode meta/parsing keyword dispatch cases such as tick, postpone, DOES>, quotations, and checked `is` assignment. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-28 |
| em-compile-loop-keywords | `--` | Emits compile-mode loop, return-stack, recursion, and locals keyword dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-keywords | `--` | Chains factored compile-mode keyword dispatch groups. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-local | `--` | Emits compile-mode local-reference lookup and fallthrough. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-literal | `--` | Emits compile-mode numeric literal handling for integer and float literals. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-arith-ops | `--` | Emits arithmetic and bitwise optimized operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-shuffle-ops | `--` | Emits optimized stack-shuffle operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-compare-ops | `--` | Emits optimized comparison operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-unary-ops | `--` | Emits optimized unary numeric operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-float-ops | `--` | Emits optimized floating-point operator dispatch cases. | `tools/compiler-dispatch-test.f`, `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-ops | `--` | Chains factored compile-mode arithmetic, shuffle, comparison, unary, and float operator dispatch emitters. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-call | `--` | Emits compile-mode lookup, immediate execution, and call generation. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-reset-compile-state | `--` | Emits reset of compile/repl/evaluate state cells after rollback or recovery. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-eval-undef-rollback | `--` | Emits evaluate-frame rollback for undefined-word failures. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-eval-throw-recover | `--` | Emits the evaluate throw-escape recovery entry: transactional frame rollback that delivers the escaping throw code via EVALERR-CELL instead of exiting the process. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |
| em-repl-recover | `--` | Emits REPL recovery after errors, restoring line-start compile state and stacks. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-undef | `--` | Emits undefined-word diagnostics and evaluate/REPL recovery routing. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-eval-clean-exit | `--` | Emits clean evaluate end-of-buffer return path. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-repl-read | `--` | Emits REPL line-state save, read callback call, EOF handling, and input reset. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile-exit | `--` | Emits interpreter end-of-input handling for evaluate, REPL ok/read, and process exit. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| em-compile | `--` | Chains the factored compile-mode dispatch, call, undefined, and exit emitters. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| emit-main | `--` | Allocates main-loop labels and chains EM-STARTUP/COMMENT/INTERPRET/COMPILE. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| SRCA@ | `-- ptr u8` | Reads EMIT-FORTH's saved source pointer from a raw variable for the final `BYTES,` copy. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| AOT-BLOB-BUF@ | `-- ptr u8` | Views the AOT-REPL capture code-blob scratch buffer as bytes for the blob copy and `BYTES,` emit. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |
| AOT-REC-BUF@ | `-- ptr a` | Views the AOT-REPL capture dict-record scratch buffer for the 48-byte record copy and `BYTES,` emit. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |
| AOT-SITE-BUF@ | `-- ptr u8` | Views the AOT-REPL capture call-site table scratch buffer as bytes for the packed 4B reloc rows (blob-off u16 + name-off u16) and `BYTES,` emit. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |
| AOT-NAMES-BUF@ | `-- ptr u8` | Views the AOT-REPL capture name-pool scratch buffer as bytes for the name intern and `BYTES,` emit. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |
| AOT-DSITE-BUF@ | `-- ptr u8` | Views the AOT-REPL capture DATA/CODE-literal relocation table scratch buffer as bytes for the packed u16 blob-offset tables and `BYTES,` emit. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |
| AOT-BOOTRUN-BUF@ | `-- ptr u8` | Views the AOT-REPL capture boot-run name-list scratch buffer as bytes for the `[len][name]` intern and `BYTES,` emit of the install-tail entry words. | `test/run.f` | src/habu/habu2.f | 2026-07-03 |
| AOT-PWID-BUF@ | `-- ptr u8` | Views the protected-WID registry AOT capture scratch buffer as bytes for the u32-WID serialize (ACAP-PWID-*) and `BYTES,` emit (TFAM 2b-v). | `test/run.f` | src/habu/habu2.f | 2026-07-04 |
| AOT-DBASE | `-- ptr a` | Host build-time cast of the metabuild dictionary base to a record pointer for the AOT-REPL capture reverse-lookup. | `test/run.f` | src/habu/aot-capture.f | 2026-07-03 |
| AOT-A>U8 | `ptr a -- ptr u8` | Host build-time byte view of a code/dict address for the AOT-REPL capture blob and name copies. | `test/run.f` | src/habu/aot-capture.f | 2026-07-03 |
| AOT-N>U8 | `n -- ptr u8` | Host build-time byte view of a code/dict address value for the AOT-REPL capture blob source and EXT-name reads. | `test/run.f` | src/habu/aot-capture.f | 2026-07-03 |
| AOT-CELL@ | `ptr a -- n` | Host build-time cell read of a metabuild dict record field for the AOT-REPL capture reverse-lookup. | `test/run.f` | src/habu/aot-capture.f | 2026-07-03 |
| emit-forth | `ptr u8 n --` | Top-level engine builder: consumes source bytes, allocates every forward-ref label, and emits the complete image. | `test/run.f` | src/habu/habu2.f | 2026-06-25 |
| LINUX-VA>PTR | `va -- ptr n` | Linux runtime loader addresses are tagged as `va`; converting one to a host pointer for GOT/header reads is the raw image boundary. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 |
| IMAGE-TEXT-SIZE-OFF | `-- n` | Linux executable header offset where the snapshot writer reads the mapped text size. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 |
| IMAGE-TEXT-CONTENT-ADJ | `-- n` | Linux text-size field adjustment from segment size to code-content size for snapshot streaming. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 |
| IMAGE-TEXT-TRAILER-ADJ | `-- n` | Linux trailer address adjustment for snapshot restore when the text-size field includes the code offset. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 |
| DATA-VA | `-- ptr a` | Linux fixed DATA virtual address used by snapshot and AOT startup writers as both cell-address and byte-span base. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 |
| DATA-SIZE | `-- n` | Linux fixed DATA mapping size used by snapshot validation and image inspection. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 |
| MBUF-RC>PTR | `n -- ptr u8` | Narrows the raw anonymous-mmap return cell for the target image-builder output buffer into the typed byte span used by checked image writers. | `tools/image-bytes-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/os/image-bytes.f | 2026-07-03 |
| CODE-OFF | `-- n` | Linux executable code offset used by checked snapshot streaming code. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/linux/layout.f | 2026-06-29 |
| LINUX-DLOPEN-SLOT-OFF | `-- n` | Linux dynamic ELF GOT byte offset for the `dlopen` relocation inside the computed RW segment. | `test/run.f`, `test/gate-aot-positive.f` | src/os/linux/layout.f | 2026-06-29 |
| LINUX-DLSYM-SLOT-OFF | `-- n` | Linux dynamic ELF GOT byte offset for the `dlsym` relocation inside the computed RW segment. | `test/run.f`, `test/gate-aot-positive.f` | src/os/linux/layout.f | 2026-06-29 |
| LINUX-IMAGE-BASE | `-- n` | Linux runtime image base is recovered from the code base and executable offset before reading the snapshot header. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 |
| LINUX-TEXT-CELL | `-- ptr n` | Linux runtime text-size header cell is reached through raw image-address arithmetic. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 |
| LINUX-TEXT-SIZE | `-- n` | Linux runtime text size is read from the mapped executable header to locate the dynamic RW segment. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 |
| LINUX-RW-VA | `-- va` | Linux dynamic RW segment starts after the live text mapping, not at a fixed address, so runtime FFI slots derive from the image header. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 |
| DLOPEN-SLOT-VA | `-- va` | Linux dynamic ELF GOT virtual address for `dlopen`, computed from the live RW segment so it cannot overlap the snapshot text. | `test/run.f`, `test/gate-aot-positive.f` | src/os/linux/layout.f | 2026-06-29 |
| DLSYM-SLOT-VA | `-- va` | Linux dynamic ELF GOT virtual address for `dlsym`, computed from the live RW segment so it cannot overlap the snapshot text. | `test/run.f`, `test/gate-aot-positive.f` | src/os/linux/layout.f | 2026-06-29 |
| DLOPEN-SLOT | `-- ptr n` | Linux dynamic ELF GOT cell where ld.so resolves `dlopen` before Habu FFI reads the function pointer. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 |
| DLSYM-SLOT | `-- ptr n` | Linux dynamic ELF GOT cell where ld.so resolves `dlsym` before Habu FFI reads the function pointer. | `test/run.f`, `lib/ffi-test.f` | src/os/linux/layout.f | 2026-06-29 |
| SNAP-EXTRA-PTR | `-- ptr u8` | Linux snapshot writer stages the dynamic RW segment after the header buffer and streams it after the padded live text. | `test/run.f`, `test/engine-suite.f` | src/os/linux/elf.f | 2026-06-29 |
| SNAP-EXTRA-SIZE | `-- n` | Linux snapshot writer appends the fixed-size `.dynamic` plus GOT segment after the padded text payload. | `test/run.f`, `test/engine-suite.f` | src/os/linux/elf.f | 2026-06-29 |
| IMAGE-TEXT-SIZE-OFF | `-- n` | macOS executable header offset where the snapshot writer reads the mapped text size. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 |
| IMAGE-TEXT-CONTENT-ADJ | `-- n` | macOS text-size field adjustment from section size to code-content size for snapshot streaming. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 |
| IMAGE-TEXT-TRAILER-ADJ | `-- n` | macOS trailer address adjustment because the section size excludes the code offset. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 |
| DATA-VA | `-- ptr a` | macOS fixed DATA virtual address used by snapshot and AOT startup writers as both cell-address and byte-span base. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 |
| DATA-SIZE | `-- n` | macOS fixed DATA mapping size used by snapshot validation and image inspection. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 |
| CODE-OFF | `-- n` | macOS executable code offset used by checked snapshot streaming code. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/macos/layout.f | 2026-06-29 |
| MACHO>N-PTR | `n -- ptr n` | macOS image boundary cast: turns a computed Mach-O header/GOT cell address into a typed cell pointer; all offset arithmetic around it remains checked. | `lib/ffi-test.f`, `test/run.f` | src/os/macos/layout.f | 2026-06-29 |
| DLOPEN-SLOT | `-- ptr n` | macOS dyld-resolved `__DATA_CONST,__got` cell for libSystem `_dlopen`, located from the live Mach-O text size. | `lib/ffi-test.f`, `test/run.f` | src/os/macos/layout.f | 2026-06-29 |
| DLSYM-SLOT | `-- ptr n` | macOS dyld-resolved `__DATA_CONST,__got` cell for libSystem `_dlsym`, adjacent to `DLOPEN-SLOT`. | `lib/ffi-test.f`, `test/run.f` | src/os/macos/layout.f | 2026-06-29 |
| SNAP-EXTRA-PTR | `-- ptr u8` | macOS snapshot writer stages the `__DATA_CONST` GOT page plus chained-fixups blob after the header buffer and streams it after the padded live text. | `test/run.f`, `test/engine-suite.f` | src/os/macos/macho.f | 2026-06-29 |
| SNAP-EXTRA-SIZE | `-- n` | macOS snapshot images append one `__DATA_CONST` page plus the fixed chained-fixups payload after the padded text payload. | `test/run.f`, `test/engine-suite.f` | src/os/macos/macho.f | 2026-06-29 |
| ARGC-CELL | `-- n` | Common DATA header byte offset for the process argc startup cell. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/layout.f | 2026-06-26 |
| ARGV-CELL | `-- n` | Common DATA header byte offset for the process argv vector startup cell. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/layout.f | 2026-06-26 |
| ENVP-CELL | `-- n` | Common DATA header byte offset for the process envp vector startup cell. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/layout.f | 2026-06-26 |
| JIT-XT-EXECUTE | `n --` | Narrow higher-order JIT boundary: checked dispatch entry words pass one build-time emitter xt through this raw `execute` shim. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/jit.f | 2026-06-27 |
| fold-entry | `label ptr a n n --` | JIT constant-fold case: emits the keyword guard then dispatches one fold handler through `JIT-XT-EXECUTE` and branches to the main-loop label. | `test/run.f` | src/habu/jit.f | 2026-06-27 |
| vop-entry | `label ptr a n n n --` | JIT binop case: fold-vs-emit split with two indirect xts and raw asm, returning to the main-loop label. | `test/run.f` | src/habu/jit.f | 2026-06-27 |
| vopi-entry | `label ptr a n n n n --` | JIT binop-immediate case: fold/register/immediate split with three indirect xts and raw asm, returning to the main-loop label. | `test/run.f` | src/habu/jit.f | 2026-06-27 |
| vshuf-entry | `label ptr a n n n --` | JIT reg-aware stack-shuffle case (dup/over/swap/drop/nip as register moves); `sxt` runs through `JIT-XT-EXECUTE`. | `test/run.f` | src/habu/jit.f | 2026-06-27 |
| vun-entry | `label ptr a n n n --` | JIT unary-op case: con-fold vs in-place reg op via `foldxt`/`emitxt` and raw asm, returning to the main-loop label. | `test/run.f` | src/habu/jit.f | 2026-06-27 |
| c-prof-mctx>r21 | `--` | Profiler SIGALRM handler derives the target mcontext address from raw signal-entry registers. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 |
| c-prof-pc>r9 | `--` | Profiler SIGALRM handler reads the target-specific saved PC field from mcontext. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 |
| c-prof-sigaction-frame | `--` | Profiler builds the target kernel sigaction record directly on the generated stack. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 |
| c-prof-sigaction | `--` | Profiler installs SIGALRM through the target raw sigaction syscall ABI. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 |
| c-prof-sigaction-done | `--` | Profiler releases the generated sigaction stack scratch frame. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 |
| c-prof-timer-frame | `--` | Profiler builds the target itimerval record directly on the generated stack. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 |
| c-prof-timer | `--` | Profiler arms the interval timer through the raw setitimer syscall ABI. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 |
| c-prof-timer-done | `--` | Profiler releases the generated timer stack scratch frame. | `test/gate-debug.f`, `test/run.f` | src/habu/prof.f | 2026-06-25 |
| emit-prof-prims | `--` | Emits the sampling-profiler prims as raw asm via FPRIM-L. | `test/run.f`, `test/gate-debug.f` | src/habu/prof.f | 2026-06-25 |
| TIMES | `R i64 [ R -- R ] -- R` | Counted loop keeps the quotation available across repeated `execute`; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f | 2026-06-16 |
| EACH | `R ptr a i64 [ R a -- R ] -- R` | Array iterator keeps the quotation across element calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f | 2026-06-16 |
| MAP | `R ptr a i64 [ R a -- R a ] -- R` | Array map keeps the quotation across element calls and mutates cells in place; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f | 2026-06-16 |
| FOLD | `R ptr a i64 b [ R b a -- R b ] -- R b` | Array fold keeps the quotation across accumulator calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f | 2026-06-16 |
| INCLUDE-MMAP-PTR | `n -- ptr u8` | Refines the checked anonymous `mmap` result into the byte pointer backing include buffers after size selection and `-1` failure checking; syscall-result pointer refinement is outside checker inference. | `test/gate-dictionary.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/include.f | 2026-06-28 |
| INCLUDE-EVALUATE | `ptr u8 n --` | Source composition reads and bounds file bytes in checked code, then crosses the dynamic `evaluate` boundary that the checker intentionally rejects in ordinary checked definitions. | `test/gate-dictionary.f`, `test/run.f` | src/core/include.f | 2026-06-28 |
| ARENA-RC>PTR | `n -- ptr a` | Thin identity refinement from a checked, nonnegative anonymous `mmap` result into the checker's cell arena pointer; syscall-result pointer typing is outside checker inference. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-03 |
| TOKBUF-RC>PTR | `n -- ptr u8` | Thin identity refinement from a checked, nonnegative anonymous `mmap` result into the checker's token byte-buffer pointer; syscall-result pointer typing is outside checker inference. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-03 |
| USIGS-RC>PTR | `n -- ptr u8` | Thin identity refinement from a checked, nonnegative anonymous `mmap` result into the checker's transient signature byte store; syscall-result pointer typing is outside checker inference. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-03 |
| USIGS-CELL-AT | `n -- ptr a` | Refines a cell-aligned offset inside the byte-addressed transient signature store so checker metadata can write cell headers (e.g. the USIGS-CLEAR head cell) while byte-copy paths keep `ptr u8`. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-04 |
| HIDX-MEM-NULL | `-- ptr a` | The unallocated symbol-index cache sentinel is a null pointer; the checker cannot type a literal `0` as `ptr a`, so this one-line refinement supplies the typed null that `HIDX-MEM-CLEAR` stores and `HIDX-MEM-READY?` tests. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-04 |
| HIDX-RC>PTR | `n -- ptr n` | Thin identity refinement from a checked, nonnegative anonymous `mmap` result into the checker's symbol-index cell table; syscall-result pointer typing is outside checker inference. | `tools/check-test.f`, `tools/build-fixpoint-test.f`, `test/run.f` | src/core/checker.f | 2026-07-03 |
| CELL | `-- n` | Structure layouts load before the checker so checker records can use them; this row publishes the already-defined cell-size constant to checked users. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 |
| STRUCT-BYTE+ | `ptr a n -- ptr u8` | `CFIELD:` needs to refine a structure base plus byte offset into a byte pointer; generic `+` can produce only `ptr a`, and `BYTE+` requires an existing byte pointer. | `test/gate-dictionary.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 |
| BEGIN-STRUCTURE | `-- ptr a n` | Structure defining words use `CREATE`/`DOES>` and parse definition names, so the checker needs declared effects for the top-level layout DSL. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 |
| +FIELD | `ptr a n n -- ptr a n` | Field definers consume and return the in-progress layout cursor while creating accessor words through `CREATE`/`DOES>`. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 |
| PTR-FIELD: | `ptr a n -- ptr a n` | Pointer field definer preserves the layout cursor while creating a pointer-valued accessor; `CREATE`/`DOES>` keeps this as a trusted defining boundary. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 |
| PTR-VARIABLE | `--` | Pointer variables are created through `CREATE`/`DOES>` with a pointer-valued runtime effect that the checker cannot infer from the definer body. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 |
| CFIELD: | `ptr a n -- ptr a n` | Byte field definer preserves the layout cursor while creating a byte-pointer accessor; `CREATE`/`DOES>` keeps this as a trusted defining boundary. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 |
| END-STRUCTURE | `ptr a n --` | Sealing a structure consumes the layout cursor and writes the final byte size into the created size word. | `test/gate-dictionary.f`, `lib/vector-test.f`, `test/run.f` | src/core/structures-effects.f | 2026-06-30 |
| DTC-EVAL | `--` | Audited `evaluate` wrapper for `deftype`: compiles the constructed `TRUSTED: >NAME ( n -- NAME ) ;` / `NAME>N` converter shapes so a user-declared nominal integer gets its explicit no-op identity casts. `evaluate` cannot be checker-typed; each generated converter is a proven identity, so this single boundary covers every deftype-derived pair. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-07-03 |
| FDEF-EVAL | `--` | Audited `evaluate` wrapper for `FFI:`: compiles one generated checked wrapper from an explicit typed effect, resolver word, and C symbol. The generated body performs only role erasure/pointer erasure at the ABI edge, fail-closed `dlsym`, `FFI-ARG!`, `FFI-CALLABI`, and declared result refinement. | `lib/ffi-test.f`, `maki/test.f`, `test/run.f` | lib/ffi.f | 2026-07-03 |
| >IDX | `n -- idx` | Runtime identity cast from a generic cell to the nominal index role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| IDX>N | `idx -- n` | Runtime identity cast from the nominal index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >LEN | `n -- len` | Runtime identity cast from a generic cell to the nominal length role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| LEN>N | `len -- n` | Runtime identity cast from the nominal length role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >COUNT | `n -- count` | Runtime identity cast from a generic cell to the nominal count role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| COUNT>N | `count -- n` | Runtime identity cast from the nominal count role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >OFF | `n -- off` | Runtime identity cast from a generic cell to the nominal offset role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| OFF>N | `off -- n` | Runtime identity cast from the nominal offset role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >FD | `n -- fd` | Runtime identity cast from a generic cell to the nominal file-descriptor role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| FD>N | `fd -- n` | Runtime identity cast from the nominal file-descriptor role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >RC | `n -- rc` | Runtime identity cast from a generic cell to the nominal return-code role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| RC>N | `rc -- n` | Runtime identity cast from the nominal return-code role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >PID | `n -- pid` | Runtime identity cast from a generic cell to the nominal process-id role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| PID>N | `pid -- n` | Runtime identity cast from the nominal process-id role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >MS | `n -- ms` | Runtime identity cast from a generic cell to the nominal millisecond role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| MS>N | `ms -- n` | Runtime identity cast from the nominal millisecond role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >NS | `n -- ns` | Runtime identity cast from a generic cell to the nominal nanosecond role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| NS>N | `ns -- n` | Runtime identity cast from the nominal nanosecond role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >TOK | `n -- tok` | Runtime identity cast from a generic cell to the nominal token-index role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| TOK>N | `tok -- n` | Runtime identity cast from the nominal token-index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >REG | `n -- reg` | Runtime identity cast from a generic cell to the nominal register role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| REG>N | `reg -- n` | Runtime identity cast from the nominal register role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >LABEL | `n -- label` | Runtime identity cast from a generic cell to the nominal code-label role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| LABEL>N | `label -- n` | Runtime identity cast from the nominal code-label role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >VA | `n -- va` | Runtime identity cast from a generic cell to the nominal virtual-address role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| VA>N | `va -- n` | Runtime identity cast from the nominal virtual-address role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >SYMIDX | `n -- symidx` | Runtime identity cast from a generic cell to the nominal dynamic-symbol-index role; the checker cannot infer nominal role refinement from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| SYMIDX>N | `symidx -- n` | Runtime identity cast from the nominal dynamic-symbol-index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >ASM | `n -- asm` | Runtime identity cast from a generic cell to the nominal assembled-code phase token; the checker cannot infer phase-token refinement from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| ASM>N | `asm -- n` | Runtime identity cast from the nominal assembled-code phase token back to a generic cell; the checker cannot infer phase-token erasure from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >IMG | `n -- img` | Runtime identity cast from a generic cell to the nominal executable-image phase token; the checker cannot infer phase-token refinement from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| IMG>N | `img -- n` | Runtime identity cast from the nominal executable-image phase token back to a generic cell; the checker cannot infer phase-token erasure from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| >SNAP | `n -- snap` | Runtime identity cast from a generic cell to the nominal snapshot-header phase token; the checker cannot infer phase-token refinement from an empty body. | `test/gate-engine.f`, `test/engine-suite.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| SNAP>N | `snap -- n` | Runtime identity cast from the nominal snapshot-header phase token back to a generic cell; the checker cannot infer phase-token erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f | 2026-06-26 |
| TTHROWS-RAW | `a n --` | Top-level test assertion boundary around execution-token `catch`; checked colon definitions should use `TTHROWSQ`, but top-level scripts cannot push `[: ;]` quotations. | `lib/test/assert-test.f`, `test/run.f` | lib/test/assert.f | 2026-06-22 |
| P>N | `ptr a -- n` | FFI argument marshalling: reinterpret any pointer as the raw integer cell the AAPCS64 trampoline loads into x0-x7; the checker has no pointer-to-cell coercion. | `lib/ffi-abi-test.f`, `lib/ffi-test.f`, `test/gate-stdlib.f` | lib/ffi-abi.f | 2026-06-27 |
| N>P | `n -- ptr u8` | FFI return marshalling: reinterpret an integer return cell (a handle or pointer from dlopen/dlsym or a callee) as a byte pointer. | `lib/ffi-abi-test.f`, `lib/ffi-test.f`, `test/gate-stdlib.f` | lib/ffi-abi.f | 2026-06-27 |
| TASK-NULL | `-- ptr a` | Null pointer sentinel for task control-block fields; Habu has no typed null literal. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-06-30 |
| TASK-N>PTR | `n -- ptr a` | Reinterpret task-control-block cell storage as a pointer when loading the current task pointer. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-06-30 |
| TASK-CELL>PTR-SLOT | `ptr a -- ptr ptr a` | Reinterpret a data-region cell address as a pointer-valued slot; the checker cannot infer the slot payload type from the offset. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-06-30 |
| TASK | `n --` | Defining word that allocates a task control-block record and returns it through DOES>; CREATE/DOES> effect is outside checker inference. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-06-30 |
| +USER | `n n -- n` | Defining word for task-local user storage; CREATE/DOES> returns an address derived from the current data region. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-06-30 |
| FACILITY | `--` | Defining word for owner-tracked pthread mutex storage; CREATE/DOES> returns the facility record address. | `lib/task-test.f`, `test/gate-stdlib.f` | lib/task.f | 2026-06-30 |
| c-task-live-guard | `--` | Engine emitter guard that rejects dictionary/source mutation while pthread tasks are live; raw exit path and token printing are assembly-side. | `lib/task-test.f`, `test/gate-stdlib.f`, `test/run.f` | src/habu/habu2.f | 2026-06-30 |
| SNAP= | `[ R -- S ] [ R -- S ] --` | Typed depth-introspection comparator (dot habu-typed-depth-introspection-18f0efda) and the sole snapshot boundary now that the untyped `T{ -> }T` DSL is retired: the checker verifies the two quotations leave an identical row shape S at CHECK time, so a shape mismatch is rejected before runtime; only the depth-marked drain of each quotation's output row stays trusted, and both drains are inlined into this one word. Values are compared through the checked TS-* judge path. | `lib/test/snap-test.f`, `lib/array-test.f`, `test/run.f` | lib/test/snap.f | 2026-07-03 |
| BUILD-CHECK-RAW | `ptr u8 n -- n` | Build helper boundary around `CHECK!`; the checker cannot certify a source definition by evaluating its own checker recursively. | `lib/build-test.f`, `test/run.f` | lib/build.f | 2026-06-18 |
| CHECK-QUIET-CANDIDATE! | `ptr u8 n -- n` | Shared test harness boundary that temporarily suppresses checker diagnostics and runs `CHECK-CANDIDATE!`; recursive checker invocation and raw `DIAGXT` mutation are centralized here. | `test/engine-suite.f`, `lib/array-test.f`, `lib/vector-test.f`, `lib/string-test.f`, `lib/json-write-test.f`, `tools/image-bytes-test.f`, `tools/asm-checked-test.f`, `lib/ptx/tile-test.f`, `lib/ptx/collective-test.f`, `test/run.f` | test/checker-assert.f | 2026-06-30 |
| MBUF | `-- ptr u8` | Image-byte test reuses the raw checked boundary loaded from `src/os/image-bytes.f`; the test may run after the file is already baked, so it republishes the audited effect locally. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-BOUNDS-RC | `-- n` | Image-byte test republishes the raw bounds-error status accessor from the image writer boundary. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-RESET | `--` | Image-byte test republishes the raw image-writer reset effect from the audited image-byte boundary. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-LEN | `n -- len` | Image-byte test republishes the nominal length constructor used by image-writer negative fixtures. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-OFF | `n -- off` | Image-byte test republishes the nominal offset constructor used by image-writer negative fixtures. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-HERE | `-- n` | Image-byte test republishes the raw image cursor read effect from the image writer boundary. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| IMG-M8 | `n --` | Image-byte test republishes the raw byte emitter effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| IMG-M16 | `n --` | Image-byte test republishes the raw 16-bit emitter effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| IMG-M32 | `n --` | Image-byte test republishes the raw 32-bit emitter effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| IMG-M64 | `n --` | Image-byte test republishes the raw 64-bit emitter effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-BYTES-LEN | `ptr u8 len --` | Image-byte test republishes the typed byte-copy effect for role-confusion fixtures. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-NAME16-LEN | `ptr u8 len --` | Image-byte test republishes the typed fixed-name copy effect for role-confusion fixtures. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-PAD-OFF | `off --` | Image-byte test republishes the typed pad-to-offset effect for role-confusion fixtures. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-LE32@ | `off -- n` | Image-byte test republishes the typed little-endian read effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| TLP-W32 | `n n -- n` | Layout-lowering golden reader: reinterprets a compiled subject's xt as the byte base for one u32 instruction load — test-only code introspection, same class as the imgdump/jitdump readers; every use sits directly under the suite's golden asserts. | `test/type-layout-lower-pending.f`, `test/run.f` | test/type-layout-lower-pending.f | 2026-07-06 |
| TLP-UN2 | `tlp-res<n,n> -- n n` | Matching raw 2-cell unpack of the seeded width-2 bundle so plain value asserts can prove whole-bundle transport preservation. | `test/type-layout-lower-pending.f`, `test/run.f` | test/type-layout-lower-pending.f | 2026-07-06 |
| TLP-UN4 | `tlp-mix<n,n> -- n n n n` | Matching raw 4-cell unpack of the seeded width-4 bundle for the execution asserts. | `test/type-layout-lower-pending.f`, `test/run.f` | test/type-layout-lower-pending.f | 2026-07-06 |
| M-LE32! | `n off --` | Image-byte test republishes the typed little-endian patch effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-LE64! | `n off --` | Image-byte test republishes the typed 64-bit patch effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-BE-RESET | `off --` | Image-byte test republishes the big-endian patch cursor reset effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-BE-HERE | `-- n` | Image-byte test republishes the big-endian patch cursor read effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-BE32 | `n --` | Image-byte test republishes the big-endian 32-bit emit effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-BE64 | `n --` | Image-byte test republishes the big-endian 64-bit emit effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| M-BE-BYTES-LEN | `ptr u8 len --` | Image-byte test republishes the big-endian typed byte-copy effect for fixture coverage. | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-06-30 |
| MSIZE | `-- n` | Image-byte test republishes the image buffer capacity to drive the cursor-overflow regression (the silent maker-build failure class). | `tools/image-bytes-test.f`, `test/run.f` | tools/image-bytes-test.f | 2026-07-03 |
| P5 | `-- i64` | Engine-suite trusted immediate around `POSTPONE`; the compile-time body emits `IM5`, while the declared effect is the runtime value compiled into `TP`. | `test/engine-suite.f`, `test/run.f` | test/engine-suite.f | 2026-06-24 |
| PROP-CHECK-HOOK | `ptr u8 n -- n` | Property-test fail-closed source hook wraps `CHECK!`; recursive checker invocation cannot be certified by the checked source it protects. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| PROP-INSTALL-HOOK | `--` | Property-test installer sets the fail-closed checker hook; mutating the hook is a named trusted boundary instead of a top-level mutation. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| CLEAR-MEAS | `R n -- n` | Property-test oracle drains the arbitrary residual data-stack tail left by a generated program while preserving the measured count; this is exactly the value-agnostic depth boundary. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-22 |
| ERR@ | `-- n` | Reads the engine `evaluate` recovery cell from the live `data-base` header so the in-process property oracle can distinguish clean execution from recovered traps. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| MARK | `--` | Property-test checkpoint captures code, dictionary, and user-signature cursors; these raw interpreter stores are outside the checker model. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| FORGET | `--` | Property-test rollback restores code, dictionary, and user-signature cursors after a generated program; raw interpreter-state mutation is the boundary. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| SMARK | `--` | Nested property-test checkpoint for shrink/metamorphic probes captures code, dictionary, and user-signature cursors. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| SFORGET | `--` | Nested property-test rollback restores code, dictionary, and user-signature cursors after shrink/metamorphic probes. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| CHK-MARK | `--` | Candidate-check checkpoint captures interpreter state before evaluating one generated definition under the verdict hook. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| CHK-FORGET | `--` | Candidate-check rollback removes a generated definition when the checker verdict was not certified. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| CHK-HOOK | `ptr u8 n -- n` | Candidate verdict hook records `CHECK!` result but returns success so rejected generated definitions can be rolled back in-process. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| CHK | `ptr u8 n --` | Installs the candidate verdict hook, evaluates generated source, restores the fail-closed hook, and rolls back non-certified candidates. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| RUN-MEAS | `n n --` | Builds and evaluates the generated measurement program, records `LAST-MEAS` or `LAST-TRAP`, and normalizes dynamic evaluation paths that the checker cannot express directly. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| REND-SIG$ | `-- ptr u8 n` | Reads the checker's last rendered signature buffer for the property round-trip amplifier; renderer state is internal checker state. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| CONFIRM-FR? | `-- bool` | False-reject oracle deliberately compiles one generated program with checking disabled, restores the hook, and measures runtime behavior to prove a rejection was real incompleteness. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test.f | 2026-06-24 |
| AX-COUNT | `-- n` | Primitive-axiom census reads the live checker PES table row count (`#PE`); the internal axiom table is outside the checker model. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test-core.f | 2026-07-03 |
| AX-NAME$ | `n -- ptr u8 n` | Primitive-axiom census recovers one PES axiom's folded primitive name from the checker symbol table. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test-core.f | 2026-07-03 |
| AX-STK | `n -- n` | Primitive-axiom census walks a persistent effect-node stack list (`EN-PUSH` chain) to count its arity; raw checker node layout is the boundary. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test-core.f | 2026-07-03 |
| AX-ARITY | `n -- n n` | Primitive-axiom census reads one PES axiom's declared data in/out arity from its effect record. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test-core.f | 2026-07-03 |
| AXEVAL | `-- n` | Primitive-axiom census evaluates the generated per-axiom measurement runner in-process; dynamic `evaluate` is outside the checker model. | `test/prop-test.f`, `test/gate-debug.f`, `test/run.f` | test/prop-test-core.f | 2026-07-03 |
| MOVZHW | `n n n -- n` | ARM64 source test reuses the raw unchecked encoder effect after conditional source loading or CLI-runner bake. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| ENC-ADD | `n n n -- n` | ARM64 source test republishes the raw add-instruction encoder effect for fixture assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| ENC-LDR | `n n n -- n` | ARM64 source test republishes the raw load-instruction encoder effect for fixture assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| ENC-BLR | `n -- n` | ARM64 source test republishes the raw branch-link-register encoder effect for fixture assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| >LIMM | `n -- n` | ARM64 source test republishes the immediate-layout helper effect used by encoder assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| ENC-ANDI | `n n n -- n` | ARM64 source test republishes the raw logical-immediate encoder effect for fixture assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| CW@ | `n -- ptr u8` | ARM64 source test republishes the code-buffer byte pointer boundary used to inspect emitted words. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| CODE-BYTE+ | `ptr u8 n -- ptr u8` | ARM64 source test republishes typed code-buffer byte-pointer arithmetic used by fixture reads. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| ARESET | `--` | ARM64 source test republishes the assembler-buffer reset effect for fixture setup. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| ADD, | `n n n --` | ARM64 source test republishes the raw instruction emitter effect for fixture setup. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| ASM-LEN | `-- n` | ARM64 source test republishes the assembler buffer length accessor effect for fixture assertions. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| LIT64, | `n n --` | ARM64 source test republishes the literal-emitter effect for fixture setup. | `tools/asm-src-test.f`, `test/run.f` | tools/asm-src-test.f | 2026-06-30 |
| MEM-ALLOC-PTR | `n -- ptr u8` | Refines a raw anonymous `mmap` result into a typed byte pointer after size validation and `-1` failure checking; the checker cannot express this syscall-result refinement yet. | `lib/memory-test.f`, `test/run.f` | lib/memory.f | 2026-06-21 |
| IMG-MMAP-PTR | `n -- ptr u8` | Refines a raw file-backed `mmap` result into a typed byte pointer after checking the `-1` failure result; the checker cannot express syscall-result refinement yet. | `tools/imgdump-test.f`, `test/run.f` | tools/imgdump.f | 2026-06-25 |
| CODE | `-- ptr u8` | Lazily maps the assembler output buffer outside DATA and refines the raw mmap result to the byte pointer used by `EMITW`, `BYTES,`, and image writers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/arch/arm64/icode.f | 2026-06-26 |
| ICODE-TABS | `-- ptr n` | Lazily maps the assembler label/fixup table block outside DATA and refines the raw mmap result to the numeric-cell pointer used by `LBLP`/`FXS`/`FXL`/`FXK`. | `test/run.f`, `tools/build-fixpoint-test.f` | src/arch/arm64/icode.f | 2026-06-26 |
| ENV-DATA | `-- ptr n` | Returns the fixed engine data-region header pointer used for argc/argv/envp cells. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 |
| ENV-DASH | `-- n` | Shared ASCII dash byte constant used by argv parsing helpers. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 |
| ARGC | `-- n` | Reads the process argc value captured by the native startup entry. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 |
| ARGV-BASE | `-- ptr ptr u8` | Refines the raw argv vector pointer read from the engine startup byte-offset cell. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 |
| ENVP-BASE | `-- ptr ptr u8` | Refines the raw envp vector pointer read from the engine startup byte-offset cell. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 |
| ZLEN | `ptr u8 -- n` | Measures a NUL-terminated byte string read from argv or envp. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 |
| ARGV$ | `n -- ptr u8 n` | Converts one argv c-string pointer to a counted byte string. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 |
| ENV-FALSE | `-- bool` | Produces a typed false flag for env and argv parsing helpers. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 |
| ENV=? | `ptr u8 ptr u8 n -- bool` | Compares one envp c-string against a counted variable name and following `=`. | `lib/process-env-test.f`, `test/run.f` | src/os/env-base.f | 2026-06-28 |
| NULL$ | `-- ptr u8 n` | Returns a typed empty string pair used for absent environment values. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-28 |
| GETENV | `ptr u8 n -- ptr u8 n` | Returns a counted environment value by scanning the raw envp vector captured at startup. | `lib/process-env-test.f`, `test/run.f` | src/os/env-base.f | 2026-06-28 |
| TMP-PATH-CAP | `-- n` | Fixed scratch capacity for target temp-path construction during pre-hook build drivers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-28 |
| TPP@ | `-- ptr u8` | Reads the temporary-path scratch cursor from a raw variable. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-28 |
| TMP-PATH-CHECK | `n --` | Validates the fixed target path scratch capacity before raw byte copies. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-28 |
| TMP-PATH | `ptr u8 n -- ptr u8 n` | Builds `$HB_TMP` or `/tmp` child paths in pre-hook engine build drivers. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-28 |
| SCRIPT-LOAD-Z? | `ptr u8 -- bool` | Recognizes the `--load` argv marker in a raw argv c-string. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 |
| SCRIPT-LOAD? | `-- bool` | Detects source-list mode from captured process argv. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 |
| SCRIPT-SEP? | `n -- bool` | Recognizes the `--` argv separator in source-list mode. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 |
| SCRIPT-ARG-START | `-- n` | Computes the first user script argument after source-list handling. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 |
| SCRIPT-ARGC | `-- n` | Returns user script argument count after source-list handling. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 |
| SCRIPT-ARGV | `n -- ptr u8` | Returns one user script argv c-string after source-list handling. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 |
| SCRIPT-ARGV$ | `n -- ptr u8 n` | Returns one user script argument as counted bytes. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/script-argv.f | 2026-06-28 |
| SCRIPT-ARG-START | `-- n` | Computes the first user argument for standalone bundles, where argv[0] is the executable path. | `tools/hb-build-test.f`, `test/run.f` | src/habu/bundle-argv.f | 2026-06-28 |
| SCRIPT-ARGC | `-- n` | Returns standalone bundle user argument count. | `tools/hb-build-test.f`, `test/run.f` | src/habu/bundle-argv.f | 2026-06-28 |
| SCRIPT-ARGV | `n -- ptr u8` | Returns one standalone bundle user argv c-string. | `tools/hb-build-test.f`, `test/run.f` | src/habu/bundle-argv.f | 2026-06-28 |
| SCRIPT-ARGV$ | `n -- ptr u8 n` | Returns one standalone bundle user argument as counted bytes. | `tools/hb-build-test.f`, `test/run.f` | src/habu/bundle-argv.f | 2026-06-28 |
| SHAKE? | `-- ptr n` | Treeshaker enable flag cell is a raw variable; checked scanner code needs its cell type pinned before using `@`/`!`. | `test/run.f` | src/habu/treeshake.f | 2026-06-26 |
| SHK-U | `-- ptr n` | Treeshaker source length cell is a raw variable used by checked scanner bounds tests. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| SKP | `-- ptr n` | Treeshaker scan cursor cell is a raw variable used by checked token scanning. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| STS | `-- ptr n` | Treeshaker token-start cell is a raw variable used by checked token scanning. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| REACHN | `-- ptr n` | Treeshaker reachability-buffer length cell is a raw variable used by checked reachability scanning. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| TKP | `-- ptr n` | Treeshaker tokenizer cursor cell is a raw variable used by checked token scanning. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| CHG | `-- ptr bool` | Treeshaker fixpoint-change flag cell is a raw variable used by checked reachability iteration. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| INDEF | `-- ptr bool` | Treeshaker in-definition flag cell is a raw variable used by checked source scanning. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| XNAME | `-- ptr bool` | Treeshaker expecting-definition-name flag cell is a raw variable used by checked source scanning. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| KEEPCUR | `-- ptr bool` | Treeshaker keep-current-definition flag cell is a raw variable used by checked reachability expansion. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| RSP | `-- ptr n` | Treeshaker reachability scan cursor cell is a raw variable used by checked reachability scanning. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| RTS | `-- ptr n` | Treeshaker reachability-token-start cell is a raw variable used by checked reachability scanning. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| TU | `-- ptr n` | Treeshaker current-token length cell is a raw variable used by checked scanner code. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| HB@ | `-- ptr u8` | Reads the stdin-engine baked-source buffer pointer stored in a raw variable. | `test/run.f` | src/habu/stdin.f | 2026-06-16 |
| EVAL-HOST | `ptr u8 n --` | Compiles a REPL source buffer in the metabuild host dict for AOT capture; `evaluate`'s net effect is source-dependent so the boundary declares the balanced install-tail effect. | `test/run.f` | src/habu/stdin.f | 2026-07-03 |
| BLD-PB@ | `-- ptr u8` | Reads the standalone-build source buffer pointer stored in a raw variable. | `test/run.f`, `tools/hb-build.f` | src/habu/build.f | 2026-06-24 |
| CHECK-BODY | `ptr u8 n -- n` | Shared source pre-verification recursively invokes the checker on an assembled definition body and renders the checker-owned uncheckable diagnostic before returning the verdict; recursive checker invocation and diagnostic-state access are the explicit verifier boundary. | `tools/hb-build-test.f`, `tools/check-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/verify-source.f | 2026-07-01 |
| CHECK-DOES-BODY | `ptr u8 n ptr u8 n -- n` | Shared source pre-verification routes `DOES>` bodies through the checker's dedicated `CHECK-DOES!` entrypoint; ordinary `CHECK!` cannot model the created-word data-field pointer. | `tools/check-test.f`, `test/run.f` | src/habu/verify-source.f | 2026-06-28 |
| TRUST-SIGNATURE | `ptr u8 n ptr u8 n --` | Shared source pre-verification records source-order defining-word signatures for parsed names; the checker cannot infer a dynamic mutation of its signature table from scanner state. | `tools/hb-build-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/verify-source.f | 2026-06-28 |
| MULTI-ERR-MODE? | `-- bool` | Shared source pre-verification reads the checker-internal multi-error mode flag; the checker registry does not publish `MULTI-ERR?` to later checked loads, so the verify loop's continue-past-reject decision rides the same verifier boundary as `CHECK-BODY`. | `tools/check-all-errors-test.f`, `test/run.f` | src/habu/verify-source.f | 2026-07-07 |
| CA-MULTI-BEGIN | `--` | The all-errors driver arms the checker-internal multi-error load mode around its single whole-buffer verify pass; mode control words are not registry-published to checked tool loads. | `tools/check-all-errors-test.f` | tools/check-all-errors-core.f | 2026-07-07 |
| CA-MULTI-END | `-- n` | Reads the multi-error reject count and clears the mode for the fail-closed exit decision; same unpublished-mode-word boundary as `CA-MULTI-BEGIN`. | `tools/check-all-errors-test.f` | tools/check-all-errors-core.f | 2026-07-07 |
| AOT-PB@ | `-- ptr u8` | Reads the AOT build source buffer pointer stored in a raw variable. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-lib.f | 2026-06-24 |
| AOT-DBASE@ | `-- ptr a` | Reads the runtime dictionary base pointer for AOT dictionary-record scans; record fields are mixed, so callers specialize the pointee type at each access. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-closure.f | 2026-06-24 |
| AOT-PTR@ | `ptr a -- ptr a` | Reads a dictionary long-name pointer field whose pointee is another address; the checker cannot express this pointer-to-pointer load yet. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-closure.f | 2026-06-24 |
| JSON-DIAGS | `-- ptr a` | AOT diagnostics read the checker's JSON-mode flag; the checker registry does not publish its own words to later checked loads, so the variable is typed as an axiom for the checked AOT tail. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-closure.f | 2026-07-07 |
| CHECK! | `ptr u8 n -- n` | The AOT driver hook wraps the engine checker entrypoint for user source; the entrypoint's effect is modeled as a primitive axiom so the checked AOT tail compiles under the toolchain hook. | `test/run.f`, `tools/hb-build.f` | src/habu/aot-closure.f | 2026-07-07 |
| MK-SBUF@ | `-- ptr u8` | Reads the hb-build maker source buffer pointer stored in a raw variable while compiling the separate maker image. | `tools/hb-build-test.f`, `test/run.f` | src/habu/maker.f | 2026-06-24 |
| STB@ | `-- ptr u8` | Reads the snapshot source text base pointer stored in a raw variable. | `test/run.f`, `tools/build-fixpoint.f snap` | src/habu/snap-lib.f | 2026-06-26 |
| STB-CELL@ | `-- ptr n` | Reads the snapshot source text base pointer as a cell-address for executable-header size lookup. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-06-26 |
| SDB@ | `-- ptr u8` | Reads the snapshot dictionary/data-region pointer stored in a raw variable. | `test/run.f`, `tools/build-fixpoint.f snap` | src/habu/snap-lib.f | 2026-06-26 |
| SNAP-CHECK-HOOK | `ptr u8 n -- n` | Snapshot image installs the fail-closed checker hook into emitted images that need a fresh hook; recursive `CHECK!` hook bodies are trusted boundaries. | `test/run.f`, `test/gate-debug.f` | src/habu/snap-lib.f | 2026-06-26 |
| SNAP-INSTALL-HOOK | `--` | Snapshot image mutates the checker hook cell only through a named trusted installer. | `test/run.f`, `test/gate-debug.f` | src/habu/snap-lib.f | 2026-06-26 |
| SNC-PTR | `-- ptr u8` | Scratch snapshot region view over a raw anonymous mmap; canonical-base writer scratch. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 |
| SNC-TEXT-N | `-- n` | Reads the saved text base cell as a plain integer for relocation band math. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 |
| SND-PTR | `-- ptr u8` | Scratch snapshot data view over a raw anonymous mmap; live-cell zeroing target. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 |
| SND-ZERO-CELL | `n --` | Zeroes one loader-overwritten live cell in the data scratch copy by layout offset. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 |
| SND-ZERO-SPAN-CELL | `n --` | Zeroes one evaluate-frame cell in the data scratch copy by layout offset. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 |
| SND-QUARANTINE@ | `n -- n` | Reads one quarantined dangling-pointer offset from the create table for scratch zeroing. | `test/run.f snap`, `tools/build-fixpoint-test.f` | src/habu/snap-lib.f | 2026-07-02 |
| S2-PATH-CAP | `-- n` | Fixed path-buffer capacity for the stage2 fixpoint driver. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/stage2.f | 2026-06-26 |
| S2-PATH-BUF | `-- ptr u8` | Stage2 fixpoint path scratch buffer used while building private artifact paths. | `test/run.f`, `tools/build-fixpoint-test.f` | src/habu/stage2.f | 2026-06-26 |
| SBUF@ | `-- ptr u8` | Reads the stage2 source buffer pointer stored in a raw variable. | `test/run.f` | src/habu/stage2.f | 2026-06-26 |
| IMGD-MMAP-PTR | `n -- ptr u8` | Converts the raw image mmap result into a typed byte pointer after checking mmap failure; OS mapping pointers are outside checker inference. | `tools/imagedisasm-test.f`, `test/run.f` | tools/imagedisasm.f | 2026-06-25 |
| MK-SPAN | `ptr<space-global,t> u32 -- span<space-global,t,fresh-extent-n>` | PTX from-raw-parts boundary: consumes a runtime extent assertion and retypes the base pointer as a span with a fresh rigid extent token. The checker cannot validate allocation length. | `lib/ptx/tile-test.f`, `test/run.f` | lib/ptx/tile.f | 2026-06-30 |
| MK-SPAN-ONCE | `ptr<space-global,t> u32 -- span<space-global-once,t,fresh-extent-n>` | PTX from-raw-parts boundary for an externally proven read-once/affine gradient buffer; it mints a distinct `space-global-once` span, not a cast from an ordinary span. | `lib/ptx/tile-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-30 |
| MK-SPAN= | `ptr<space-global,t> ptr<space-global,u> u32 -- span<space-global,t,fresh-extent-n> span<space-global,u,fresh-extent-n>` | PTX from-raw-parts boundary for two buffers sharing one asserted runtime extent; the repeated fresh template stamps both output spans with the same rigid extent token. | `lib/ptx/tile-test.f`, `test/run.f` | lib/ptx/tile.f | 2026-06-30 |
| MK-MATRIX | `ptr<space-global,t> u32 u32 -- matrix<space-global,t,fresh-extent-r,fresh-extent-c>` | PTX dense row-major matrix from-raw-parts boundary: consumes asserted row/column extents and retypes the base pointer as a matrix. The checker cannot validate allocation shape. | `lib/ptx/collective-test.f`, `test/run.f` | lib/ptx/tile.f | 2026-06-30 |
| MK-MATRIX-ONCE | `ptr<space-global,t> u32 u32 -- matrix<space-global-once,t,fresh-extent-r,fresh-extent-c>` | PTX dense row-major matrix from-raw-parts boundary for externally proven read-once rows; it mints a distinct `space-global-once` matrix used by row once words. | `lib/ptx/collective-test.f` | lib/ptx/tile.f | 2026-06-30 |
| GRID-CTX | `span<space-global,t,e> -- gridctx<b,e,fresh-mask-live>` | PTX tile-DSL v0: derives a flat grid-strided context from a global span and mints a fresh rigid mask token for that context; lowers to PTX index/mask setup the checker cannot infer (a tile primitive). | `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-06-29 |
| GRID-CTX-ONCE | `span<space-global-once,t,e> -- gridctx<b,e,fresh-mask-live>` | PTX tile-DSL read-once context derivation: same index/mask lowering as GRID-CTX but only accepts `space-global-once` spans. | `lib/ptx/tile-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-30 |
| COOP-CTX | `span<space-global,t,e> -- coopctx<b,e,fresh-mask-live>` | PTX cooperative shared-memory context: derives a block-uniform staging context from `%tid.x` without an early bounds branch, so all lanes reach shared-memory barriers. | `lib/ptx/tile-smem-test.f`, `lib/ptx/tile-test.f`, `tools/ptx/smem-cg.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-30 |
| LOAD | `span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m>` | PTX tile-DSL v0: masked coalesced load of a tile from a global span under a grid context; lowers to PTX ld.global the checker cannot infer (a tile primitive). | `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-06-27 |
| LOAD-ONCE | `span<space-global-once,t,e> gridctx<b,e,m> -- tile<t,b,m>` | PTX read-once/affine load: same PTX load as LOAD but typed to the once-space witness so its reverse-mode adjoint may be STORE-ONCE. | `lib/ptx/tile-test.f`, `lib/ptx/ad-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-30 |
| STORE | `tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> --` | PTX tile-DSL v0: masked store of a tile to a global span under a grid context (active lanes only); lowers to PTX st.global the checker cannot infer (a tile primitive). | `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-06-27 |
| STORE-ONCE | `tile<t,b,m> span<space-global-once,t,e> gridctx<b,e,m> --` | PTX read-once/affine store: ordinary `st.global` permitted only for `space-global-once` spans; normal LOAD adjoints still use SCATTER-ADD. | `lib/ptx/tile-test.f`, `lib/ptx/ad-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-30 |
| SCATTER-ADD | `tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> --` | PTX tile-DSL AD memory adjoint: accumulates a tile cotangent into a global span with `red.global.add.f32`; conservative default for `LOAD` adjoints unless a checked once-space witness selects STORE-ONCE. | `lib/ptx/tile-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-30 |
| FANIN-CTX | `ptr<space-global,t> -- fanctx<b,extent-n,fresh-mask-live>` | PTX fan-in scalar context: mints a fresh rigid active-lane mask over `%r1` lanes while preserving scalar addressing, so fan-in VJPs cannot be expressed as ordinary lane-indexed spans. | `lib/ptx/tile-test.f`, `tools/ptx/scatter-add-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 |
| FANIN-LOAD | `ptr<space-global,t> fanctx<b,e,m> -- tile<t,b,m>` | PTX fan-in scalar load: broadcasts one scalar global cell to every active lane under a fan-in context; rejects ordinary grid contexts. | `lib/ptx/tile-test.f`, `tools/ptx/scatter-add-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 |
| FANIN-SCATTER-ADD | `tile<t,b,m> ptr<space-global,t> fanctx<b,e,m> --` | PTX fan-in scalar adjoint: accumulates active-lane cotangents into one scalar global cell with `red.global.add.f32`; rejects lane-indexed contexts. | `lib/ptx/tile-test.f`, `tools/ptx/scatter-add-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 |
| INDEX-CTX | `span<space-global,u32,i> span<space-global,t,e> -- idxctx<b,i,e,fresh-mask-live>` | PTX indexed-memory context: mints a fresh active-lane mask over the index span and carries both index extent `i` and data extent `e`, so arbitrary gather/scatter words cannot mix index and data shapes. | `lib/ptx/tile-test.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 |
| UNIQUE-INDEX-CTX | `span<space-global,u32,i> span<space-global,t,e> -- uniqidxctx<b,i,e,fresh-mask-live>` | PTX indexed-memory context plus an audited external uniqueness witness for `idx`; plain indexed stores require this witness instead of assuming duplicate-free indices. | `lib/ptx/tile-test.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 |
| INDEX-DENSE-LOAD | `span<space-global,t,i> idxctx<b,i,e,m> -- tile<t,b,m>` | PTX dense-side companion load under an indexed context: reads lane `i` from a span whose extent matches the index span, preserving the indexed context mask. | `lib/ptx/tile-test.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 |
| UNIQUE-INDEX-DENSE-LOAD | `span<space-global,t,i> uniqidxctx<b,i,e,m> -- tile<t,b,m>` | PTX dense-side load for unique-index kernels; same dense lane access as `INDEX-DENSE-LOAD` while preserving the uniqueness witness for a later plain indexed store. | `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-07-01 |
| INDEX-DENSE-STORE | `tile<t,b,m> span<space-global,t,i> idxctx<b,i,e,m> --` | PTX dense-side store under an indexed context: writes active lane `i` to a span matching the index extent, not to `idx[i]`, so duplicate index values do not create a race. | `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-07-01 |
| INDEX-LOAD | `span<space-global,u32,i> span<space-global,t,e> idxctx<b,i,e,m> -- tile<t,b,m>` | PTX generic indexed gather: loads `data[idx[i]]` with a runtime `idx[i] < e` guard while the checker enforces shared index/data extent tokens through `idxctx`. | `lib/ptx/tile-test.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 |
| INDEX-SCATTER-ADD | `tile<t,b,m> span<space-global,u32,i> span<space-global,t,e> idxctx<b,i,e,m> --` | PTX generic indexed scatter-add: accumulates active lanes into `data[idx[i]]` with `red.global.add.f32`, safe for duplicate indices and the conservative AD default. | `lib/ptx/tile-test.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/tile.f | 2026-07-01 |
| INDEX-STORE | `tile<t,b,m> span<space-global,u32,i> span<space-global,t,e> uniqidxctx<b,i,e,m> --` | PTX indexed plain store: writes `data[idx[i]]` only when the caller supplies a `uniqidxctx` uniqueness witness; duplicate-prone indexed updates must use `INDEX-SCATTER-ADD`. | `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-07-01 |
| SCALE | `tile<t,b,m> uniform<t> -- tile<t,b,m>` | PTX tile-DSL v0: tile times a uniform scalar; lowers to PTX mul.rn (no contraction) the checker cannot infer (a tile primitive). | `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-06-27 |
| FMA. | `uniform<t> tile<t,b,m> tile<t,b,m> -- tile<t,b,m>` | PTX tile-DSL v0: fused multiply-add `a*x+y` with one rounding; lowers to PTX fma.rn the checker cannot infer (a tile primitive). | `lib/ptx/tile-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-29 |
| +. | `tile<t,b,m> tile<t,b,m> -- tile<t,b,m>` | PTX tile-DSL v0: elementwise tile add with matching mask; lowers to PTX add.rn the checker cannot infer (a tile primitive). | `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-06-29 |
| -. | `tile<t,b,m> tile<t,b,m> -- tile<t,b,m>` | PTX tile-DSL v0: elementwise tile subtract with matching mask; lowers to PTX sub.rn the checker cannot infer (a tile primitive). | `lib/ptx/tile-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-29 |
| *. | `tile<t,b,m> tile<t,b,m> -- tile<t,b,m>` | PTX tile-DSL v0: elementwise tile multiply with matching mask; lowers to PTX mul.rn the checker cannot infer (a tile primitive). | `lib/ptx/autograd-test.f`, `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-06-29 |
| /. | `tile<t,b,m> tile<t,b,m> -- tile<t,b,m>` | PTX tile-DSL v0: elementwise tile divide with matching mask; lowers to PTX div.rn the checker cannot infer (a tile primitive). | `lib/ptx/tile-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile.f | 2026-06-29 |
| RELU | `tile<t,b,m> -- tile<t,b,m>` | PTX tile-DSL v0: elementwise ReLU over one tile; lowers to PTX max with zero, which the checker cannot infer (a tile primitive). | `lib/ptx/tile-test.f` | lib/ptx/tile.f | 2026-06-29 |
| TILE-LOOP | `n tile<t,b,m> [ tile<t,b,m> -- tile<t,b,m> ] -- tile<t,b,m>` | PTX tile-DSL checked counted loop (K-reduction / streaming): applies an accumulator-preserving body `n` times. The checker enforces the body's `( tile -- tile )` effect at every call site (capability (a) of habu-checker-capability-typed); the emit unroll lowers to PTX the checker cannot infer (a tile primitive). | `lib/ptx/tile-loop-test.f`, `lib/ptx/tile-loop-neg-test.f` | lib/ptx/tile-loop.f | 2026-06-27 |
| STAGE | `span<space-global,t,e> coopctx<b,e,m> -- span<space-shared,t,e>` | PTX tile-DSL shared-memory staging (capability (b)): cooperatively copies a global block into `SMEM`, emits `bar.sync`, and returns a `space-shared` span. It rejects elementwise `gridctx` so lanes cannot branch around barriers. | `lib/ptx/tile-smem-test.f`, `lib/ptx/tile-smem-neg-test.f`, `tools/ptx/smem-cg.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile-smem.f | 2026-06-30 |
| SLOAD | `span<space-shared,t,e> coopctx<b,e,m> -- tile<t,b,m>` | PTX tile-DSL shared load (capability (b)): reads a register tile from a `space-shared` span under the same cooperative mask; rejects a `space-global` span and rejects elementwise contexts. | `lib/ptx/tile-smem-test.f`, `lib/ptx/tile-smem-neg-test.f`, `tools/ptx/smem-cg.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile-smem.f | 2026-06-30 |
| SSTORE | `tile<t,b,m> span<space-shared,t,e> coopctx<b,e,m> --` | PTX tile-DSL shared store (capability (b)): writes a register tile into a `space-shared` span under the same cooperative mask and emits a barrier after the write. | `lib/ptx/tile-smem-test.f`, `tools/ptx/smem-cg.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile-smem.f | 2026-06-30 |
| ACC-ZERO | `gridctx<b,e,m> -- acc<t,b,m>` | PTX tile-DSL register accumulator (capability (c)): a fresh zeroed accumulator of the new `acc<t,b,m>` type (distinct from tile<>, never unify). Emits `mov.f32 0f0` via cg.f the checker cannot infer; device-verified (tools/ptx/acc-device-test.f). | `lib/ptx/tile-acc-test.f`, `lib/ptx/tile-acc-neg-test.f` | lib/ptx/tile-acc.f | 2026-06-27 |
| ACC-FMA | `acc<t,b,m> tile<t,b,m> tile<t,b,m> -- acc<t,b,m>` | PTX tile-DSL accumulator FMA (capability (c)): fused multiply-add of two operand tiles into the register accumulator (one K-step). Emits `fma.rn.f32` via cg.f the checker cannot infer; device-verified (tools/ptx/acc-device-test.f). | `lib/ptx/tile-acc-test.f` | lib/ptx/tile-acc.f | 2026-06-27 |
| ACC-TILE | `acc<t,b,m> -- tile<t,b,m>` | PTX tile-DSL accumulator finalize (capability (c)): the completion gate - converts an `acc<>` to a storable `tile<>` so an unfinalized accumulator cannot be stored to global. Identity in emit (the accumulator register is the result tile); device-verified (tools/ptx/acc-device-test.f). | `lib/ptx/tile-acc-test.f`, `lib/ptx/tile-acc-neg-test.f` | lib/ptx/tile-acc.f | 2026-06-27 |
| ACC-LOOP | `n acc<t,b,m> [ acc<t,b,m> -- acc<t,b,m> ] -- acc<t,b,m>` | PTX tile-DSL accumulator-typed counted loop (capability (c)): the K-reduction over an `acc<>` accumulator, enforcing an accumulator-preserving body. Emit unrolls; lowers to PTX the checker cannot infer (a tile primitive). | `lib/ptx/tile-acc-test.f` | lib/ptx/tile-acc.f | 2026-06-27 |
| GRID-CTX-V4 | `span<space-global,t,e> -- gridctx<b,e,fresh-mask-live>` | PTX v4 tile DSL: derives a flat grid context where each thread owns four consecutive elements and mints a fresh rigid mask token for that context; general `N` is handled by scalar residual lanes in load/store. | `lib/ptx/tile-v4-test.f` | lib/ptx/tile-v4.f | 2026-06-29 |
| LOAD-V4 | `span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m>` | PTX v4 tile DSL: lowers to `ld.global.v4.f32` for full vectors and predicated scalar loads for residual lanes while preserving the scalar tile type. | `lib/ptx/tile-v4-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile-v4.f | 2026-06-29 |
| STORE-V4 | `tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> --` | PTX v4 tile DSL: lowers to `st.global.v4.f32` for full vectors and predicated scalar stores for residual lanes while preserving the scalar tile type. | `lib/ptx/tile-v4-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile-v4.f | 2026-06-29 |
| SCALE-V4 | `tile<t,b,m> uniform<t> -- tile<t,b,m>` | PTX v4 tile DSL: lane-wise multiply for the four-register tile representation; codegen detail only, checked effect matches scalar `SCALE`. | `lib/ptx/tile-v4-test.f` | lib/ptx/tile-v4.f | 2026-06-29 |
| ADD-V4 | `tile<t,b,m> tile<t,b,m> -- tile<t,b,m>` | PTX v4 tile DSL: lane-wise add for the four-register tile representation; codegen detail only, checked effect matches scalar `+.`. | `lib/ptx/tile-v4-test.f` | lib/ptx/tile-v4.f | 2026-06-29 |
| SUB-V4 | `tile<t,b,m> tile<t,b,m> -- tile<t,b,m>` | PTX v4 tile DSL: lane-wise subtract for the four-register tile representation; codegen detail only, checked effect matches scalar `-.`. | `lib/ptx/tile-v4-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile-v4.f | 2026-06-29 |
| MUL-V4 | `tile<t,b,m> tile<t,b,m> -- tile<t,b,m>` | PTX v4 tile DSL: lane-wise multiply for the four-register tile representation; codegen detail only, checked effect matches scalar `*.`. | `lib/ptx/tile-v4-test.f` | lib/ptx/tile-v4.f | 2026-06-29 |
| DIV-V4 | `tile<t,b,m> tile<t,b,m> -- tile<t,b,m>` | PTX v4 tile DSL: lane-wise divide for the four-register tile representation; codegen detail only, checked effect matches scalar `/.`. | `lib/ptx/tile-v4-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/tile-v4.f | 2026-06-29 |
| RELU-V4 | `tile<t,b,m> -- tile<t,b,m>` | PTX v4 tile DSL: lane-wise ReLU for the four-register tile representation; codegen detail only, checked effect matches scalar `RELU`. | `lib/ptx/tile-v4-test.f` | lib/ptx/tile-v4.f | 2026-06-29 |
| ROW | `-- rowidx<e>` | PTX tile-DSL M6: blockIdx.x as a row index proven < R under the launch ABI; a primitive the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| ROW-SPAN | `matrix<space-global,t,e,k> rowidx<e> -- span<space-global,t,k>` | PTX tile-DSL M6: row r of a dense matrix as a span over its columns (base r*C, checked); a primitive the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| ROW-SPAN-ONCE | `matrix<space-global-once,t,e,k> rowidx<e> -- span<space-global-once,t,k>` | PTX tile-DSL read-once row projection: derives a row span only from a `space-global-once` matrix. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-30 |
| ROW-CTX | `span<space-global,t,k> -- rowctx<b,k,fresh-mask-live>` | PTX tile-DSL M6: one-block-per-row context (lane = tid, mask = tid < N) and fresh rigid mask token for that context; a primitive the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| ROW-CTX-ONCE | `span<space-global-once,t,k> -- rowctx<b,k,fresh-mask-live>` | PTX tile-DSL read-once row context derivation; same index/mask lowering as ROW-CTX but only accepts once-space spans. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-30 |
| ROW-LOAD | `span<space-global,t,k> rowctx<b,k,m> -- tile<t,b,m>` | PTX tile-DSL M6: masked row-local load of a tile; lowers to PTX ld.global the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| ROW-LOAD-ONCE | `span<space-global-once,t,k> rowctx<b,k,m> -- tile<t,b,m>` | PTX read-once row load: same PTX load as ROW-LOAD but typed to once-space so its reverse-mode adjoint may be ROW-STORE-ONCE. | `lib/ptx/collective-test.f`, `lib/ptx/ad-test.f` | lib/ptx/collective.f | 2026-06-30 |
| ROW-STORE | `tile<t,b,m> span<space-global,t,k> rowctx<b,k,m> --` | PTX tile-DSL M6: masked row-local store (active lanes only); lowers to PTX st.global the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| ROW-STORE-ONCE | `tile<t,b,m> span<space-global-once,t,k> rowctx<b,k,m> --` | PTX read-once row store: ordinary row `st.global` permitted only for once-space row spans; ordinary ROW-LOAD adjoints still use ROW-SCATTER-ADD. | `lib/ptx/collective-test.f`, `lib/ptx/ad-test.f` | lib/ptx/collective.f | 2026-06-30 |
| ROW-SCATTER-ADD | `tile<t,b,m> span<space-global,t,k> rowctx<b,k,m> --` | PTX tile-DSL AD row memory adjoint: masked `red.global.add.f32` to rowbase+lane offset; conservative default for `ROW-LOAD` adjoints unless a checked once-space witness selects ROW-STORE-ONCE. | `lib/ptx/collective-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/collective.f | 2026-06-30 |
| BLOCK-MAX | `tile<f32,b,m> -- uniform<f32>` | PTX tile-DSL M6: shared-memory thread-0 fold over `PTX-BLOCK@` lanes; inactive lanes contribute max identity (-inf) at the reducer, independent of the tile value. Warp-shfl remains future perf work. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| BLOCK-SUM | `tile<f32,b,m> -- uniform<f32>` | PTX tile-DSL M6: shared-memory thread-0 fold over `PTX-BLOCK@` lanes; inactive lanes contribute sum identity (0) at the reducer, so direct row sums and backward cotangents do not depend on `ROW-LOAD` seeding. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| B- | `tile<t,b,m> uniform<t> -- tile<t,b,m>` | PTX package-public tile-DSL M6 word (`PTX:B-` at call sites): tile minus a broadcast uniform scalar; lowers to PTX sub.rn the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| B/ | `tile<t,b,m> uniform<t> -- tile<t,b,m>` | PTX package-public tile-DSL M6 word (`PTX:B/` at call sites): tile divided by a broadcast uniform scalar; lowers to PTX div.rn the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| U/ | `uniform<t> uniform<t> -- uniform<t>` | PTX package-public tile-DSL word (`PTX:U/` at call sites): uniform divided by uniform for the softmax `PTX:B/` adjoint (`ds = -Sum(dz*z)/s`); lowers to PTX div.rn the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| EXP. | `tile<f32,b,m> -- tile<f32,b,m>` | PTX tile-DSL M6: elementwise exp (ex2.approx.ftz(x*log2e), tolerance acceptance-gated); a primitive the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| BROADCAST | `uniform<f32> -- tile<f32,b,m>` | PTX tile-DSL AD: fills a tile from a uniform (named form of the broadcast in `PTX:B-`/`PTX:B/`); the mutual adjoint of BLOCK-SUM for reverse-mode AD; a primitive the checker cannot infer. | `lib/ptx/autograd-test.f` | lib/ptx/collective.f | 2026-06-29 |
| BLOCK-MAX-SELECT | `uniform<f32> tile<f32,b,m> uniform<f32> -- tile<f32,b,m>` | PTX tile-DSL AD: the BLOCK-MAX adjoint - a masked scatter routing the cotangent to the arg-max lane (deterministic lowest-lane tie-break), 0 elsewhere; a primitive the checker cannot infer. | `lib/ptx/collective-test.f` | lib/ptx/collective.f | 2026-06-29 |
| NEG | `a -- a` | PTX tile-DSL AD: polymorphic sign flip (forward NEG self-adjoint; the `PTX:B-`/`PTX:B/` adjoints negate a block-uniform); lowers to PTX neg.f32. | `lib/ptx/ad-saved-test.f` | lib/ptx/ad-saved.f | 2026-06-27 |
| SAVED-X | `-- tile<f32,b,m>` | PTX tile-DSL AD saved-value: a nonlinear adjoint's saved forward input tile; materialised by the save-vs-recompute pass the checker cannot infer (body throws E-PTX-NOIMPL pending buffer reload, habu-ad-thread-saved). | `lib/ptx/ad-saved-test.f` | lib/ptx/ad-saved.f | 2026-06-27 |
| SAVED-Y | `-- tile<f32,b,m>` | PTX tile-DSL AD saved-value: a nonlinear adjoint's saved forward output tile (EXP. bwd = dz*y); body throws E-PTX-NOIMPL pending buffer reload. | `lib/ptx/ad-saved-test.f` | lib/ptx/ad-saved.f | 2026-06-27 |
| SAVED-Z | `-- tile<f32,b,m>` | PTX tile-DSL AD saved-value: `PTX:B/`'s saved output tile z (ds = -Sum(dz*z)/s); body throws E-PTX-NOIMPL pending buffer reload. | `lib/ptx/ad-saved-test.f` | lib/ptx/ad-saved.f | 2026-06-27 |
| SAVED-MX | `-- uniform<f32>` | PTX tile-DSL AD saved-value: BLOCK-MAX's saved block-uniform max (arg-max select); body throws E-PTX-NOIMPL pending buffer reload. | `lib/ptx/ad-saved-test.f` | lib/ptx/ad-saved.f | 2026-06-27 |
| SAVED-S | `-- uniform<f32>` | PTX tile-DSL AD saved-value: `PTX:B/`'s saved block-uniform divisor s; body throws E-PTX-NOIMPL pending buffer reload. | `lib/ptx/ad-saved-test.f` | lib/ptx/ad-saved.f | 2026-06-27 |
| SPAN-REG | `n -- span<space-global,f32,extent-n>` | PTX codegen: from-register identity cast - a kernel arg is a PTX register number, this asserts its span type so the emit driver runs the checked kernel checked (the codegen from_raw_parts boundary). | `tools/ptx/saxpy-cg.f` | lib/ptx/cg.f | 2026-06-29 |
| UNIFORM-REG | `n -- uniform<f32>` | PTX codegen: from-register identity cast asserting a register holds a uniform scalar param; thin boundary so the emit driver stays checked. | `tools/ptx/saxpy-cg.f` | lib/ptx/cg.f | 2026-06-29 |
| PTR-REG | `n -- ptr<space-global,f32>` | PTX codegen: from-register identity cast asserting a kernel arg register holds a scalar global f32 pointer for checked fan-in emit drivers. | `tools/ptx/scatter-add-grad-cg.f`, `tools/ptx/scatter-add-gradcheck.f` | lib/ptx/cg.f | 2026-07-01 |
| SPAN-ONCE-REG | `n -- span<space-global-once,f32,extent-n>` | PTX codegen: from-register identity cast asserting a kernel arg register holds a read-once/affine span for checked once-space emit tests. | `tools/ptx/once-cg.f`, `tools/ptx/saxpy-test.f` | lib/ptx/cg.f | 2026-06-30 |
| INDEX-SPAN-REG | `n -- span<space-global,u32,extent-i>` | PTX codegen: from-register identity cast asserting a kernel arg register holds a u32 index span for generic indexed gather/scatter emit drivers. | `tools/ptx/indexed-scatter-cg.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/cg.f | 2026-07-01 |
| INDEX-VALUE-SPAN-REG | `n -- span<space-global,f32,extent-i>` | PTX codegen: from-register identity cast asserting a kernel arg register holds dense per-index values with the same extent as the index span. | `tools/ptx/indexed-scatter-cg.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/cg.f | 2026-07-01 |
| DATA-SPAN-REG | `n -- span<space-global,f32,extent-d>` | PTX codegen: from-register identity cast asserting a kernel arg register holds the indexed data span whose extent is checked separately from the index span. | `tools/ptx/indexed-scatter-cg.f`, `tools/ptx/indexed-scatter-gradcheck.f` | lib/ptx/cg.f | 2026-07-01 |
| MATRIX-REG | `n -- matrix<space-global,f32,extent-r,extent-c>` | PTX codegen: from-register identity cast asserting a kernel arg register holds a row-major matrix, so row-kernel emit drivers run checked bodies. | `tools/ptx/softmax-launch.f`, `tools/ptx/saxpy-test.f` | lib/ptx/cg.f | 2026-06-30 |
| MATRIX-ONCE-REG | `n -- matrix<space-global-once,f32,extent-r,extent-c>` | PTX codegen: from-register identity cast asserting a kernel arg register holds a read-once row-major matrix for checked once-space row emit tests. | `lib/ptx/collective-test.f` | lib/ptx/cg.f | 2026-06-30 |
| R>BITS | `r -- n` | PTX codegen f64->f32 marshalling: reinterpret a Habu 64-bit float as its bit pattern (the one thin cast; F64>F32 then repacks to 32-bit in checked code). | `lib/ptx/header-test.f` | lib/ptx/cg.f | 2026-06-29 |
| BITS>R | `n -- r` | PTX codegen f32->f64 readback: reinterpret a device-returned f32 bit pattern (widened by F32>F64) back into a Habu float - lets a GPU training loop read weights back and recompute gradients. | `lib/ptx/header-test.f` | lib/ptx/cg.f | 2026-06-29 |
| MM-A-REG | `n -- matrix<space-global,f32,extent-m,extent-k>` | GEMM codegen from-register cast for the A operand; separate from MATRIX-REG so the checked call site proves A[M,K], B[K,N], C[M,N]. | `lib/ptx/gemm-checked-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/cg-matmul.f | 2026-06-30 |
| MM-B-REG | `n -- matrix<space-global,f32,extent-k,extent-n>` | GEMM codegen from-register cast for the B operand; shares K with A and N with C at the checked MM-CHECKED call site. | `lib/ptx/gemm-checked-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/cg-matmul.f | 2026-06-30 |
| MM-C-REG | `n -- matrix<space-global,f32,extent-m,extent-n>` | GEMM codegen from-register cast for the C operand; ties output rows to A and output columns to B at the checked MM-CHECKED call site. | `lib/ptx/gemm-checked-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/cg-matmul.f | 2026-06-30 |
| MM-STATE | `matrix<space-global,f32,m,k> matrix<space-global,f32,k,q> matrix<space-global,f32,m,q> -- mmctx<m,k,q> mmacc<f32,block-256,mask-live>` | GEMM codegen token shim: consumes the typed A/B/C matrix operands after checked setup emission and creates the phase/accumulator tokens used by checked `MM-K-LOOP` and `MM-STORE`. | `lib/ptx/gemm-checked-test.f`, `tools/ptx/saxpy-test.f` | lib/ptx/cg-matmul.f | 2026-06-30 |
| CODE-BYTE+ | `ptr u8 n -- ptr u8` | Refines assembler code-buffer byte-pointer arithmetic for emitted instruction bytes and patching. | `test/run.f`, `tools/build-fixpoint-test.f`, `tools/bootstrap-codegen-test.f` | src/arch/arm64/icode.f | 2026-06-29 |
| CRH | `-- ptr u8` | Crash-handler header buffer is raw dictionary storage copied into signal-safe write output. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| CRH-BYTE+ | `ptr u8 n -- ptr u8` | Refines crash-handler byte-pointer arithmetic while copying the signal header. | `test/gate-debug.f`, `test/run.f` | src/habu/crash.f | 2026-06-30 |
| linux-spawn-fail-n | `n --` | Linux child-side spawn failure reporter emits raw `write`/`exit_group` for the supplied failure-pipe fd register number. | `lib/process-test.f`, `test/run.f` | src/habu/habu1.f | 2026-06-29 |
| BFR-BYTE@ | `ptr u8 n -- u8` | Refresh prelude byte reader over dictionary name bytes; raw record pointers are refined before this checked scanner can read them. | `tools/build-fixpoint-test.f`, `test/run.f` | src/habu/hide.f | 2026-06-29 |
| SHK-N | `-- ptr n` | Treeshaker token length cell is a raw variable used by checked token comparison loops. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| SHK-C | `-- ptr n` | Treeshaker byte/delimiter scratch cell is a raw variable used by checked scanner helpers. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| KEEP-U | `-- ptr n` | Treeshaker candidate-token length cell is a raw variable used by checked keep/reachability scanning. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| SHK-BYTE+ | `ptr u8 n -- ptr u8` | Refines treeshaker byte-pointer arithmetic for token scanning; the raw `+` is the typed pointer-offset boundary. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| SCAN-MODE | `-- ptr n` | Treeshaker reachability scan-mode cell is a raw variable used by checked source walks. | `test/run.f` | src/habu/treeshake.f | 2026-06-30 |
| XREF-REC+ | `ptr a n -- ptr a` | Offsets an opaque dictionary-record pointer by a raw dictionary-layout displacement. | `tools/xref-test.f`, `test/gate-dictionary.f`, `test/run.f` | src/habu/xref.f | 2026-06-29 |
| ZBYTE@ | `ptr u8 n -- u8` | Reads one byte from argv/envp C strings through byte-offset pointer arithmetic. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-29 |
| ZBYTE! | `u8 ptr u8 n --` | Writes one byte into target temp-path scratch through byte-offset pointer arithmetic. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-29 |
| ZPTR+ | `ptr u8 n -- ptr u8` | Refines argv/envp C-string byte-pointer arithmetic after the `NAME=` prefix. | `test/run.f`, `tools/hb-build-test.f` | src/os/env-base.f | 2026-06-29 |
| TMP-PATH-COPY-SRC | `ptr u8 n --` | Copies a script path suffix into the fixed target temp-path scratch using raw byte offsets. | `test/run.f`, `tools/build-fixpoint-test.f` | src/os/env-base.f | 2026-06-29 |

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
`0 set-check` window followed by the `CHECK-F-HOOK` definition and its
`' CHECK-F-HOOK set-check` re-arm, fail-closed via `70 throw`. That generated
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

Every trust site carries a class and an owning dot in the inventory TSV, and this
block is also the single source of truth for the ratchet ceiling (there is no
separate count block). Each row is `file[:name] class dot [count]`: a bare `file`
row classifies every site in that file no named row owns and carries an explicit
site count `N`; a `file:name` row overrides the file row for the site(s) called
`name` and implies count 1 unless it carries an explicit count (a name that
appears at more than one trust site, e.g. a definition plus its install). Valid
classes: `builder-emit` (engine/image/build
emitters and raw layout boundaries), `stdlib-boundary` (library-level trusted
boundaries), `test-metaprog` (test-owned fixtures and metaprogramming
harnesses), `prim-axiom` (nominal identity casts and primitive models the
checker treats as axioms), `discharge-candidate` (sites believed checkable
today). Sites without a row report class `-` and count as unclassified;
`bin/hb --load tools/trusted-inventory.f -- strict` fails while any remain, and
also fails for every owning dot referenced below that does not exist in
`.dots/` (as `<id>.md` or `<id>/<id>.md`), so closed or never-minted owners
cannot linger in the mapping. `strict` also prints a `by-file` line per source
with its non-zero per-class site counts, so classification drift is visible per
file, not just as a repo total.
The block is being refined from file granularity to `file:name` row granularity
and reassigned from the `habu-audit-trusted-inventory-3a950436` placeholder to
each site's real capability/discharge owner: `src/core/roles.f` (all 34
nominal-cast axioms) and `test/prop-test-core.f` (test-metaprog fixtures) carry
per-site rows, and the whole `prim-axiom` class — the nominal-cast axioms plus
the engine-primitive TRUST rows in `src/core/structures-effects.f`,
`tools/check-core.f`, and `src/core/include.f` — is now owned by its real owner
`habu-primitive-effect-axiom-1119f176` (the audited axiom table).
`test/prop-test-core.f` keeps a file-level row because its `0 set-check`
boundaries have no nameable key. Reassigning the remaining `builder-emit`,
`stdlib-boundary`, `test-metaprog`, and `discharge-candidate` rows to their real
owners is the rest of `habu-audit-trusted-inventory-3a950436`.

<!-- trusted-inventory-classes
src/arch/arm64/icode.f builder-emit habu-audit-trusted-inventory-3a950436 3
src/habu/aot-capture.f builder-emit habu-audit-trusted-inventory-3a950436 4
src/habu/aot-closure.f builder-emit habu-audit-trusted-inventory-3a950436 2
src/habu/aot-closure.f:JSON-DIAGS prim-axiom habu-primitive-effect-axiom-1119f176
src/habu/aot-closure.f:CHECK! prim-axiom habu-primitive-effect-axiom-1119f176
src/habu/aot-lib.f builder-emit habu-audit-trusted-inventory-3a950436 2
src/habu/build.f builder-emit habu-audit-trusted-inventory-3a950436 2
src/habu/bundle-argv.f builder-emit habu-audit-trusted-inventory-3a950436 4
src/habu/crash.f builder-emit habu-audit-trusted-inventory-3a950436 12
src/habu/debug-watch.f builder-emit habu-audit-trusted-inventory-3a950436 3
src/habu/debug.f builder-emit habu-audit-trusted-inventory-3a950436 8
src/habu/habu1.f builder-emit habu-audit-trusted-inventory-3a950436 37
src/habu/habu1.f:linux-setpgid-self builder-emit habu-builder-trust-rows-c5d41af6
src/habu/habu1.f:spawn-darwin-zero-attr builder-emit habu-builder-trust-rows-c5d41af6
src/habu/habu1.f:spawn-darwin-attr-defaults builder-emit habu-builder-trust-rows-c5d41af6
src/habu/habu2.f builder-emit habu-audit-trusted-inventory-3a950436 109
src/habu/hide.f builder-emit habu-audit-trusted-inventory-3a950436 14
src/habu/hide.f:BFR-CHECK-OFF builder-emit habu-staged-fixpoint-src-0b5fc6e6
src/habu/jit.f builder-emit habu-audit-trusted-inventory-3a950436 6
src/habu/layout.f builder-emit habu-audit-trusted-inventory-3a950436 3
src/habu/maker.f builder-emit habu-audit-trusted-inventory-3a950436 2
src/habu/prof.f builder-emit habu-audit-trusted-inventory-3a950436 9
src/habu/snap-lib.f builder-emit habu-audit-trusted-inventory-3a950436 10
src/habu/snap.f builder-emit habu-audit-trusted-inventory-3a950436 2
src/habu/stage2.f builder-emit habu-audit-trusted-inventory-3a950436 3
src/habu/stdin.f builder-emit habu-audit-trusted-inventory-3a950436 2
src/habu/treeshake.f builder-emit habu-audit-trusted-inventory-3a950436 18
src/habu/verify-source.f builder-emit habu-audit-trusted-inventory-3a950436 5
src/habu/xref.f builder-emit habu-audit-trusted-inventory-3a950436 7
src/os/env-base.f builder-emit habu-audit-trusted-inventory-3a950436 19
src/os/image-bytes.f builder-emit habu-audit-trusted-inventory-3a950436 1
src/os/linux/elf.f builder-emit habu-audit-trusted-inventory-3a950436 2
src/os/linux/layout.f builder-emit habu-audit-trusted-inventory-3a950436 17
src/os/macos/layout.f builder-emit habu-audit-trusted-inventory-3a950436 9
src/os/macos/macho.f builder-emit habu-audit-trusted-inventory-3a950436 2
src/os/script-argv.f builder-emit habu-audit-trusted-inventory-3a950436 7
tools/imagedisasm.f builder-emit habu-audit-trusted-inventory-3a950436 1
tools/imgdump.f builder-emit habu-audit-trusted-inventory-3a950436 1
tools/jitdump-core.f builder-emit habu-audit-trusted-inventory-3a950436 1
src/core/include.f prim-axiom habu-primitive-effect-axiom-1119f176 2
src/core/checker.f:ARENA-RC>PTR discharge-candidate habu-checker-self-typing-9ff8ba86
src/core/checker.f:TOKBUF-RC>PTR discharge-candidate habu-checker-self-typing-9ff8ba86
src/core/checker.f:USIGS-RC>PTR discharge-candidate habu-checker-self-typing-9ff8ba86
src/core/checker.f:USIGS-CELL-AT discharge-candidate habu-checker-self-typing-9ff8ba86
src/core/checker.f:HIDX-MEM-NULL discharge-candidate habu-checker-self-typing-9ff8ba86
src/core/checker.f:HIDX-RC>PTR discharge-candidate habu-checker-self-typing-9ff8ba86
src/core/roles.f:DTC-EVAL prim-axiom habu-typed-defining-words-aa224eb5
src/core/roles.f:>IDX prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:IDX>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>LEN prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:LEN>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>COUNT prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:COUNT>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>OFF prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:OFF>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>FD prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:FD>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>RC prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:RC>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>PID prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:PID>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>MS prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:MS>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>NS prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:NS>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>TOK prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:TOK>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>REG prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:REG>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>LABEL prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:LABEL>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>VA prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:VA>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>SYMIDX prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:SYMIDX>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>ASM prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:ASM>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>IMG prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:IMG>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:>SNAP prim-axiom habu-primitive-effect-axiom-1119f176
src/core/roles.f:SNAP>N prim-axiom habu-primitive-effect-axiom-1119f176
src/core/structures-effects.f prim-axiom habu-primitive-effect-axiom-1119f176 8
tools/check-all-errors-core.f builder-emit habu-multi-err-checking-42db26f4 2
tools/check-core.f prim-axiom habu-primitive-effect-axiom-1119f176 6
tools/check-core.f:CHECK! prim-axiom habu-primitive-effect-axiom-1119f176
src/core/combinators.f discharge-candidate habu-audit-trusted-inventory-3a950436 4
lib/ffi.f:FDEF-EVAL stdlib-boundary habu-typed-defining-words-aa224eb5
lib/build.f stdlib-boundary habu-audit-trusted-inventory-3a950436 1
lib/ffi-abi.f stdlib-boundary habu-audit-trusted-inventory-3a950436 2
lib/memory.f stdlib-boundary habu-audit-trusted-inventory-3a950436 1
lib/task.f stdlib-boundary habu-audit-trusted-inventory-3a950436 6
lib/ptx/ad-saved.f stdlib-boundary habu-adg-lowering-multi-24043a69 6
lib/ptx/cg-matmul.f stdlib-boundary habu-audit-trusted-inventory-3a950436 4
lib/ptx/cg.f stdlib-boundary habu-audit-trusted-inventory-3a950436 11
lib/ptx/collective.f stdlib-boundary habu-audit-trusted-inventory-3a950436 18
lib/ptx/tile-acc.f stdlib-boundary habu-checker-capability-typed-e0c76a02 4
lib/ptx/tile-loop.f stdlib-boundary habu-checker-capability-typed-e0c76a02 1
lib/ptx/tile-smem.f stdlib-boundary habu-checker-capability-typed-e0c76a02 3
lib/ptx/tile-v4.f stdlib-boundary habu-audit-trusted-inventory-3a950436 9
lib/ptx/tile.f stdlib-boundary habu-audit-trusted-inventory-3a950436 31
tools/lint/text.f:CHECK! prim-axiom habu-primitive-effect-axiom-1119f176
lib/test/snap.f:SNAP= test-metaprog habu-typed-depth-introspection-18f0efda
lib/test/assert.f test-metaprog habu-audit-trusted-inventory-3a950436 1
maki/eval.f test-metaprog habu-audit-trusted-inventory-3a950436 1
test/checker-assert.f test-metaprog habu-audit-trusted-inventory-3a950436 1
test/type-layout-lower-pending.f test-metaprog habu-tfam-12-layout-057181a9 3
test/engine-suite.f test-metaprog habu-audit-trusted-inventory-3a950436 49
test/gate-common-lib.f test-metaprog habu-audit-trusted-inventory-3a950436 6
test/prop-test-core.f test-metaprog habu-seal-set-check-b3676b33 2
test/prop-test-core.f:PROP-INSTALL-HOOK test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:CLEAR-MEAS test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:ERR@ test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:MARK test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:FORGET test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:SMARK test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:SFORGET test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:CHK-MARK test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:CHK-FORGET test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:CHK-HOOK test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:CHK-COMPILE-CERT test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:CHK test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:RUN-MEAS test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:REND-SIG$ test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:CONFIRM-FR? test-metaprog habu-audit-trusted-inventory-3a950436
test/prop-test-core.f:AX-COUNT prim-axiom habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:AX-NAME$ prim-axiom habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:AX-STK prim-axiom habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:AX-ARITY prim-axiom habu-primitive-effect-axiom-1119f176
test/prop-test-core.f:AXEVAL prim-axiom habu-primitive-effect-axiom-1119f176
tools/asm-src-test.f test-metaprog habu-audit-trusted-inventory-3a950436 12
tools/image-bytes-test.f test-metaprog habu-audit-trusted-inventory-3a950436 22
src/core/check-hook.f:HOOK stdlib-boundary habu-police-set-check-850bc543
src/habu/aot.f:USER-HOOK builder-emit habu-police-set-check-850bc543
src/habu/snap-lib.f:SNAP-CHECK-HOOK builder-emit habu-police-set-check-850bc543 2
tools/check-core.f:CHK-CHECK-HOOK stdlib-boundary habu-police-set-check-850bc543
tools/lint/text.f:LINT-CHECK-HOOK stdlib-boundary habu-police-set-check-850bc543
test/engine-suite.f:ES-VERDICT-HOOK test-metaprog habu-police-set-check-850bc543 2
test/engine-suite.f:HOOK test-metaprog habu-police-set-check-850bc543 2
test/gate-aot-negative-lib.f:HOOK test-metaprog habu-police-set-check-850bc543
test/prop-test-core.f:PROP-CHECK-HOOK test-metaprog habu-police-set-check-850bc543 4
-->
