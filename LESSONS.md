# Lessons

What worked, what didn't, and why. Read at session start; update after findings,
mistakes, or insights. Lessons only — no API reference or code snippets (→ `docs/`).

## Self-host milestone 4 — execution tokens + catch/throw (2026-06-10)

- Standalone now has `'`/`[']`/`EXECUTE` and `catch`/`throw`. `[']` bakes the
  found word's code address as a literal push (via the existing `c-lit` emitter,
  x11=addr); `EXECUTE` saves x30 around `BLR` (the enclosing word still needs its
  link). `catch`/`throw` chain 48-byte handler frames through a data-region cell
  `[x20+8]` (HND): the frame saves prev-HND, data-sp(x19), machine-sp, link, and a
  **resume address from an ADR inside the catch stencil** — PC-relative, so it
  survives the memcpy that inlines the stencil into a word; `throw` restores all
  four and `br`s to it. Verified: throw returns the code, no-throw returns 0, the
  data stack is restored to the catch point, nesting + sequencing work.
- **`Lkwcmp` must fold only A–Z**, not `|0x20` unconditionally, or symbol
  keywords (`'`, `[']`, `s"`) never match (`'['|0x20` = `'{'`). Keywords are
  stored literally; only uppercase-letter source bytes are lowered.
- **No forward references** in the single-pass standalone: `['] FOO` before FOO is
  defined silently emits nothing (FIND fails, the guard skips `c-lit`), leaving a
  garbage xt → `catch`/`EXECUTE` jumps to it and faults. Define before use.
- **Locals `{: a b :}`** (completes dot 4): a data-region header holds the
  per-word locals table (count, frame size, name records). `{:` records the names,
  carves an `sp` frame, and pops the declared values into slots (slot 0 = first
  name; pop top→highest slot). A body token is matched against the table
  (`Lloc-find`) BEFORE FIND → resolves to `ldr x9,[sp,#slot*8]`. `;` tears down the
  frame before RET. Locals coexist with catch: the locals frame sits below catch's
  handler frames, so `throw` unwinds to the catch and the locals stay readable.

## Self-host 7 — occurs-check (the "core risk") runs natively (2026-06-10)

- The PLAN flags occurs/resolve mutual recursion through structure as the core
  risk. Done iteratively (a worklist instead of recursion, which the single-pass
  standalone can't do by-name): `OCCURS` resolves each term and pushes a `ptr`'s
  inner onto the worklist. Verified natively: `var0 ∈ ptr(ptr(var0))` → true,
  `var0 ∈ ptr(var1)`/`ptr(con)` → false. Adds `T-PTR` + a CREATE'd arena for ptr
  inners. This is the hardest checker algorithm, proven runnable on the standalone.
- **Standalone limitation: one `{: :}` block per word.** `c-lbrace` resets the
  locals table on each `{:`, so a second block in the same word wipes the first
  (bit `MK-PTR` when it used `{: inner :} … {: idx :}`). Use a single block, or no
  locals. (A real fix would accumulate across blocks + grow the frame.)

## Self-host 6/7 — checker core runs natively (2026-06-10)

- `CONSTANT` added (defining word: pop the value, emit a `c-lit` push body). With
  constants + the existing `CREATE`/`@`/`!`/`cells` + `BEGIN/WHILE/REPEAT` +
  `IF/ELSE/THEN`, the checker's **algorithmic core** — type-term encode/decode,
  occurs-style checks, **unification, and binding-chain resolution** — compiles
  and runs as native code emitted by the standalone (verified end to end:
  2-link var→var→con resolves, unify binds and a re-resolve sees it, con≠con
  fails). The heart of dots 6/7, proven runnable on the standalone itself.
- **Buffer sizes are real limits as the standalone grows:** `SCODE` (assembled-
  code scratch) was 8 KB and the standalone hit 8504 B → `EMITW` wrote past it →
  gforth `-9` (invalid memory address) inside ASSEMBLE, not a clean throw. Bumped
  `SCODE`→96 KB, `MPAGE` (__TEXT)→64 KB, `MSIZE` (Mach-O buf)→128 KB. Watch these
  when porting more code natively.

## Self-host milestone 5 — wordlists (2026-06-10)

- Each dict record carries a wordlist id (DREC 40→48, wid at +40; seed prims = 0 =
  FORTH). A data-region header cell holds CURRENT (new defs take it) and a WIDNEXT
  counter. `WORDLIST` hands out fresh ids; `get-current`/`set-current` pick the
  target; `search-wl ( a u wid -- addr|0 )` is FIND restricted to one wid
  (case-folded). The interpreter's FIND stays wid-agnostic (so the standalone can
  always call its own words); wordlist scoping is opt-in via `search-wl` — exactly
  what the codegen dispatch port needs (CG-VS vs FORTH).
- `S"` is compile-only — testing `search-wl` at the top level silently fed it a
  garbage string (S" skipped as unknown in interpret mode). Exercise it inside `:`.

## Crash diagnostics — in-binary register dump (2026-06-10)

- caf-built binaries (NATIVE-EVAL exes + the standalone) install an in-binary
  signal handler (`crash.fs`) for SIGILL/TRAP/BUS/SEGV that dumps the faulting
  registers (sig, x0..x28, fp, lr, sp, pc) as hex to stderr and `exit(134)`. caf
  itself dumps codegen state (`#IC`/`VSP`/`CARRY-N`/`LOOP-DEPTH`/token pos) on a
  codegen throw. No external debugger — **lldb can't launch our minimal Mach-O in
  this sandbox** (its batch `-o run` hangs even on a normal C binary; debugger
  task-port acquisition is denied), so self-contained is the only robust path.
- **macOS arm64 signal ABI without libc:** `sigaction(#46)` with a
  `struct __sigaction { sa_handler, sa_tramp, sa_mask, sa_flags }`. Set BOTH
  sa_handler and sa_tramp to the handler — the kernel enters sa_tramp directly
  with x2=sig, x4=ucontext (no libc `_sigtramp` needed). `mcontext = [ucontext+48]`;
  `__ss.__x[0]` at `mcontext+16`, then x0..x28, fp(+248), lr(+256), sp(+264),
  pc(+272). `SA_SIGINFO`=0x40.
- **Reg-31 footgun (cost two debug cycles):** in the shifted-register ALU forms
  (`ADD/MOV/ORR rd,rn,rm`) register 31 is **XZR**, but in immediate/load-store
  forms (`ADDI`, `LDR/STR [rn,#off]`) it is **SP**. `ADD x15, sp, x11` silently
  used XZR → garbage address → handler faulted → re-entered → hung. To get SP into
  a GP reg use `ADDI rd, sp, #0`, never `MOV`/`ADD`-register.

## Self-host is the real frontier — decomposed, not faked (2026-06-10)

- The standalone `src/cg/forth.fs` is a 300-line stencil-JIT native Forth with
  only `+ - * dup drop swap .` + int literals. "Standalone IS caf" (run the full
  checker + ICode codegen natively, stage2==stage3, drop gforth) is a multi-week
  bootstrap, not one increment. Decomposed into 10 ordered sub-dots (core words →
  memory → strings → locals/catch → wordlists → port arena/types → port checker →
  port codegen → in-process code-allocator → stage2==stage3). Don't fake a fixpoint.
- **Adding standalone primitives is cheap because they ARE caf ICode.** Each prim
  is a niladic word emitting icode.fs mnemonics on the x19 data stack (`b+` =
  `B g-pop A g-pop A A B ADD, A g-push`); FPRIM registers start/RET/end labels for
  stencil inlining. Milestone 1 added the comparison/logic/shift/`/`/`mod`/shuffle
  set this way — golden-tested through the standalone's own REPL (NF/NF=). Control
  flow (IF/THEN/BEGIN) is the harder remaining piece: the stencil JIT must patch
  relative branches across inlined copies.
- **Milestone 1b (control flow) landed.** The standalone's `:` compiler now emits
  IF/ELSE/THEN + BEGIN/UNTIL/AGAIN/WHILE/REPEAT by keeping a control-flow stack at
  a reserved region offset (CFSTK-OFF) and patching each branch's relative offset:
  forward branches (IF/WHILE) emit a CBZ placeholder and record its address;
  THEN/REPEAT compute `(CP-addr)/4` and OR it into the recorded word. One `Lpat`
  routine auto-detects CBZ (imm19, bit31 set) vs B (imm26) so it patches both.
  Backward branches (UNTIL/AGAIN/REPEAT) emit the offset directly (target known).
  Keyword dispatch is a tiny `Lkwcmp` (case-folded) over embedded lowercase
  keyword bytes. Verified through the standalone REPL: ABS, SGN (nested IF/ELSE),
  counted BEGIN/UNTIL, and BEGIN/WHILE/REPEAT sums all compute correctly.
- **Milestone 2 (memory + data space) landed.** `@ ! c@ c! cells here allot , c,`
  as stencils, plus CREATE/VARIABLE as interpret-mode defining words (reuse the
  `:` slot pattern + `c-lit` to emit a push-address body). **Key gotcha:** the
  data space CANNOT live in the code mmap — `;` mprotects the whole region to RX,
  so a later `!` to data bus-errors (writing an executable page). Data needs a
  SEPARATE always-RW mmap. x20 (RBASE) is dead after the startup seed-dict copy,
  so it's repurposed as the data base; `[x20]` holds DP. Verified: variables,
  arrays via `create…cells allot`, and read-modify-write all compute correctly.
- **Milestone 3 (strings) landed.** `S" …"` (compile mode) parses the string from
  the input cursor (not the whitespace tokenizer), embeds the bytes in the code
  image, emits a `B` over them, and pushes the bytes' ABSOLUTE address (known at
  compile time — no PC-relative ADR needed) + length via `c-lit`. `TYPE` is the
  `write(2)` syscall (#4). Gotcha that cost a debug cycle: `Lpat` clobbers x10, so
  keeping `len` there made the length push garbage — stash live values in
  registers the callee (Lpat/c-lit) doesn't touch, and patch the B BEFORE the
  literals are emitted (its target is the push site) while preserving len in x15.

## Register-resident DO..LOOP — caf ties clang -O3 (2026-06-10)

- **The spill was NEVER the loop bottleneck — the missing back-edge register
  liveness was.** The memory-path loop spilled the carry every iteration, but
  that store/load was hidden under the xorshift latency chain; making the carry
  register-resident alone changed nothing. caf stayed at 1.94× clang until the
  carry actually stayed in a register *across the back-edge*. Then it tied clang
  -O3 (0.20s/1e8 iters, exit 221). Measure the real critical path, don't assume.
- **Mechanism:** pre-scan the body; if every token is VS-safe (a CG-VS prim, `I`,
  or a literal) and there's no nested loop, pin the carry into fixed register
  homes at loop entry (`carry-snap`), walk the body register-resident, and at the
  back-edge parallel-move the carry-out regs back into those homes (`carry-recon`,
  cycle-safe via a T0 scratch — handles SWAP/ROT carries). Anything else (IF,
  `.`, `>R`, nested DO) makes the body non-straight-line → speculative
  `cg-snapshot`/`cg-rollback` cleanly reverts to the proven memory path.
- **A linear peephole optimizer is UNSOUND across a loop back-edge.** COPY-PROP +
  `REG-DEAD-AFTER?` treated the loop-top LABEL as "register dead after here," so
  it killed `mov x28,limit` while the back-edge `cmp x27,x28` still read x28 →
  loop ran ~0 times, wrong result, silently (correct-looking on tiny inputs).
  Fix: PIN the loop-carried regs (x27/x28 + carry homes); pinned regs are never
  proven dead and never have a copy coalesced into them. The same boundary-as-
  dead assumption was already latent-unsound for forward fall-through.
- **Correct results do NOT prove the fast path ran.** Both paths are correct;
  only disassembly (`otool -tv`) or an op-count probe shows whether the carry is
  register-resident. A stack-underflow bug in the `?DO`/`DO` guard (`2over` on a
  3-item stack) silently sent every loop to the memory path while all tests
  passed. Verify the generated code, not just the answer.

## Floating point — floats on the data stack (2026-06-10)

- **One model decision unlocked everything: an f64 is ONE data-stack cell holding
  the IEEE-754 bits** — no separate FP stack (unlike hosted gforth). DUP/DROP/
  SWAP/@/!/literals all "just work" on floats for free; only the arithmetic prims
  (F+ F- F* F/ FNEGATE FABS FSQRT, the compares, S>F/F>S) are new. They move the
  cell bits X→D (`FMOVXD`), compute in the D-register file, move back D→X
  (`FMOVDX`) into a pool register. D0/D1 are scratch FP regs.
- **Float literals MUST be classified BEFORE `NUMBER?`.** `s>number?` parses
  "2.0" as a *double* (n=2), so `NUMBER? nip nip` is truthy and would steal it as
  i64. Gate on an FP marker ('.', 'e', 'E') + `>float` success, ahead of the
  integer clause, in BOTH the checker (`CHECK-FLOAT`) and codegen
  (`EMIT-FLOAT`). Reinterpret bits via a scratch `f!` then `@`.
- **FP-register residency, not a second stack, is the perf answer.** caf is
  statically typed, so the checker already separates `f64` from `i64` — the job a
  classic Forth FP stack does at runtime. Keep the unified data stack; add a third
  VS tag `V-FREG` (value lives in a D-register) so chained ops (`F+ F* F-`) stay
  in the D-file. `v-popd` FMOVs a non-resident operand in; results push V-FREG;
  `v-popr`/`v-spill` FMOV out only at a GP consumer or control-flow boundary.
  Measured: `2.0 3.0 F+ 4.0 F* F>S` emits 0 FMOVDX (down from 2) — only the
  unavoidable literal loads remain. The shuffle/spill words must each free/copy
  the right pool (`d-free`/`FMOVDD` for V-FREG) or floats leak D-regs.
- **FP condition codes differ from integer.** After `FCMP`: F< → `MI` (N set),
  F> → `GT`, F= → `EQ`; F0< → `MI`, F0= → `EQ`. (Integer `<` uses `LT`.)
- **Adding a TC-* type touches FOUR renderers or CHART throws 1495
  (E-BADTYPE):** sigparse `CON-CODE`, render `CODE$`, diag `TY-NAME`, plus the
  config code itself. Miss `render.fs CODE$` and even `PRIM` fails at chart time
  (it renders the canonical sig). No gforth-FP oracle exists for our model, so
  test via `F>S`→int exit codes.

## Environment

- Homebrew ships Gforth **0.7.3** only; "0.7.9" is the unreleased dev branch and
  needs a source build from git.
- **caf targets 0.7.9**, built from `git://git.savannah.gnu.org/gforth.git`
  (version `0.7.9_20260513`), installed at `~/.local/bin/gforth`. 0.7.3 also
  present at `/opt/homebrew/bin/gforth` — ensure `~/.local/bin` precedes it on PATH.

## Building Gforth 0.7.9 on macOS ARM

- `configure` **rejects Apple clang** ("long unfixed bug, use GCC"). Must build
  with real GCC: `./configure CC=gcc-15` (brew `gcc`). `/usr/bin/gcc` is clang — fails.
- Need GNU autotools + texinfo + GNU libtool from brew: `autoconf automake libtool texinfo`.
  Run `glibtoolize` (not Apple `libtool`); `./autogen.sh` picks it up via `LIBTOOLIZE=glibtoolize`.
- texinfo + m4 are keg-only: prepend `/opt/homebrew/opt/texinfo/bin` and
  `/opt/homebrew/opt/m4/bin` to PATH for `makeinfo`.
- Full recipe: `./autogen.sh && ./configure --prefix=$HOME/.local CC=gcc-15 && make && make install`.
  `install-info` prints harmless "excess command line argument" warnings.

## Gforth 0.7.9 gotchas (verified against the binary)

- **`echo '…' | gforth -e bye` swallows stdout.** Run via a `.fs` file
  (`gforth /tmp/x.fs -e bye`) whenever output matters.
- **`gforth … -e bye` exits 0 even when a `T{}T` assertion fails.** Do NOT use
  the exit code as a pass/fail signal — maintain a failure counter and
  `(bye)` with an explicit code (see `PLAN.md` Test strategy).
- **`IF`/`ELSE`/`;` are compile-only** — all conditional/diagnostic logic must
  live inside `:` definitions, never at the top level.
- **Hayes tester ships with gforth** at `share/gforth/<ver>/test/{tester,ttester}.fs`
  but is **not** on the `require` path — vendor it by copying into the repo.
- **`parse-name` returns a transient `(c-addr u)`** invalidated by the next
  `s"`/`."`/`refill` — `move` the bytes out immediately before parsing again.
- **`s>number?` returns a double** `( c-addr u -- d flag )` — narrow with `d>s`.
- Verified working: locals `{: a b :}`, `RECURSE` (incl. through `EVALUATE`),
  overriding `:` via saved xt + reentrancy flag, `outfile-execute` output
  capture, `wordlist`/`set-current`/`create ,`/`search-wordlist`/`>body`.

## Process

- **One concern per file.** Don't bundle unrelated responsibilities in a single
  file (e.g. `sig.fs` was about to hold parser + renderer + DB + primitive
  table). It hurts readability AND blocks parallelism — split at responsibility
  seams (`render`/`sigparse`/`db`/`prims`) and those build concurrently.
  Co-locate only things that change together or share one hook. **Why:** the
  user flagged the over-stuffed file; finer files = more parallel agents + easier
  review. **How to apply:** when a file's description lists "X + Y + Z" of
  distinct kinds, that's a smell — make X, Y, Z separate files with explicit deps.

## Debugging — never diagnose at the gforth interpret level (BLOCKING)

The single biggest time-sink in the Stage-2 build was **broken diagnostics**, not
broken codegen. Compile-only constructs read GARBAGE when typed at the REPL /
`gforth -e`, silently, with no error:
- `[']` is compile-only → use `'` interpretively (or wrap in a `:` def).
- `i`/`j` and `{: … :}` locals only work inside a colon definition.
- `>r … r@ … rdrop` straddling a `do … loop` returns the **loop index** from
  `r@`, not your saved value (the loop owns the return stack).

Each of these made a probe "prove" a codegen bug that did not exist (mis-bound
label offsets, missing prim bodies). **Rules:** (1) wrap any multi-step
diagnostic in a `:` definition and call it; (2) prefer the dogfooded inspectors
in **`src/cg/inspect.fs`** (`ICDUMP`, `ICSCAN`, `?LBL`, `ICAT`) and the
**`test/nf.fs`** harness (`NFX` = build+run+show, `NF-RUN`/`NF=`) over hand-rolled
one-liners; (3) for ground truth on an emitted binary, use the EXTERNAL oracle
`otool -tV <file>` — it has no caf-tooling bugs. **Don't fight caf's tooling;
improve it** — when a one-liner is awkward, add a tested word to `inspect.fs`.

The type checker would NOT have caught these: it checks the stack *effect* of caf
source, not the *value* semantics of a return-stack/`do-loop` interleaving, and
throwaway `gforth -e` snippets are never run through it at all. The fix is
discipline + reliable tooling, not a new checker feature.

## Case-insensitivity (gforth, and caf itself)

- **gforth is case-insensitive**, so a `{: decl :}` local collides with a
  `variable DECL` — `decl DECL !` stored at the local's value (a type term) →
  "Invalid memory address". Never name a local the same as a global ignoring case.
- **caf should be case-insensitive too** (it checks Forth). Word/type lookups go
  through `search-wordlist`/`find-name` (already CI). Keyword/type-name matching
  must use a CI compare, not `compare`. The ONE case-meaningful element is the
  single-letter signature var: lowercase = type var, uppercase = row var — and
  that's unambiguous because type names are ≥2 chars, so length disambiguates.

## Test hygiene (shared gforth image)

All tests run in ONE gforth image, so global state leaks between test files —
per-file tests can pass while the combined `all.fs` suite fails. Rules:
- **Never `is` a real seam defer in a test** (e.g. `is OCCURS-TYPE`) — it stays
  re-pointed and breaks every later test. Use a throwaway `defer` to exercise the
  mechanism. (This corrupted `unify` across the suite.)
- **Tests that build raw-id terms and then resolve must `TV-CLEAR RV-CLEAR`
  first** — `TV-RESET`/`RV-RESET` only zero the NEXT counter, not the bind arrays;
  a prior file's bindings on ids 0,1,… leak in. Production is safe (the checker
  only touches `TV-ALLOC`-cleared ids). (This broke `t-render` after `t-unify`.)
- **Don't chart names that collide with the primitive table** (re-`create` warns
  and the warning fails an empty-output check).
- **Always run the integrated suite**, not just per-file — it's the only thing
  that surfaces cross-file leakage.

## Implementation findings (build of the checker)

- **Nested parens break `( … )` stack comments** — the inner `)` closes the
  comment early (`( (R0,i64) )` leaves a stray `)`). Never put parens inside a
  comment; write `( R0 i64 )`.
- **zsh doesn't word-split unquoted `$VAR`** — `gforth $SRC …` passes the whole
  string as one filename. List the `.fs` files explicitly (or `${=SRC}`).
- **Tag-sentinel collision (design):** any tagged cell stored in a slot where
  `0` means "unbound" must encode to nonzero. Row tag `S-ROW=0` made `MK-ROW 0`
  (row var id 0) equal `UNBOUND` → fixed by `S-ROW=1 S-PUSH=2`. Type vars were
  safe only because `T-VAR=1`. Keep tags that can carry payload 0 nonzero.
- **Clean error-code tests:** wrap the failing sequence in a `( -- )` named word
  and use `' WORD catch` → a clean `( code )`. `catch` restores the data stack to
  its depth when `catch` ran; operands built *inside* the word vanish on throw,
  so no stray operands are left (unlike building them before `catch`).
- **Mutual recursion:** `defer` the seam (in `forward.fs`) + `is` it later for
  cross-file; `RECURSE` for a word recursing on itself; a file-local `defer`+`is`
  for two mutually-recursive words in the same file (used for occurs).
- **Scheme persistence:** a scheme is a canonical signature **string** (DB holds
  strings); `INST`=re-parse (fresh vars by name per call → polymorphism for free),
  `GENERALIZE`=render. Sidesteps copying terms out of the per-check arena.

## caf REPL + TUI (Forth, dogfoods the checker, 2026-06-10)

Two front-ends, both Forth (no C/Zig — the Zig `~/Work/pz` is reference only):
- **`src/repl.fs`** (`caf-repl.fs`) — line REPL. Enter a checked def → `✓ NAME (
  effect )` or the caf diagnostic; non-def lines `EVALUATE`. Reads stdin via
  `stdin read-line` (NOT `refill`, which reads the `-e` eval source and hits EOF).
- **`src/tui.fs`** (`caf-tui.fs`) — full-screen TUI with **as-you-type** feedback:
  `RUN-TUI` raw-modes the terminal (`stty raw -echo`, restore `stty sane`),
  single-line live editor (`caf> <buf>   <status>`, horizontal positioning only —
  robust everywhere), and on each keystroke shows the inferred effect / diagnostic
  via **`CHECK-DRY`** (checks WITHOUT charting — refactored `CHECK-DEF` into a
  non-charting `CHECK-CORE`; else every keystroke pollutes the effect DB).

- A new `CHECK-CODE` var in `colon.fs` lets the REPL read the last `:`-outcome
  (-2 none / 0 ok / err code) so success shows the effect; failures self-report.
- Define REPL/TUI infra with **`CHECKING-ON? off`** then re-enable at file end —
  `( -- )` parses as a valid sig, so otherwise the helper words chart themselves
  as `R -- R` and clutter `WORDS`.
- The raw key loop needs a real tty; gate it behind `TTY?` (`test -t 0` via
  `system`/`$?`) and fall back to a message on a pipe. Test the parser +
  `CHECK-DRY` (no tty needed); the key loop is thin glue exercised by hand.

## Register allocation — store-forwarding, and the register-reuse wall (2026-06-10)

`opt.fs` gained two block-local passes (`STORE-FWD`, `X19-CANCEL`): forward
`STR→LDR` to register MOVs, kill overwritten/unobserved stores (DSE), then cancel
the orphaned inverse `ADDI/SUBI x19` pairs. Boundaries (label/call/branch/generic
memory op) flush tracking; a slot's forward-register is dropped whenever that
register is redefined (the correctness lynchpin — backstopped by the differential
native-exe suite). Real wins on multi-value shuffles: `OVER +` 13→9, `SWAP OVER -`
18→14, `DUP ROT * +` 22→18 (~20-30%).

The dup-heavy hot loop (xorshift) didn't improve under store-forwarding — the
**register-reuse wall**: `DUP 13 LSHIFT XOR` loaded `h`→x9, stored the dup, then
`LSLI x9` reused x9, so the dup'd `h` survived only in memory; forwarding the
reload was (correctly) impossible. Root cause: every `templ.fs` primitive
hardcodes `T0/T1/T2`, so distinct live values collide.

**Fixed — abstract register stack (`src/cg/regstack.fs`).** A compile-time value
stack whose entries are POOL registers (`x13-x15,x20-x24`) or CONSTANTS; pure
arithmetic/shuffle primitives operate on it with NO memory traffic (DUP copies to
a fresh register, so the copy survives later ops). `walk.fs` SPILLS the whole VS
to memory before anything that isn't a VS primitive (control flow, calls, return-
stack ops, `>R`, unsupported words) — so those keep the proven memory path
unchanged; correctness is isolation-by-spill + the checker's branch balance. The
VS folds constants and selects immediate shifts itself (subsumes the old `CTS`).
Result: `DUP 13 LSHIFT XOR DUP 7 RSHIFT XOR DUP 17 LSHIFT XOR` **25→13** ops (and
pre-opt == post-opt — the allocator emits near-optimal code directly); `DUP DUP *
*` 15→8, `DUP *` 8→6. Full differential suite green (control flow, recursion, CLI).

- **Wordlist-collision footgun:** the VS comparison helper was first named `g-cmp`
  — but `templ.fs` already defines `g-cmp` in the FORTH wordlist, and a CG-VS word
  referencing `g-cmp` resolves it via the SEARCH order (which has templ's, not
  CG-VS's), silently calling the old memory version. Renamed to `vcmp`/`vcmp0`.
  When a new wordlist's words call helpers, give the helpers names that aren't
  shadowed in the search order.
- **Invariant:** no two VS entries name the same register (DUP/OVER emit a copy),
  so reusing a popped operand register as an op's result is always safe.
- **Shifted-operand fusion (B3, done).** With values register-resident, an
  in-place immediate shift feeding an ALU op (`LSLI rd,rd,#k ; EOR rx,ry,rd`, rd
  dead after) fuses to `EOR rx,ry,rd,LSL #k` — one instruction, matching LLVM.
  Mechanism: `SHIFT,` packs (type,amount) into the ALU record's `IC-D`; the `RRR`
  encoder ORs in bits [23:22] (type) + [15:10] (amount); `d=0` = `LSL #0` so
  unshifted ops are unaffected. Done as an opt.fs peephole (`OPT-SHIFT-FUSE`,
  reusing the store-forwarding liveness/boundary machinery) rather than threading
  a shifted-register kind through the value stack — far less surface. Result:
  xorshift chain **13→10**.
- **MOV-coalescing / copy propagation (done).** `OPT-COPY-PROP` turns the DUP-copy
  of an in-place self-op (`MOV r2,r ; EOR r,r,r2,LSL#k`) into `EOR r,r,r,LSL#k` —
  propagate the copy into the reader, kill the MOV when the copy is dead after.
  **Footgun:** the reader of the copy is often *also* the instruction that
  redefines the copy's source (`EOR r,r,r2` writes `r`=source) — that's a
  read-before-write, so check the reader case BEFORE the "source redefined → stop"
  guard, or it never fires. Result: **xorshift 10→7** (`eor r,r,r,lsl#k` ×3 +
  load/store framing = LLVM hand-asm parity; full pipeline **54→7**, ~7.7×). The
  last lever is loop-invariant allocation across the back-edge.

## AOT locals + the u8/u32 "typed payoff" reality (2026-06-10)

- **Locals (`src/cg/cglocals.fs`) — done.** `{: a b :}` lowers to a per-word
  FRAME (`LOCSZ` bytes carved BELOW the data stack in `g-prologue`, addressed
  `[sp,#slot*8]`). At the opener: spill the VS, pop the named inputs into slots;
  a name use LDRs its slot onto the VS. Because locals are in memory (not the VS),
  they **survive spills** — so they read correctly across IF/ELSE and loops (the
  key correctness property; tested). Hooked into `EMIT-TOKEN` before the
  number/prim classification via `CHECK-LOCAL-CG`; reuses the checker's
  `BRACE-OPEN?`/`NAME-PART`/`CI=`. Inputs-only (v0), matching the checker.
  **Layout gotcha:** locals at `[sp,…)` would alias the data stack (`Xds=sp`) —
  `g-prologue` now sets `Xds = sp+LOCSZ` so the frame sits below the data stack.
- **u8/u32 width is correct by construction — no typed selector needed.** The
  plan assumed fixnum tags/boxing and a typed-stencil bank; caf never had tags
  (values are full 64-bit cells, like gforth), and the register allocator makes
  registers IR fields (no frozen stencils to specialise). gforth's model is
  "full-cell arithmetic, truncate only at a byte store" — caf matches it exactly:
  `c@`/`c!` use `LDRB`/`STRB` (zero-extend / low-byte). So the "typed payoff"
  (unboxing) is free, and width truncation happens at the right point already. No
  `UXTB`/`UXTW` in registers required for correctness.

## Speed gate CLOSED — the bar is LLVM, not gforth (2026-06-10)

gforth performance is not the competition; **LLVM (`clang -O3`) is the bar** a
native backend must rival. `bench/dispatch.{fs,s,c}` — a dispatch-bound byte-mix
(load + add + 3× `dup <<n xor`, ~10 cheap ops/byte, 983 M iters); the serial
xorshift chain is latency-bound so NEITHER side can vectorize — a fair
head-to-head. Measured on this M-series box:

| build | ns/iter | note |
| ----- | ------- | ---- |
| **clang -O3 (LLVM — the bar)** | **2.252** | optimized C |
| hand ARM64 (what caf targets) | 2.256 | LLVM **parity** (same exit 203) |
| clang -O0 (unoptimized C) | 7.21 | — |
| gforth-fast (its own native codegen) | 2.27 | parity too |
| gforth threaded (caf-checked runs on) | 23.87 | 10.6× slower — irrelevant baseline |

**Verdict: caf's native target matches LLVM -O3** on this loop. Caveat: the hand
baseline fuses `dup <<n xor` into one shifted-EOR; caf's current backend emits
separate shift+xor through the stack, so caf's *real* output sits above 2.25 —
the shifted-operand-fusion peephole (dot) closes that last gap to LLVM. Always
measure against LLVM `-O3`, never gforth.

**Constant folding (`src/cg/walk.fs`).** Literal arithmetic folds at compile time
via a compile-time value stack: numbers are deferred (not emitted); a foldable op
over pending constants folds them; any other token first flushes them as g-lits
(so runtime values never mix with deferred ones). `3 4 + 5 *` → one `LIT 35`.
Only ops whose gforth semantics match the emitted ARM64 are folded (NOT `/`,`MOD`,
`2/` — division rounding / shift signedness differ). A **constant shift amount**
over a runtime value (one pending const at `LSHIFT`/`RSHIFT`) emits an *immediate*
shift (`lsl/lsr #k`, a UBFM alias — the disassembler shows raw `.word`) instead
of materialising `k` + a register shift. Full shifted-operand *fusion* (fold the
shift into the next `EOR`/`ADD`/… — the last gap to LLVM on the dispatch loop)
needs ICode ALU records to carry a shift field — a bigger IR change (dot B3).
**Footgun:** `s>number?`
leaves its double on the stack *even on failure* — the non-number path must
`2drop` it or a garbage "token" reaches the encoder (E-NO-ENC).

**Dogfooding the codegen (the honest limit).** The codegen proper (`EMIT-TOKEN`,
the value stack) is metaprogramming — `s>number?`, `search-wordlist`, `execute`,
raw stacks — so it cannot be written as fully *checked* caf. But the bug class
that actually bit the build IS catchable: chart `s>number?` as `( R str -- R i64
bool )` (the double modeled as one i64) and the checker's "both IF arms must
leave equal stacks" rule rejects the exact mistake — a branch that consumes the
flag but forgets the value — with `E-BRANCH` (`test/t-dogfood.fs` proves
`S>NUMBER? IF DROP THEN` is refused, `S>NUMBER? IF THEN` accepted). So: the
codegen stays trusted, but the *failure mode* is now expressible and demonstrably
caught — which is the real value the dot asked for.

## Codegen Phase 0.3 — speed gate (superseded by the dispatch bench above)

Serial scalar LCG (`x = x*A + C`, 1e9 iters; `bench/inner-loop.{fs,s}` +
`bench/run.sh`). **Native baseline must be hand-written ARM64** (`inner-loop.s` —
the exact `mul; add` caf emits), NOT clang `-O2` C: clang unrolled/scheduled to
0.97 ns/iter, flattering native; the faithful naive loop is **1.26 ns/iter** and
**unroll-8 is identical (1.26)** → the loop is **latency-bound** (mul→add serial
chain; unrolling buys nothing).

| Build | ns/iter | native advantage |
| ----- | ------- | ---------------- |
| native (hand ARM64, real floor) | 1.26 | — |
| gforth-fast | 2.08 | **1.66×** |
| gforth threaded (= caf today) | 5.98 | **4.76×** |

**Two lessons.** (1) **Don't measure the native ceiling with C** — clang's
optimizer ≠ what caf emits; use hand-asm. (2) **The LCG is the WRONG gate
benchmark**: it's latency-bound (2 serial ops, `mul` latency irreducible and
shared by both engines), so native's real win — eliminating per-op threading
dispatch — is a small fraction → only 1.66× over gforth-fast. The plan specified a
**dispatch-bound** loop ("decoder/VM step, arith + `@`/`c@` + a branch") where
gforth pays NEXT per cheap op and native collapses them to register ops → expect
3–10×. **Gate is PENDING a dispatch-bound re-bench.** Decisive even now: **4.76×
over the threaded engine caf actually uses**; the 2× bar is only contested vs
gforth-fast (an engine caf doesn't use) on an unfavorable loop.

**asm gotcha:** in clang's integrated ARM assembler **`;` is a comment**, not a
statement separator — `mul …; add …` silently drops the `add`. Put one
instruction per line. (Caught a fake "0.16 ns/iter, 8× speedup" = dead muls.)

### Dispatch-bound bench — gforth-fast IS a native engine (2026-06-10)

`bench/dispatch.{fs,s}`: xorshift byte-mix over 64KB × 15000 (~10 cheap ops/byte),
ns/byte (all three agree, low byte 203):

| Build | ns/byte |
| ----- | ------- |
| native (hand ARM64) | 2.28 |
| gforth-fast | 2.21 (**parity with native**) |
| gforth threaded (plain) | 23.69 (10.4× slower) |

**gforth-fast uses dynamic superinstructions / native-code copying — it compiles
to native (~0.2ns/op << one NEXT).** So hand-asm is at PARITY, not ahead. Hard
consequences:
- **caf-checked words are gforth colon words → running caf under `gforth-fast`
  already gives native speed for free, no backend.** A backend that only matches
  gforth-fast adds nothing on speed.
- The "unboxing" win doesn't apply vs gforth: gforth cells are raw/untagged
  (nothing to unbox — that was a *habu* fixnum-tag cost, not gforth's).
- Even register-kept accumulator (asm) = parity; gforth-fast's stack ops are
  L1-cheap.

**Reframe the speed gate:** "≥2× over gforth-fast" was wrong — gforth-fast is a
native engine, not a threaded strawman. The backend's justification is the
committed **self-host / standalone (gforth dropped)** goal, which needs native
codegen regardless of speed; parity-with-gforth-fast + 10× over threaded is a
fine target for a standalone. Beating gforth-fast is a later *bonus* (cross-word
regalloc, type-specialized width — look modest here), not a gate. Don't bank the
project on outrunning gforth-fast.

## Codegen working end-to-end (2026-06-10)

caf generates ARM64 machine code on the Mac, in Forth, no C. Pipeline:
Forth body → `cg/templ.fs` (tokenize, stack-op generators over Xds=x19) →
`cg/icode.fs` IR → `cg/opt.fs` peephole → `cg/asm.fs` encoders → `cg/macho.fs`
(dynamic Mach-O in a buffer) → `cg/exec.fs` (write + `codesign -f -s -` +
`system`-run). Proven: `test/t-cg-exe.fs` (exit/add/mul/loop/stdout),
`test/t-cg-word.fs` (`DUP *`→square, `3 + 2 *`, `DUP OVER + +`, …).

Mach-O / exec findings:
- gforth `$?` is the raw **wait status**; exit code = `8 rshift $FF and`.
- `codesign -f -s -` adds the ad-hoc `LC_CODE_SIGNATURE` into **header slack** —
  leave the entry at file offset 0x1000 (cmds end ~440) so there's room; it also
  extends `__LINKEDIT` (segment at 0x4000, file padded to one page).
- libSystem LC_LOAD_DYLIB: ts=2, current=1356.0.0 (`$054C0000`), compat=1.0.0
  (`$00010000`); dylinker `/usr/lib/dyld`; `__text` flags `$80000400`; header
  flags `$00200085` (incl. PIE). Static binaries are SIGKILLed — dynamic only.
- Data-stack model: reserve on the machine stack (`sub sp,sp,#256; add x19,sp,#0`),
  push=`str reg,[x19]; add x19,x19,#8`, pop=`sub x19,x19,#8; ldr reg,[x19]`.
  `mov xN,sp` must be `add xN,sp,#0` (reg 31 = SP only as ldr/str/add base; it is
  XZR for `mov`/logical).

**Remaining for fully-standalone (gforth dropped):** wire `CODEGEN-HOOK` so live
`:` definitions auto-compile (read `CAP$`); broaden the op set (control flow,
combinators, locals); then Part F — a native Forth runtime (interpreter +
dictionary + `evaluate`) so the artifact self-compiles without gforth (the
stage2≡stage3 fixpoint). Part F is the genuine long pole.

## caf is a working native AOT compiler (2026-06-10)

caf (hosted on gforth) compiles checked Forth to **standalone ARM64 macOS CLI
executables** — no gforth at runtime, no C, no LLVM. `src/cg/`:
`icode` (IR+mnemonics) → `opt` (peephole) → `asm` (encoders) → `templ` (prim/
control generators) → `walk` (tokenize body) → `link` (subroutine ABI, deps,
multi-word + MAIN) → `rt` (`.`/atoi runtimes) → `macho`/`exec` (emit+sign+run).
Wired to the checker via `CODEGEN-HOOK` (`forward.fs`/`colon.fs:80`), gated by
`CODEGEN-ON?`. Front door: `s" /tmp/sq" CAF-EXE SQUARE` → `./sq 12` prints 144.

Supported subset (tested, `test/t-cg-*.fs`): DUP DROP SWAP OVER NIP, + - * / MOD
1+ 1- NEGATE, AND OR XOR, < > = <= >= <> 0= 0< 0>, IF/ELSE/THEN,
BEGIN/UNTIL/AGAIN/WHILE/REPEAT, ?DO/DO/LOOP/I, EXIT, RECURSE, word→word calls,
`.` (signed-decimal print). Verified standalone: `rfact 7`=5040, `sumto 100`=5050.

Gotchas hit:
- **Non-leaf detection must count every BL-emitting token** (`.`, RECURSE, a
  callee), else x30 isn't saved and the word's `RET` jumps to itself → infinite
  loop. (Disasm caught it.)
- **`mov xN, sp` is `add xN, sp, #0`** — reg 31 is SP only as a ldr/str/add base;
  it's XZR for mov/logical.
- **Load cg under `CHECKING-ON? off`** — its `( idx -- u32 )` comments parse as
  caf sigs and locals confuse the override; cg is infra, not checked caf.
- **`EMIT-PRIM` must throw silently** (no diagnostic print) — the codegen hook
  validates by catching it, so printing leaks during normal skips.
- **Subroutine ABI:** Xds (x19) is a global threaded through calls (push/pop
  mutate it, never restored); non-leaf words save/restore x30; args/results live
  on the Xds data stack; CLI entry gets x0=argc, x1=argv (save argv in x22).

**Still gforth-hosted:** the COMPILER runs on gforth. Fully standalone (compiler
self-hosts, gforth dropped) needs Part F — a native Forth runtime (interpreter +
dictionary + evaluate). That remains the long pole.

## Part F — standalone native Forth interpreter (2026-06-10)

`src/cg/forth.fs` emits a **standalone native Forth** (no gforth, no C): a Mach-O
with a dictionary + subroutine-threaded primitives + an outer interpreter that
parses an embedded source line, number-pushes, FINDs, and EXECUTEs. Proven
(`test/t-cg-forth.fs`): `2 3 + .`→5, `10 20 + 5 * .`→150, `8 3 swap - .`→-5.

Design choices that worked:
- **PC-relative throughout** (PIE-safe): non-PIE binaries still get ASLR'd on this
  macOS (load base ≠ VMBASE — measured), so bake nothing absolute. Code base
  `RBASE = ADR(anchor)` at startup; dict stores **code byte-offsets**, EXECUTE =
  `RBASE + offset` → BLR. Data (strings, dict) embedded via new ICode pseudo-ops
  `BYTES,`/`DCQ,`/`DLBL,` (DLBL = a cell holding a label's byte offset), reached
  by `ADR`.
- **Subroutine-threaded**: primitives are native routines ending in `RET`;
  EXECUTE is `BLR`. No IP/NEXT/DOCOL engine needed.
- Bug that cost time: a routine that **bakes a count must run after the thing it
  counts** — `emit-find` baked `#PRIMS` but ran before `emit-prims` → baked 0 →
  FIND looped zero records → silent no-op. Emit prims first.

Registers: x19=DSP, x20=RBASE, x21/x22=input ptr/end, x23/x24=tok addr/len.

**Stage 2 (runtime `:`/`;`) — BUILT & working.** The emitted Forth JITs new words
into an `mmap`'d region by INLINING stencils: each token's machine code (a
primitive's body, or a prior word's, both minus the trailing `RET`) is copied
into the new word, so compiled words are fully flattened/leaf — no calls, no
`x30` save needed, and BL-range to `__TEXT` never matters. Literals compile to a
`movz/movk` + push stencil. `5 SQ .`→25, 4-level nesting (`C=B B`,`B=A A`,
`A=DUP *`: 2→65536) all pass (`test/t-cg-forth.fs`).

- **JIT memory on Apple Silicon: W^X, pure syscalls.** Plain `RWX` mmap (`prot=7`)
  faults on execute (verified: exit 0/garbage). `MAP_JIT` needs the jit
  entitlement + a libSystem call (`pthread_jit_write_protect_np`) — avoided. The
  working recipe (proven by a probe → exit 25): `mmap` **RW** (`prot=3`,
  `MAP_ANON|MAP_PRIVATE=0x1002`) → write code → `mprotect` **RX** (`prot=5`) →
  flush → execute. No entitlement, no C, no `pthread_*`.
- **Cache flush is mandatory & ordered:** per 64-byte line `DC CVAU` (clean
  dcache to PoU — new `DCCVAU,` op, `0xD50B7B20|Rt`), then `DSB ISH`, then
  `IC IVAU` per line, `DSB ISH`, `ISB`. Do it AFTER the `mprotect` RX.
- **W^X re-toggle bug:** the runtime dict lives in the same region as the code.
  After `;` makes the region RX, the NEXT `:` must `mprotect` **RW _before_**
  writing the new dict slot — else the slot store hits read-only memory. (Cost a
  while: a single def worked, the second produced empty output.)
- **Case-insensitivity bites the emitted FIND.** caf source is UPPER-CASE
  (`DUP`); `emit-prims` registers lower-case (`dup`); a raw byte `FIND` matched
  `+ - *` (no letters) but not `DUP/DROP/SWAP`. Stage-1 tests only ever used
  lower-case source, so it hid for ages. Fix: fold `A–Z→a–z` on BOTH bytes in
  FIND's compare (branchless: `sub #'A'; cmp #26; cset cc; lsl #5; orr`). The
  native Forth must be case-insensitive like gforth + caf itself.

**Stage 3 (read program from STDIN) — built.** `FORTH-REPL-EXE` emits a Forth
with no baked source: it `mmap`s a 1 MB RW buffer and loops `read(fd=0,…)` until
EOF, then runs the same outer interpreter over it. `echo ': SQ DUP * ; 5 SQ .' |
./forth` → 25; multi-line works.
- **Tokenizer must treat newline as whitespace.** Stage-1/2 split tokens on
  **space (32) only**; baked single-line tests never contained `\n`, so it hid.
  Piped/multi-line stdin has `\n` (and `echo` appends one) → tokens glued, FIND
  failed, empty output. Fix: any byte **≤ 32** is a delimiter (`CMP #32` →
  `C-HI` starts a token, `C-LS` ends it). Probe the syscall in isolation first
  (`read` round-trips fine) to localize the bug to the tokenizer, not `read`.

## Type checker — capability vs. coverage (2026-06-10)

When dogfooding the codegen (`src/cg/`) as typed caf, the question arose whether
the checker was lagging. **It wasn't the engine — it was prim-DB coverage.**

- The checker ENGINE is capable: `src/control.fs` already models `IF/ELSE/THEN`,
  `BEGIN/UNTIL/AGAIN/WHILE/REPEAT`, `DO/?DO/LOOP/+LOOP`, `I`/`J` (depth-tracked),
  `EXIT`, `RECURSE`; `src/prims.fs` charts the return stack (`>R/R>/R@/2>R/…`) and
  combinators (`DIP/KEEP/BI/TIMES/EACH/MAP/FOLD`). A `?DO … LOOP` word checks
  cleanly; the earlier review claim that counted loops were unmodeled was wrong —
  verify by test, not by assumption.
- What lagged: the codegen-supported words `MIN MAX 0< 0> 0<> U< U> WITHIN 2DUP
  2DROP 2SWAP /MOD` were **not charted**, so any program using them escaped to
  `E-UNCHECKED` (compiled native, unverified). Fixed by extending `src/prims.fs`.
  Lesson: the checker's reach = the prim DB; keep it in step with what the
  codegen accepts, or checkable code silently slips to unchecked.
- Genuinely untypeable (stay out of checked code): `?DUP` (variadic 0/2 outputs),
  `PICK`/`ROLL`/`DEPTH` (runtime depth). Not worth special-casing yet.
- Dogfooding payoff is real: the session's bugs (encoder `drop` arity, CF-stack
  order) are exactly stack-effect errors the checker rejects. Charting more prims
  widens how much of our own code caf can verify. Effect syntax → `docs/effects.md`.
- Smell noted: `EFFECT-OF` returns `( a u -- ea eu )` when found but a single `0`
  when absent — asymmetric stack effect; callers must `dup 0= if drop …`.
