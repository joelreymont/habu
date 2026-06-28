# Habu tasking — CPU threads (adopts the SwiftForth task interface)

Status: settled design. Validated against the engine by the codex oracle (three
read-only passes) and against the SwiftForth Reference Manual §7 Multitasking.
Fits the existing `habu-swiftforth-language-borrow` direction (Habu already
borrowed `BEGIN-STRUCTURE` from SwiftForth). **Codex final verdict:
GO-WITH-CHANGES** — see "Required invariants" below.

## Build progress

- **Step 0 (cell audit): done** — see classification below.
- **Atomics (step 3): done + validated.** `atomic@` (LDAR `0xC8DFFD29`), `atomic!`
  (STLR `0xC89FFD49`), `atomic-add` (LDADDAL `0xF8E90149`, returns old), `atomic-cas`
  (CASAL `0xC8E9FD6A`, returns witnessed value), `fence` (DMB ISH `0xD5033BBF`).
  Added as JIT leaf primitives in `src/habu/habu1.f` (emitters + `EMIT-MEMORY-PRIMS`)
  with checker effects in `src/core/checker.f` `PT-MEMORY-PRIMS`. The engine
  **rebuilds to byte-for-byte fixpoint** with them (a few KB over the baseline
  engine) via `BF-BUILD-ALL` on a temp engine (shared `bin/hb` untouched), and
  all five ops were **runtime-validated** on an aligned cell.
  - **Constraint found:** LSE atomics fault (SIGBUS) on unaligned addresses, and
    Habu's `variable`/dict cells are **not 8-aligned** (observed addr mod 8 = 6).
    So atomic cells must be explicitly aligned (mmap pages are; `variable` is not).
    A follow-up should add an aligned-cell facility (or `align` before the cell)
    for shared task state; the atomics themselves are correct.
- **Step 1 — data-stack swap: done + validated.** `run-in-stack ( xt base size -- )`
  (`src/habu/habu1.f` `BRUNSTACK`, FPRIM; checker sig in `PT-MEMORY-PRIMS`) runs an
  xt on a **fresh `x19` data stack** (full-ascending, x19=base) and restores the
  caller's stack on return. Engine **fixpoints** with it; runtime-validated
  (`test/run-in-stack-smoke.f`): an xt computed `2 3 +` on the fresh stack and
  wrote a shared cell (=5), and the caller's stack stayed intact (`7 8 +`=15).
  This is the essence of per-task execution — each task its own data stack.
- **Step 1 cont. — x20 region swap: BLOCKED (measured).** A prototype
  `run-in-region` (swap `x20` to a fresh mmap region + `x19`, init `S0`/`RBASE`,
  zero `RSP`/`LOOPSP`/`HND`) **crashes (SIGBUS) even running an *empty* xt** — so
  the fault is not the return-stack init, it is the `x20` repoint itself. The
  engine **mixes absolute DATA-VA references with x20-relative ones**, so pointing
  `x20` at a fresh region (at a different VA) desynchronizes the two and faults.
  This is the concrete form of codex's GO-WITH-CHANGES invariant ("user-area base
  register authoritative for every access").
  **Real blocker / next sub-project:** audit the engine for absolute `DATA-VA`
  references and make **all** runtime cell access x20(base-register)-relative
  before a task can own its region. Until then, only the x19 data-stack swap
  (`run-in-stack`, done) is sound. After the conversion: `run-in-region` →
  pthread trampoline → `TASK/ACTIVATE` + two-task aliasing fixture.
  (The unvalidated `run-in-region` prototype was reverted; it is not in the tree.)

## Decision: adopt SwiftForth's task model, back it with pthread

Modern SwiftForth on Linux/macOS (Reference Manual §7.1.1) already uses **OS
threads**, not a cooperative round-robin: "a thread ... is given its own stacks
and user variables, and is executing code from within the SwiftForth process's
dictionary. However, because the OS controls execution and task switching, there
is no equivalent of the round-robin loop." That is exactly the model Habu needs
for 4×60 Hz acquisition + parallel GPU/DLA inference. We adopt its **vocabulary
and semantics** and back them with `pthread` via the existing FFI.

### Interface (borrowed from SwiftForth §7.2)

Task definition / control:
- `TASK <name>` ( u -- ) — define a task; `u` sizes its user area + stacks;
  `name` returns its Task Control Block (TCB) address. Compile-time; TCB is a
  permanent dictionary entry.
- `<tcb> ACTIVATE <words> ;` — instantiate (OS allocates the private stack frame:
  data + return stacks + user area) and run `<words>`. Used inside a definition.
- `CONSTRUCT ( tcb -- )` — instantiate without starting (optional; ACTIVATE
  auto-constructs).
- `HALT ( tcb -- )` — stop at next yield, stay instantiated (reactivatable).
- `KILL ( tcb -- )` — stop and release memory.
- `PAUSE ( -- )` — relinquish the CPU. For a real OS thread, a loop instead
  *blocks* in a system call (our acquisition loop blocks in the SDK `grab`, the
  detector in `infer`) — SwiftForth explicitly allows "a word that calls a system
  function that blocks the thread" in place of PAUSE.

Thread-local storage — **user variables** (§7.2.1), the proven solution to the
re-entrancy problem below:
- `+USER ( n1 n2 -- n3 )` — define a user variable at offset `n1`, size `n2`,
  return next offset `n3`.
- `#USER ( -- n )` — current user-area size (a VALUE; start offset for new vars).
- `HIS ( task n -- addr )` — address of another task's user variable.
- Executing a user variable adds its offset to the **register holding the current
  task's user-area base**. (Habu already keeps engine cells at `DATA`-relative
  offsets via a base register — see below; user variables generalize that to
  per-task.)

Mutual exclusion (§7.2.2): `GET ( facility -- )` / `RELEASE ( facility -- )`.
SwiftForth spins these on PAUSE; for OS threads we back them with a real
`pthread_mutex` (or prefer lock-free atomics). Adopt the vocabulary, not the
spin.

## The re-entrancy plan (grounded by the codex audit)

SwiftForth §7.1.2: "routines that use global variables are not re-entrant ...
provide private versions of these variables to each task; such variables are
called user variables." Codex's read-only audit found Habu's runtime-mutable
engine cells (all `DATA <X>-CELL`, `src/habu/habu2.f` boot/eval paths). Classify
each (confirm cell-by-cell in Step 0):

**Runtime-touched → become USER variables (per task):**
`S0-CELL`/`XDS` (data-stack base), `RSP-CELL` (return-stack ptr), `LOOPSP-CELL`
(loop stack), `LVD-CELL`/`VSP-CELL` (locals/var state), `HND-CELL` (catch/throw
frame head), `INP-CELL`/`INE-CELL` (input cursor), `TKA-CELL`/`TKL-CELL` (current
token), `EVALD-CELL` (eval depth), `EVALERR-CELL`, `REPLH-CELL` (repl handler),
plus the FFI scratch buffers `FFI-BUF/FFI-FBUF/FFI-STACK-BUF/FFI-DLBUF`
(`lib/ffi.f:9` — "single-threaded scratch buffers ... Do not nest").

**Compile-only → stay process-global (untouched while tasks run):**
`DP-CELL`/`CP`/`NDICT` (dictionary + code cursors, `here/allot/,/c,` via
`BHERE/BALLOT` `src/habu/habu1.f:1003,1403`), `TSIG-*`/`TCSIG-*`/`CRSIG-*` (type
signature buffers), `QPATCH-CELL`, `LOCN-CELL`, `BODYLEN-CELL`, `EXITH-CELL`,
`PEND-CELL`, `DOESB-CELL`, `TRUSTED-CELL`, `VRFREE-CELL` (register allocator).
`lib/memory.f` needs no change (mmap-per-call, no heap cursor).

**Governing invariant:** tasks execute only already-compiled code; **no
compilation while tasks are live.** That is what keeps every compile-only cell
safe. Enforced by a checker/runtime guard: `ACTIVATE`-reachable code must be
fully defined; defining words are illegal in a task body. The dictionary is
shared read-only (matches SwiftForth: "the entire dictionary is shared among
tasks").

## Backing + trampoline (thin C shim)

`pthread_create/join/mutex` via FFI. One static C trampoline in the thin shim
(`void* habu_task_entry(void*)`) installs the task's user-area base register +
stacks, then calls the engine re-entry that executes the task's xt. Per-task user
area + stacks are allocated by `TASK`/`CONSTRUCT` (sized by the `TASK` operand).

## Atomics + memory model

JIT-emitted, aligned cells only: `ATOMIC@/!` (LDAR/STLR), `ATOMIC-ADD` (LDADD),
`ATOMIC-CAS` (CAS / LDXR-STXR), `FENCE` (DMB ISH). Acquire/release + DMB ISH
covers the single-producer/single-consumer hand-off the acquisition→detector
lanes use. Typed checker effects (`addr`/`cell`), `tid`/`tcb` nominal handles.

## Teardown

`KILL` all tasks (or join) before exit; process exit via **`exit_group` (94)**,
never `exit` (93) — the direct fix for the "process lingers because a spawned
thread (CUDA) survives" RCA already seen with the ZED SDK.

## Fixpoint + gate

Threads are a runtime capability; `BF-BUILD-ALL` stays single-threaded →
byte-for-byte fixpoint unaffected. Fixtures: positive (TASK/ACTIVATE/atomics
typecheck + run; a 2-task producer/consumer), negative (defining word in a task
body rejected; atomic on non-cell/unaligned rejected; user-var offset overflow
rejected). Full native gate green, engine self-rebuilt to fixpoint.

## Step 0 audit — complete cell classification (done)

Authoritative source: `src/habu/layout.f` (the whole DATA region). Read/write
sites checked in `src/habu/habu2.f`, `jit.f`, `regalloc.f`, `repl.f`, `lib/ffi.f`.
**Key refinement:** a task only **EXECUTEs a precompiled xt** — it never compiles
*and* never interprets. So *both* the compile machinery and the interpret/eval/
REPL state stay process-global; only live execution state is per-task. That
shrinks the migration set well below codex's initial worst case.

**Per-task (→ user variables) — touched by ordinary execution:**
- `S0-CELL` ($1D0) — data-stack base
- `RSP-CELL` ($568), `RBASE-CELL` ($1C0) — return-stack ptr/base
- `LOOPSP-CELL` ($1C8) + `LOOP-STK-OFF` ($600) region — DO/LOOP index stack
- `LVD-CELL` ($578) + `LVH-OFF` ($580) region — locals values (depth + frame)
- `HND-CELL` (8) — catch/throw handler frame head
- `SSCR-CELL` ($1D8), `GTOD-SCRATCH` ($1E0), `LASTC-CELL` ($560) — execution scratch
- FFI marshalling scratch (`lib/ffi.f`): `FFI-BUF/FFI-FBUF/FFI-STACK-BUF/FFI-DLBUF`
  (ffi.f:9 "single-threaded scratch buffers … Do not nest")
- (debugger, only if used live) `BPA/BPTAB/BPWBASE/BPWN`

**Process-global, COMPILE-ONLY — safe (no compilation while tasks live):**
`DP-CELL`/`CP`/`NDICT`/`DBASE` (here/code/dict); `LOCN/LOCF` + `LOCNAMES`
(locals *names*, compile-time); `CUR`/`WIDN` (current + search-order wordlist —
verified compile/define-only at the CUR/WIDN sites in habu2.f); `BODYLEN`+`BODYBUF`;
`DOESP`/`DOESB`/`CREATEP`; `QPATCH`/`QENT`/`QXH` + `PEND` (pending call target,
`habu2.f:954 "bl entry"`); `DEF-TKA/DEF-TKL/DEF-WL`; `TSIG/TCSIG/CRSIG`,
`TRUSTED`; `PKG-PUB/PRI/PARENT/REC`; `DEFER-META/XT`; `VRFREE`/`FRFREE`/`FRCLM`
(regalloc, jit/recon); `EXITH` (exit-patch chain, compile-time, `habu2.f:949-1043`);
`HOOK` (compiler hook, `habu2.f`); `VSP` (JIT virtual value-stack — **jit.f only**,
not runtime); `SNAPSP`/`SNAP` (snapshot/build).

**Process-global, INTERPRET/EVAL/REPL — safe (tasks EXECUTE, never interpret):**
`INP`/`INE` (input cursor), `TKA`/`TKL` (current token), `EVALD`/`EVALERR` +
`EVAL-FRAME` (re-entrant evaluate), `REPLH` (REPL handler, `repl.f`),
`RSAVCP/RSAVND/RSAVDP/RSAVSP`/`RRECP` (evaluate save frame), `LMAINP`.

**Boot constants — shared read-only (no per-task copy):** `ARGC/ARGV/ENVP`.

This invariant — **tasks neither compile nor interpret, only EXECUTE** — is what
makes the global sets safe and must be enforced fail-closed (next section).

## Required invariants (codex GO-WITH-CHANGES)

The codex audit cleared the architecture but flagged that migrating `DATA`-global
cells to user variables changes a **core addressing invariant**, not just data
location. Two invariants must hold, enforced fail-closed (not merely documented):

1. **The user-area base register is live and authoritative for every USER access**
   across all engine paths: JIT call boundaries, FFI calls, the C trampoline/
   callbacks, throw/catch unwind, and REPL/eval re-entry. SwiftForth assumes a
   stable per-task user base; Habu must guarantee it everywhere. Add diagnostic /
   negative coverage for any *stale `DATA`-relative reference* to a migrated cell
   or scratch buffer — a single missed reference aliases state silently across
   tasks.
2. **"No compilation while tasks live" is enforced fail-closed** at the compiler/
   runtime boundary (a defining word reached under an active task throws), so the
   process-global compile-only cells (`DP/CP/NDICT/TSIG/QPATCH/...`) stay safe.

**Riskiest step (watch closely): Step 1** — moving `S0/RSP/LOOPSP/LVD/VSP/HND/
INP/INE/EVALD/TKA/TKL/REPLH` + FFI scratch from `DATA` globals to user variables
while preserving boot, throw/catch, eval, FFI, and fixpoint behavior. One missed
`DATA`-relative reference creates cross-thread corruption that **passes
single-thread gates** — so a multi-task aliasing fixture (two tasks mutating the
same migrated cell, asserting isolation) is mandatory, not optional.

## Step 1 spec — per-task region + re-entry (grounded in boot)

XTs and DEFER already exist (`'`/`[']`/`execute`/`compile,`; `defer`/`is`/
`defer-unset`), so `ACTIVATE` takes an xt with no new xt plumbing, and DEFER's
cells are compile-only (already classified global; deferred words are read-only to
call from tasks).

Boot sets the engine up in `EM-DATA-INIT` (`src/habu/habu2.f:1848`): `x20` :=
data-region base, `x19`(XDS) := data-stack top, `[x20+S0-CELL]` := stack base,
`[x20+DP-CELL]` := region+DATA-START. A task is the same shape with its own region:

1. `TASK`/`CONSTRUCT` mmaps a per-task region (data cells + RSTK/LOOP/locals areas;
   `DATA-SIZE`) and a data-stack buffer; the dictionary/code region (`x26`/RBASE-VA)
   is **shared**, mapped once at boot.
2. C trampoline (thin shim, `pthread_create`) calls an engine **re-entry** that, on
   the task thread, sets `x20` := task region, `x19` := task data-stack top, inits
   `[x20+S0-CELL]`, `[x20+RSP-CELL]`/`RBASE-CELL`, `[x20+LOOPSP-CELL]`, `[x20+HND-CELL]`,
   then `BLR` the xt; on return, exits the thread.
3. No `DP/CP/NDICT` init on a task (no compilation) — they keep their shared values
   and are never touched (the fail-closed "no compile while tasks live" guard).
4. Single-thread proof first: an engine word that swaps `x20`/`x19` to a fresh
   region, runs an xt that mutates `x20`-relative state, restores — verifies the
   region-swap before pthread enters the picture. Then the pthread wrapper + a
   two-task aliasing fixture.

## Implementation order

0. **Cell audit** (above) — confirm every runtime vs compile-only classification
   with file:line. Gates everything.
1. User-variable layer (`+USER/#USER/HIS`, user-area base register) + migrate the
   runtime cells (incl. FFI scratch) off `DATA`-global into the user area.
2. pthread trampoline in the thin shim + engine re-entry; `TASK/CONSTRUCT/
   ACTIVATE/HALT/KILL/PAUSE`.
3. Atomics + `FENCE` primitives + checker effects.
4. `GET/RELEASE` over pthread mutex.
5. Fixtures, fixpoint, gate green.
