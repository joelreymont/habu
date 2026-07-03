---
title: Hash-indexed dictionary lookup
status: done
priority: 2
issue-type: task
created-at: "2026-07-01T22:31:41.043151+02:00"
---

FIND is a linear O(NDICT) scan over flat 48-byte records (src/habu/habu1.f:1859-1959, loop at 1927-1949), re-scanned up to 3x on miss: private->public->global (habu1.f:1950-1957). Case-folding recomputed per byte per candidate (habu1.f:1940-1944). Compiling W words is O(W*NDICT) ~ O(W^2); dominates compile/load time. Fix: hash index (name-hash -> record idx, open addressing) maintained at define time, case-normalized keys stored once; wordlist id in the key or per-wid tables so miss path is 3 hash probes, not 3 full scans. Also covers search-wl BSWL (habu1.f:1555-1585) and qualified-name scan FIND-QSCAN (habu1.f:1885-1924). Needs fixpoint rebuild.

## Design (2026-07-02, architect pass)
Data structure: open-addressed hash table of u32 slots (record index+1;
0=empty), capacity 2*DICT-CAP rounded to power of two (16384 slots =
64KB), living in a dedicated DATA block (new layout cell HIDX-OFF; DATA
has headroom - verify against DATA-SIZE). Hash = FNV-1a over the
case-FOLDED name bytes XOR wordlist id (fold once - the per-byte folding
in FIND-LOOP is a large constant cost today).
Insert: at publish time (EM-COMPILE-PUBLISH NDICT++ path AND the
create/constant/defer inline publishes, habu2.f:1475-1524,1636-1663 -
enumerate ALL NDICT-increment sites; each gets a BL LHIDXADD). Probe:
linear, wrap at mask; slot stores record INDEX so the 48-byte record
stays authoritative.
Lookup: LFIND first probes the table (fold+hash once per token); on hit,
verify name equality once (collision safety) then apply the SAME
newest-wins semantics as today: since insert happens in definition order
and probing finds the FIRST match, newest-wins requires either (a)
insert-overwrite: later same-name insert REPLACES the slot value (then
FIND-MATCH-continue behavior is preserved exactly), or (b) walk-all -
choose (a), it matches the later-wins arena design in the checker.
Qualified names (pkg:word) and the 3-wordlist retry (private->public->
global): hash includes wid, so retry = 3 probes. FIND-QSCAN pre-scan
stays (cheap).
Deletion/retirement: FORGET-DEFS-FROM (hide.f) and snapshot tail retire
records -> table entries for retired indices must die: retirement is
always a TRUNCATION of NDICT (verify!), so lookup double-checks
slot-value < NDICT before use (stale entries self-invalidate; no table
rebuild needed) - document this invariant where NDICT is truncated.
Snapshot: the table is process state; persist NOTHING - restored images
rebuild lazily: keep a HIDX-READY cell zeroed in the snap-lib scratch
(add to SND-ZERO-LIVE) and have LFIND rebuild the whole table on first
miss-with-ready=0 (walk NDICT once, ~2000 inserts, <1ms).
Fallback: keep the linear FIND-LOOP intact as the verify path and as the
rebuild source; a build flag is NOT needed - the hash is an index over
the same authoritative records.
Tests: engine-suite - redefinition shadows (later-wins), qualified
resolution order unchanged, FORGET-then-redefine resolves new, snapshot
restore then lookup works, 4000-def load correctness; perf: measure the
compile of the 4000-def fixture before/after (expect the remaining
superlinear engine component to flatten; checker side already linear).
Register contract for LHIDXADD/LHIDXFIND: follow LFIND conventions
(x9=name ptr,x10=len in; x13=result out); scratch x4,x5,x11,x12,x14;
NO x18 (encoder rejects); LR saved via SP idiom if any BL inside (none
planned - leaf).
Order of work: (1) layout cell + table zeroing at startup; (2) LHIDXADD
+ insert sites; (3) LHIDXFIND probe in LFIND before the linear loop
(linear loop stays as miss fallback initially = correctness-safe rollout);
(4) flip: linear loop only on table-miss-verify; (5) retire-truncation
invariant + snapshot lazy rebuild; (6) tests + measurements; fixpoint
after each step.

## Implementation notes (2026-07-03, site enumeration)
NDICT-increment sites in src/habu/habu2.f (each gets one bare
`LHIDXADD LABEL@ BL,` AFTER the increment; LHIDXADD takes NO register
args - it reads record NDICT-1 directly for name/flags/wid):
- :1456 (definer path), :1492 (create), :1521 (variable/constant family),
  :1657 (defer), :2590 (TRUSTED: publish), :2771 (hooked publish,
  pre-reject-branch variant), :2784 (nohook publish arm).
- :2283 = baked-count init (bulk): NOT instrumented - covered by lazy
  build. :2428 = snapshot restore count: NOT instrumented - lazy build
  (HIDXP-CELL is zeroed in the snap data scratch, so restored images
  rebuild on first lookup).
Refinement to the design: LHIDXADD begins with `HIDXP-CELL == 0 -> RET`
(lazy regime: adds are no-ops until the first LFIND builds the table;
after that every publish maintains it). LHIDXFIND: ptr==0 -> build all
[0,NDICT) then probe. New layout cell: $3C98 HIDXP-CELL (spare confirmed
after TASKS-LIVE-CELL $3C90). Table: 64KB mmap at first build,
16384 u32 slots (mask $3FFF), slot = record index+1, insert-overwrite
for newest-wins, lookup validates slot-1 < NDICT (truncation
self-invalidation) AND re-checks name equality vs the record.

## CORRECTED DESIGN (2026-07-03, after a full implementation attempt, reverted)
The attempt built cleanly and self-hosted the -- stage build, but the
final engine CRASHED (exit 138 SIGBUS) on `: R ; : R ;`. Root findings
that INVALIDATE the original design assumptions:

1. THE ENGINE REJECTS DUPLICATES; it is NOT newest-wins. `bin/hb` cleanly
   exits 78 "duplicate definition: R" on a redefinition. So each
   (wordlist-id, folded-name) key appears AT MOST ONCE in the dictionary.
   => the table is INSERT-ONCE. Delete all the "continue probing past a
   match to find the newest / insert-overwrite" logic - it was wrong and
   is the likely crash source (it walked past the intended slot).
   Lookup stops at the FIRST matching slot; probing stops at the first
   EMPTY slot for both insert and miss.

2. THERE ARE TWO O(NDICT) LINEAR SCANS PER DEFINITION, both to accelerate:
   (a) FIND (EMIT-FIND, habu1.f) - the lookup path; (b) C-REJECT-DUP-DEF
   (habu2.f:1369) - a SEPARATE full-dict scan run at every definition
   START (via C-QUALIFY-DEF) comparing DEF-WL-CELL + TKL-CELL name. The
   dup-check is arguably the hotter of the two for load-heavy files.
   BOTH must probe the table. Order across a definition: qualify+dup-check
   (start) uses TKA/TKL/DEF-WL cells; publish (at ;) does NDICT++ then
   insert. So dup-check for the 2nd R finds the 1st R's slot and rejects
   before any second insert - no double-insert, table stays insert-once.

3. LIKELY CRASH BUGS in the attempt (fix in the redo): (a) LHIDXBUILD did
   NOT check the mmap return for failure before storing HIDXP and LDRW-ing
   from it - a negative return => SIGBUS; ADD the `dup 0 < -> die` check
   like every other mmap site. (b) the continue-past-match probe read
   slots it should not have.

4. REGISTER SAFETY confirmed workable: LHIDXADD ran mid-publish and must
   save its full clobber set (x2-x7,x14-x17,LR on an SP frame) - that part
   was correct and should be kept. C-HIDX-HASH must be DEFINED BEFORE
   C-HIDX-INS/EMIT-HIDX (forward-ref = build-time E-UNDEFINED). mmap flags
   constant: define `$1002 constant HIDX-MAP-ANON` locally (mirrors
   USIGS/TOKBUF; target-portability is the same open question they have).

5. WIRING that worked: 7 NDICT++ publish sites in habu2.f each get
   `LHIDXADD LABEL@ BL,`; bulk build LHIDXBUILD BL at the END of
   EM-SEED-DICT (baked path) and after NDICT set in EM-SNAPSHOT-RESTORE
   (:2428). Labels alloc in EMIT-LABEL-CORE; EMIT-HIDX in the emission
   sections after EMIT-FIND. HIDXP-CELL=$3C98, HIDX-SLOTS=$4000,
   HIDX-BYTES=$10000 in layout.f. HIDXP zeroed in snap scratch already?
   NO - must ADD HIDXP-CELL to snap-lib SND-ZERO-LIVE so restored images
   rebuild (or it holds a stale table pointer -> crash).

REDO PLAN: (1) revert-clean baseline (done). (2) Add table + LHIDXBUILD
WITH mmap error check + HIDXP snap-zeroing; build empty, verify fixpoint
(dormant, HIDXP set but table only accelerates). (3) Add INSERT-ONCE
LHIDXADD at publish sites; verify a heavy load + the dup-reject case
byte-fixpoints and does NOT crash. (4) Point FIND probe at the table
(first-match-stops, no continue). (5) Point C-REJECT-DUP-DEF at the table
too. (6) Measure 4000-def load. Each step: fixpoint + gate before next.
Test the dup-reject path explicitly at every step (it was the crash).

## Redo session findings (2026-07-03, attempt 2 — reverted, MAJOR progress)
The step-2 infrastructure (table + LHIDXBUILD at startup, no consumers)
reached: lint clean, byte-for-byte fixpoint HELD, full gate PASS, dup
rejection intact. Two blockers remain; everything learned is below.

PROVEN FACTS (lldb evidence):
1. EM-SEED-DICT runs BEFORE EM-MMAP-DATA-REGION - any DATA store there
   faults (EXC_BAD_ACCESS writing into text; x20 not yet data base).
   Startup order: ENTRY-ARGS, RUNTIME-STACK, MMAP-CODE, SEED-DICT,
   MMAP-DATA, DATA-INIT, SNAPSHOT-RESTORE, STARTUP-RUNTIME-STATE.
2. EMIT-SOURCE's emitted paths BRANCH into evaluation and do NOT fall
   through - code emitted after it in EM-STARTUP-RUNTIME-STATE is
   unreachable on at least the pipe path. Breakpoints on the LMAINP store
   (pre-EMIT-SOURCE) also never hit on a pipe run of bin/hb, so the whole
   RUNTIME-STATE tail executes on some paths only - MAP THE REAL STARTUP
   CONTROL FLOW FIRST next time (which paths run RUNTIME-STATE at all?).
3. hb-stage does NOT serve piped stdin like bin/hb (clean tree behaves
   identically: no output, no dup-reject on pipes) - NEVER smoke-test a
   stage artifact with pipes; validate via install --force + gate, or run
   the stage the way build-fixpoint does.
4. LHIDXBUILD placed after LVRINIT BL: fixpoint + gate green, dup-reject
   works. But the two-build snap compare FAILED (expected: the 20
   SND-QUARANTINE offsets are tree-dependent and the tree changed -
   re-derive them whenever emitter code changes), AND the restored
   snapshot engine crashes rc=134 on --load (bin/hb same cmd fine) -
   the BL breaks the RESTORE path specifically. Next lldb step: break in
   hb-new's restore flow, check register state when the BL runs there.
5. Routine must be fully register-transparent (SP frame saving x0-x8,
   x13-x17, LR) - startup code between source setup and eval has live
   registers.
6. Duplicate-block hazard: a jj restore during active edits can half-
   apply; verify `rg -c` counts of new symbols after any restore before
   rebuilding (duplicate definition: HIDXP-CELL / LHIDXADD cost 2 builds).
7. Exit 14 from install --force = E-BUILD-STATUS (BF-RC0) with silent
   logs; reproduce with `-- stage` + run the failing step manually.

WORKING CODE (reapply verbatim from this dot's history via jj op log or
rewrite): layout HIDXP-CELL $3C98 / HIDX-SLOTS $4000 / HIDX-BYTES $10000;
habu1 C-HIDX-HASH {: nr:n lr:n hr:n c3:n c4:n c7:n :} FNV-1a via LIT64
constants; C-HIDX-INS insert-once; EMIT-HIDX with full-save LHIDXADD +
register-transparent LHIDXBUILD + carry-flag mmap check (4 C-CS CSET,
4 bfail CBNZ) + MAP-ANON-PRIVATE (target-conditional, NOT $1002);
habu2 label allocs + EMIT-HIDX after EMIT-FIND + BL after LVRINIT;
snap-lib HIDXP-CELL SND-ZERO-CELL. Typed label locals {: x:label :}.

NEXT STEPS: (a) map startup control flow: which startup paths execute
the RUNTIME-STATE tail, where does the restore path re-enter, where is
the single point that ALL paths pass after data-init with NDICT final
(candidate: inside the interpret-loop entry LMAIN, guarded by HIDXP==0
so it runs once); (b) fix the restore-path crash; (c) re-derive
SND-QUARANTINE offsets after the emitter change; (d) then steps 3-5
(publish BLs, FIND probe, dup-scan probe) per the corrected design above.

## Scope correction (2026-07-03): restore path is slated for DELETION
(user decision: no restored images, ever - see habu-retire-snapshot-
restore). The attempt-2 blocker "restored snapshot engine crashes rc-134"
is therefore NOT a blocker for this dot: implement the hash on the normal
boot path only (LHIDXBUILD after LVRINIT), keep the '-- snap' validation
green while it exists, and do not spend effort fixing restore-path
interactions.

## DONE (2026-07-03, attempt 3 - landed, all stages green)
Implemented per the corrected design + redo plan, one commit per stage,
each with byte-for-byte fixpoint + full gate + trust-lint 0 +
typed-local-diff-lint 0 + `-- snap` green.

Commits (on the fable line):
- Add dormant hash-index dictionary table  (layout HIDXP-CELL $3C98 /
  HIDX-SLOTS $4000 / HIDX-BYTES $10000; habu1 C-HIDX-HASH FNV-1a fold,
  C-HIDX-INS insert-once, EMIT-HIDX = register-transparent LHIDXBUILD w/
  carry-flag mmap check + full-save LHIDXADD; LHIDXBUILD BL placed on the
  SAME line as LVRINIT BL in EM-STARTUP-RUNTIME-STATE (reached
  unconditionally after the cwok merge, data mapped, NDICT final);
  HIDXP-CELL added to snap-lib SND-ZERO-LIVE).
- Insert-once hash-index maintenance at publish sites  (7 bare
  `LHIDXADD LABEL@ BL,` at every `NDICT NDICT 1 ADDI,` - habu2 lines
  ~1541/1577/1606/1742/2675/2856/2869 - all same-line, zero trust drift;
  each site's runtime routine either saves LR or branches to LMAIN, so the
  BL is register-safe).
- Probe hash index in FIND before linear scan  (habu1 EMIT-FIND: at
  FIND-START, fold+hash once, walk the (name XOR wid) chain; validated
  slot (idx<NDICT, wid==x2, folded name equal) returns; empty slot falls
  through to the linear loop = miss fallback. Preserves x2/x9/x10/x13.)
- Probe hash index in duplicate-definition check  (habu1 C-HIDX-DUP?
  probes (DEF-WL, TKA/TKL); wired into habu2 C-REJECT-DUP-DEF with
  same-line edits - HIDXP==0 -> linear fallback, else authoritative
  probe: match -> C-DUP-DEF-FAIL, empty slot -> proceed. Grew the engine
  source past the AOT maker/stage2 512KB buffers: MK-SOURCE-CAP +
  S2-SOURCE-CAP $80000->$A0000 (build-only, no bin/hb change); size
  ratchet baseline 132343->148855 (one 16KB page); added a
  dup-behind-a-retired-slot regression to gate-dictionary-lib.)
- Refresh STATUS verified date (date rolled over mid-session).

CORRECTNESS proven (bin/hb): undefine+redefine resolves NEW (probe skips
retired wid=-2 slots), multi-cycle undefine, qualified names, case-insens
fold, cross-wordlist same name (wid-in-hash separates them), dup-behind-
retired-slot rejects 78, all 6 publish types reject dups 78, heavy mixed
loads byte-identical to baseline. The self-host fixpoint (millions of
FINDs + a dup-check per definition through the table) is the strongest
regression: any wrong probe result would break the byte fixpoint.

KEY INVARIANT: DICT-CAP=8192 < HIDX-SLOTS=16384, so the table is <=50%
full - the open-addressed probe can never fill and loop. Insert-once is
correct because the engine REJECTS duplicates (exit 78), so each
(wid, folded-name) key is in the table at most once; retired records keep
their slot but carry wid -2 and are skipped by the wid check.

MEASUREMENTS (macOS arm64; V-i chain fixture, each def references the
prior word so both FIND and the dup-check run per definition; best of 3):
  Engine dictionary only (`0 set-check`, isolates parse+FIND+dup+emit):
    W=1500  baseline 0.08s  hash 0.06s  1.33x
    W=3000  baseline 0.12s  hash 0.07s  1.71x
    W=6000  baseline 0.22s  hash 0.09s  2.44x
  -> baseline is superlinear (O(W^2)); hash is ~flat (O(W)); the speedup
     grows with W exactly as predicted ("the superlinear engine component
     flattens"). Full load with the checker ON (checker is the linear
     bulk) is a steady ~1.16-1.20x overall (e.g. W=6000: 1.06s -> 0.91s).
