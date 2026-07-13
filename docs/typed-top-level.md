# Typed Top Level — Design

Design for dot `habu-checker-typed-top-1cc2a481` (checker-modeled typed top
level), with the parametric-cell V2-alignment verdict for dot
`habu-checker-parametric-cell-071ec3a7` in §6. This is the principled endpoint
named by the FOO2 landing (3329ca69, `DNAME-MIN-IN` depth floor): the
interpreter accumulates a checked row across top-level tokens like a body,
certified words unify against it, and xt values carry their target effect.

Status: design only. No engine or checker changes ship with this document; the
implementation is decomposed into the sub-dots drafted in §5.

## 1. Problem: the residuals the depth floor cannot reach

The FOO2 landing guards direct certified-word dispatch
(`src/habu/habu2.f` `EM-INTERPRET-FIND`: `DNAME-WIDE` and `DNAME-INT` fail
closed, `DNAME-MIN-IN` bits 8-15 of the LFIND flags reject underdepth before
the body BLR) plus the `LARITY` per-prim minimum table. Three residuals are
value/effect problems a depth floor cannot express. All three were re-probed
live on the current engine (probe sources under the session scratchpad,
`probes/p*.f`; each run `bin/hb --load <probe> < /dev/null`):

1. **xt-execute laundering.** `: FOO2 ( n -- n n ) dup ;  ' FOO2 execute` on
   an empty stack: rc 0, `.s` shows one garbage cell read from below base.
   `execute` is LARITY-guarded min 1 (the xt itself); the TARGET's arity is
   invisible through the raw xt cell. (probe p1)
2. **Compile-mode immediates — two distinct holes.** (probe p4/p5)
   - `: IMM2 ( n -- n n ) dup ; immediate  : USER ( -- ) IMM2 drop drop ;`
     rejects rc 70 — but only because the checker models `IMM2` as a
     *runtime* call and the arity happens not to fit.
   - `: USER ( n -- n n ) IMM2 ;` **certifies and runs rc 0**: the engine
     executes IMM2 at compile time on the (empty) interpret stack — a
     below-base read visible as a garbage cell after the definition — and
     `USER`'s runtime body is EMPTY while its published certificate says
     `( n -- n n )`. Certificate/runtime divergence from fully-checked
     source, no `0 set-check` needed. This is the worst finding of the
     probe round and gets its own sub-dot (§5.1).
3. **Depth-satisfied garbage values.** `0 0 catch`: depth 2 passes the LARITY
   min, `catch` BLRs into xt 0 → SIGSEGV, crash-handler rc 134. (probe p2)
   Same class: `s" abc" +` adds a byte pointer to its length, rc 0, silent
   garbage (probe p3). The interpret path knows *nothing* about literal
   types today: `EM-INTERPRET-NUMBER` pushes a bare cell, the string
   keywords push `ptr u8`+`n` cells, `'` (`C-TICK`) pushes a raw xt cell
   (gated only against WIDE/INT laundering).

A fourth, positive finding: in-body `['] A execute` is **fail-closed today**
(probe p8, rc 70 at `execute`) because `[']` is modeled `PE-N PE-OUT`
(`src/core/checker.f:4377`) and `RSEXEC` rejects a non-quot non-var. So the
body path is over-strict but sound; the laundering residual is exclusively a
top-level problem, and xt typing (§3) is a soundness closure at top level
plus an expressiveness win in bodies.

## 2. The model: a checker row over the interpret stream

### 2.1 Where it lives

The checker is self-hosted in-process and its row machinery already exists:
`DCUR`/`RCUR` rows, `UNIFY-IN`, `E-INST` (fresh instantiation of stored
effects, the `RECURSE` precedent), `RSEXEC`/`RSCATCH` (execute/catch
unification), and the persisted per-word effect store `EFF-REC`
(`ER.DIN/DOUT/RIN/ROUT/SYM/MINI`, `src/core/checker.f:3300`). The typed top
level is a **top-row tracker**: checker-package state holding one live row
that shadows the interpret stack, driven by a native per-token hook.

- **Hook plumbing (engine side).** A new sealed DATA cell (top-row hook xt)
  installed via a `set-top-check`-style word mirroring `BSETCHECK`'s
  fail-closed install window (`src/habu/habu1.f:1920`). The interpret
  dispatch points — `EM-INTERPRET-FIND`, `EM-INTERPRET-NUMBER`, the string
  keywords, `C-TICK`, `C-CHAR` — emit one event per token to the hook when
  installed: token bytes (TKA/TKL), a class code (literal-n / literal-string
  / literal-char / tick / word), and for words the LFIND flags + record
  index (from which the hook reaches the word's `EFF-REC` via its sym).
  **Hook uninstalled = today's behavior, byte for byte** (tier 0). The
  existing native gates (WIDE/INT/MIN-IN, LARITY) stay as the floor
  underneath; the row is layered above, never instead.
- **Granularity: per token, not per line.** Top-level tokens resolve during
  execution (a line may define a word and later lines call it; definers
  consume tokens), so pre-checking whole lines would need a second parser
  and diverge from execution order. The per-token hook matches the existing
  dispatch structure exactly.

### 2.2 Row transitions

| Event | Row transition |
|---|---|
| number literal | push `n` (widening-family token, as in bodies) |
| `s"`/`S\"` string | push `ptr u8`, push `n` |
| `char` | push `n` |
| `' W`, W certified | push `xt<effect(W)>` (a `T-QUOT` of the stored effect, §3) |
| `' W`, W sig-less | push fresh var (gray value) |
| certified word W | `E-INST` the stored effect; unify din against the row; on success row := douts over remainder; on failure: tier 1 warn / tier 2 reject **before the BLR** (same pre-execution point as the `DNAME-MIN-IN` reject, rc 70 named diagnostic) |
| definer keyword | keyword-effect table: most are `( -- )`; `constant` is `( n -- )` (the value it consumes), `allot` `( n -- )`, etc. Token consumption unchanged |
| sig-less / unchecked word | execute, then mark the row DIRTY (§2.3) |
| `0 set-check` | suspend enforcement (§2.4) |
| `require`/`include` | row persists — same physical stack, same stream |
| definition span (`:`…`;`, `TRUSTED:`…, SUMTYPE…) | row-neutral; the row is untouched while the compiler owns the stream |

Depth is the ground-truth anchor: at every event the tracker compares row
length against live depth (`XDS - S0`, exactly what the native guard
computes). A mismatch outside a DIRTY window is a tracker or effect-model
bug and diagnoses loudly at tier 1 — this is the self-test that keeps the
row honest during rollout.

### 2.3 Honest reset semantics (gray values, not poison)

The row must never claim more than the certificates prove, and a whitebox
suite calling shims mid-stream must not have its remaining stream silently
un-checked. Two distinct cases, deliberately different:

- **`TRUSTED:` shims and any word with a declared, certified effect do NOT
  reset anything.** They carry `EFF-REC` rows and unify like every other
  certified word. A whitebox suite's mid-stream `TRUSTED:` shim call is the
  certified path; zero poisoning, zero warnings. (This is why the reset
  trigger is "no stored effect", never "trust level".)
- **Sig-less words (defined under `0 set-check`, `DNAME-MIN-IN` 0, no
  `EFF-REC`) are gray.** They execute (the documented boundary stays open),
  and the tracker marks the row DIRTY. At the next event it reseeds: row :=
  live depth fresh vars. Consequences: depth stays enforced (vars are
  counted cells), value types of gray cells are honestly unknown (fresh
  vars unify with any certified consumer — a whitebox suite keeps working),
  and typed knowledge resumes accumulating immediately after the reseed.
  Lazy reseed (dirty flag + resync at next event) avoids a post-execution
  callback in the native dispatch.

Rejects and throws:

- A tier-2 reject fires **pre-execution**: the stack is untouched, so the
  row is untouched. The REPL/`evaluate` catchable rc-70 contract from the
  FOO2 landing is preserved (`test/underdepth-gate.f` UDG-CATCH$ shape).
- A runtime throw that unwinds to REPL recovery leaves an arbitrary stack:
  recovery reseeds the row from live depth (gray), same as a dirty resync.

### 2.4 The escape hatch is the existing audited one

`0 set-check` (the unchecked marker) suspends row **enforcement**: inside
the window tier 2 degrades to tier-1 warnings; on re-arm (`' HOOK
set-check` / `check@ set-check` restore) the tracker reseeds from live
depth. No new escape mechanism, no per-line pragma: the audited
unchecked-boundary window — already surfaced by the checked-boundary lint
and `TRUSTED.md` culture — is the only hatch. The friend-latch precedent
applies: an escape is a sealed, enumerable, lintable site, not an ambient
mode.

### 2.5 Eval-harness and REPL ergonomics (must survive, and do)

The two top-level probing idioms the dot names:

- **Eval harness** (`maki/eval-fixture.f` and friends): top-level lines of
  the shape `s" K ( span<…> … ) …" EVAL:CHECK-PASSES? TTRUE` and
  `EVAL:TOTAL @ 8 T=`. Every word is certified (`TRUSTED:
  CHECK-PASSES? ( ptr u8 n -- bool )`, typed `lib/test` comparators), every
  line is literal → certified-consumer. Under the row these check
  end-to-end; nothing resets. Same for the `CHECK!` probe idiom
  (`s" : X … ;" CHECK! . cr`, TRUST row `ptr u8 n -- n`).
- **REPL**: values persist across lines (the row persists), experimentation
  stays interactive (rejects are pre-execution and catchable, recovery
  reseeds). `5 3 + .` types; `' W execute` now checks W's inputs against
  what is actually on the stack — strictly better ergonomics than a crash
  or garbage.

## 3. xt typing: `xt<effect>` values

`'` today: `C-TICK` (`src/habu/habu2.f:2472`) pushes a raw xt after failing
closed on WIDE/INT targets. The typed model:

- **`' W` yields `xt<effect(W)>`** — in row terms a `T-QUOT` built from W's
  stored `EFF-REC` (exactly what `[: … ;]` quotation literals already
  produce in bodies). Sig-less W → fresh var (gray xt).
- **`execute` consumes `xt<e>`** by the existing `RSEXEC` semantics
  (`src/core/checker.f:1557`) applied to the top row: unify `e`'s din/rin
  against the live row, outputs become the row. A var xt binds to a
  stack-preserving pure quot (existing sound fallback). This closes
  residual 1: `' FOO2 execute` at underdepth fails unification before the
  BLR.
- **`catch` consumes `xt<e>`** by `RSCATCH`: `e` must be stack-preserving,
  and `catch` pushes `n` (the throw code). `0 0 catch` dies at tier 2
  because literal `0` is `n`, not an xt — residual 3's crash case becomes
  a named pre-execution reject. The checked-body contract ("quotation
  catch only") is unchanged; the top level simply gains the same rule.
- **`run-in-stack`** (currently `PE-N …` at `src/core/checker.f:4132`)
  retypes its xt operand to `xt<e>`; the frame-cell count it copies must
  cover `e`'s din arity. Details bounded in sub-dot §5.4.
- **`[']` in bodies** retypes from `PE-N PE-OUT` to `xt<effect>`: probe p8's
  over-strict reject becomes expressible checked code (`['] A execute`
  certifies when A's effect fits), aligning named-word xts with quotation
  xts. `is` (defer install) accepts `xt<e>` with `e` unified against the
  deferred word's declared effect — the top-level `' IMPL is ACTION` idiom
  becomes checked instead of raw.
- **`set-check` / `check@`** retype to consume/produce the checker-hook xt
  type (`xt<ptr u8 n -- n>`), with literal `0` admitted as the off marker
  (it is itself the §2.4 suspension event, so the tracker sees it
  regardless).
- **No new dict metadata.** xt typing reads the same `EFF-REC` store the
  checker already persists (including through snapshots — the v2
  `DNAME-MIN-IN` record band precedent); the dict record keeps carrying
  only the flag bands it carries today.

Compile-mode immediates (residual 2) get the same treatment on the compile
path: an immediate executed by `EM-COMPILE-CALL` runs on the *interpret*
stack, so its effect must unify against the *top row*, not be inserted into
the body being checked. That is a checker model fix (immediates are
compile-time effects) plus a native guard on the compile-path BLR; both are
sub-dots (§5.1, §5.5) because the certificate-divergence half is a live
soundness hole independent of the row tracker.

## 4. Staged adoption

### Tier 0 — today (shipped)

Depth floor only: `DNAME-MIN-IN` + `DNAME-INT`/`DNAME-WIDE` + `LARITY`
census prims. Hook cell absent. Gate: `test/underdepth-gate.f`,
`test/internal-word-gate.f`, engine suite — all green now.

### Tier 1 — row tracked, warnings only

Hook installed by default in the native prelude; every mismatch emits one
`hb: top-row: <token> expected: <t> actual: <t>` line on stderr; rc and
execution are unchanged (the word still runs, even on mismatch — tier 1
observes, never blocks). Gray/dirty resyncs are silent; depth-anchor
mismatches (tracker bugs) warn loudly.

Gates: (a) full master gate set green with the hook on and **zero behavior
change** proven by the existing suites; (b) a new top-row fixture asserting
one warning each for the three residual probes (p1, p2, p3 shapes) and
**zero warnings** for: the eval-fixture idiom, the `CHECK!` probe idiom, a
whitebox mid-stream `TRUSTED:` shim call, and a `0 set-check` window; (c)
warning census over the tree's own loads (`test/run.f` tree, tools mains,
maki suite) — the burn-down list that measures tier-2 readiness.

### Tier 2 — reject on mismatch

Row mismatch at a certified word, `execute`/`catch`/`run-in-stack` on a
non-xt or ill-fitting xt, and value-class mismatches reject pre-execution
with rc 70 named diagnostics (same channel as the min-in reject, still
catchable through `evaluate`). Escape hatch: the `0 set-check` window
(§2.4) — audited, lint-visible, no new mechanism.

Gates: full master set on the exact tree; the tier-1 fixture flipped to
negative assertions (reject rc 70, not warning); eval harness suite;
REPL PTY recovery suite (`test/proc-pty.f` extended with a typed-reject
recovery case); zero unexplained tier-1 warnings remaining in the tree.

### Tier-2 compatibility cost table

Top-level code classes enumerated by scanning the tree (test suites, tools
mains, prelude/whitebox scripts, eval fixtures):

| Top-level idiom class | Sites (representative) | Tier-2 fate |
|---|---|---|
| `require`/`include` lines | every file | unaffected (keyword) |
| definitions + definers (`:` `TRUSTED:` SUMTYPE/ENUM/PRODUCT `variable` `create` `constant` `defer` DEFTYPE KERNEL:) | everywhere | unaffected; `constant`/`allot` get keyword effects `( n -- )` |
| single trailing MAIN call | `test/engine-suite.f` REPORT, `tools/host-lint.f` HOST-LINT, `maki/test.f` TEST:RUN, `tools/stale-status-lint.f` SS-MAIN | unaffected `( -- )` |
| literal → certified-call lines | `maki/eval-fixture.f` (`s" K …" EVAL:CHECK-PASSES? TTRUE`), `5 FOO2 + . cr` | unaffected; now fully checked |
| `CHECK!` probe idiom | `s" : X … ;" CHECK! . cr` (underdepth-gate positive) | unaffected (TRUST `ptr u8 n -- n`) |
| `' HOOK set-check` installs | `tools/lint/text.f:17`, `tools/check-core.f:24`, `test/engine-suite.f:1436,1525` | unaffected after `set-check`/`check@` retype (§3); `0 set-check` special-cased |
| `0 set-check` whitebox windows | `test/engine-suite.f` (5 sites), `test/prop-test-core.f`, `src/core/internal-mark.f:48`, `src/habu/aot-lib.f:18` | unaffected by design — the escape hatch |
| sig-less word calls at top level | engine-suite whitebox sections | run + gray reseed; depth still enforced; **not** rejected |
| mid-stream `TRUSTED:` shim calls | whitebox suites | unaffected — certified path, no reset (§2.3) |
| `' W execute` / `' W catch` | `test/underdepth-gate.f` UDG-CATCH$ | typed; existing green uses fit; **underdepth/garbage uses reject (intended)** |
| debug prints of non-`n` cells (`.` on a ptr) | ad-hoc probes; not in committed suites | **breaks**; cost LOW — use typed print helpers |
| garbage-value idioms (`0 0 catch`, `s" abc" +`) | none committed (probe-only) | **breaks (intended)** — these are the residuals |
| user `immediate` words | `test/engine-suite.f` IM5/P5/TPNI fixtures; `src/core/include.f` require/include | re-modeled as compile-time effects; engine-suite fixtures updated; cost MEDIUM, contained (§5.1/§5.5) |
| multi-line stack carrying at top level | rare; REPL sessions | allowed — the row persists and types it |

Net: every committed green idiom survives; what breaks is exactly the
exploit class the dot exists to close, plus ad-hoc pointer-printing probes,
plus a contained re-model of immediates whose current model is unsound
anyway (probe p5).

## 5. Implementation sub-dots draft

For the orchestrator to mint. Ordered; 1 and 8 are independent of the rest.

1. **checker: immediates are compile-time effects, not body steps.**
   Files: `src/core/checker.f` (token step path: consult `DNAME-IMM`;
   immediate calls inside a body apply to no row / are excluded from the
   body scheme), new `test/immediate-model-test.f`.
   Acceptance: `: IMM2 ( n -- n n ) dup ; immediate  : USER ( n -- n n )
   IMM2 ;` REJECTS (negative regression for the p5 certificate
   divergence); `postpone` and engine immediates (`require`, `include`,
   engine-suite IM5/P5/TPNI shapes, re-authored as needed) stay green;
   engine suite green.
2. **engine: top-row hook cell + sealed install + dispatch events.**
   Files: `src/habu/habu1.f` (DATA cell, `set-top-check`-style sealed
   install mirroring `BSETCHECK`), `src/habu/habu2.f`
   (`EM-INTERPRET-FIND`/`-NUMBER`/string keywords/`C-TICK`/`C-CHAR` event
   emission), fixture `test/top-row-hook-test.f` (logging hook).
   Acceptance: hook uninstalled = current gate byte-green; logging hook
   observes correct (class, token, flags) for each literal/word/tick class;
   invalid install fails closed like `BSETCHECK`.
3. **checker: top-row tracker (tier 1).**
   Files: new `src/core/top-row.f` (row state, literal typing, `E-INST`
   unify, gray/dirty reseed, `0 set-check` suspension, depth anchor,
   warning renderer), prelude install, `test/top-row-warn-test.f`.
   Acceptance: p1/p2/p3 probe shapes each produce exactly one named
   warning with rc 0; eval-fixture idiom, `CHECK!` probe, mid-stream
   `TRUSTED:` shim, and `0 set-check` window produce zero warnings; row
   persists across `require`; depth-anchor self-check clean over
   `test/run.f`.
4. **checker: xt<effect> values.**
   Files: `src/core/checker.f` (`[']` retype from `PE-N`; `execute`/
   `catch`/`run-in-stack`/`is`/`set-check`/`check@` operand retypes;
   top-row `RSEXEC`/`RSCATCH` reuse), `test/xt-effect-test.f`.
   Acceptance: `['] A execute` certifies when the effect fits (p8
   expressiveness) and rejects when it does not (negative); tier-1 warning
   for `' FOO2 execute` at underdepth; `0 0 catch` warns (tier 1) and
   rejects (tier 2 fixture, pre-armed); `' UDG-TRY catch . cr` and
   underdepth-gate positives stay green.
5. **engine: compile-mode immediate guard.**
   Files: `src/habu/habu2.f` (compile-path BLR gains the same min-in/hook
   gate against the interpret row), extend `test/underdepth-gate.f`.
   Acceptance: p4 shape (immediate at compile-time underdepth) fails
   closed with a named rc-70 diagnostic before the below-base read; both
   cold-prefix source paths covered (--load and stdin), matching the FOO2
   regression pattern.
6. **snapshot/AOT: effect rows reach the top row in every boot path.**
   Files: `src/habu/habu2.f`/`src/habu/aot-lib.f` snapshot/AOT slices,
   `test/top-row-snapshot-test.f`.
   Acceptance: a snapshot/AOT image reproduces identical tier-1 warnings
   and tier-2 rejects for the p1/p2/p3 shapes as a cold source load
   (the `EFF-REC`/`ER.SYM` persistence the min-in band already proved,
   now for full rows).
7. **tier-2 switch + tree burn-down.**
   Files: `src/core/top-row.f` (mode cell + staging env/CLI flag),
   checked-boundary lint extension for escape-window audit rows, tree
   fixes for any tier-1 warnings found by 3's census.
   Acceptance: full master gate set green with tier 2 on; the tier-1
   fixture flipped to rc-70 negatives; REPL PTY recovery case green;
   zero unexplained warnings tree-wide.
8. **(independent) parametric-cell governance disposition** — carry out §6:
   record the defer on `habu-checker-parametric-cell-071ec3a7` with the
   rationale below, and add the R7-lane constraint (no `ptr` over
   TK-EVIDENCE) to the V2 evidence-family design when that lane opens.

## 6. Parametric cell families vs V2 storage discipline (dot habu-checker-parametric-cell-071ec3a7)

**Verdict: defer — superseded by V2 R6/R4 for parametric cell families;
the TK-EVIDENCE half is vacuous today and belongs to R7 as a stronger
rule.** No per-kind NOM-SCALAR? widening should be implemented now.

Probe evidence (current engine, rc captured):

- Arity-0 nominal scalar IS governed: `variable NS-X  : NS-P ( -- ptr
  attn-stage-q ) NS-X ;` rejects rc 70 (`expected: ptr attn-stage-q<>
  actual: ptr a`) — `NOMPTR-BLOCK?` (`src/core/checker.f:1161`) working as
  landed.
- Parametric family is NOT: the identical shape with
  `ptr span<space-global,f32,extent-n>` **certifies rc 0** — a raw
  variable acquires span identity by ordinary unification, exactly the gap
  the dot records.
- Exposure is latent, not live: zero committed `ptr span<`/`ptr matrix<`/
  `ptr field<` sites exist in the tree; `field` is unreachable from user
  signatures (reserved `@` package, `src/core/type-family.f:920`); and
  TK-EVIDENCE has **zero declared families and no declaration surface**
  (SUMTYPE/ENUM/PRODUCT declare kinds 2/3/1 only; `TFAM-REG-CELL` is
  engine-internal) — the "TK-EVIDENCE pointee" scope is currently empty.

Rationale for defer:

1. **R6 IS the storage discipline this extension would approximate.**
   `arena-owner<region>` / `arena-ref<region,t>` / `arena-mut<region,t>`
   make reference *introduction* checker-generated and scope-governed
   (transaction/borrow), with escape and aliasing acceptance criteria
   (MODEL-CAD-V2-PLAN §R6). A per-kind `NOM-SCALAR?` widening would be a
   second, weaker introduction authority over raw `ptr` that R6 then has
   to dismantle — the same class of interim guard the Correct Fixes rule
   tells us not to normalize.
2. **R4 replaces the family set itself.** `tensor<shape,dtype,layout,
   address-space,region>` subsumes span/matrix as the storage-bearing
   parametric types; investing per-family governance tables in the
   transitional families optimizes a surface R4 retires.
3. **The arity-0 restriction is load-bearing.** Parametric ARGS in
   non-strict positions must keep unifying (the reason `NOM-SCALAR?`
   requires arity 0). Widening per kind means per-caller reasoning at all
   three admission points (`NOMPTR-BLOCK?` in `U-TYPE`,
   `CHECKER-LAYOUT-INFO` for LAYOUT-BUFFER, `LAYOUT-MEM-INNER` for typed
   `@`/`!`) for every widened family — real work with no live consumer
   (no committed `ptr span<…>` exists to protect).
4. **TK-EVIDENCE needs a stronger rule than governed introduction, and
   needs it in R7, not here.** Evidence is compile-only (its unforgeability
   acceptance: "raw integer metadata cannot forge evidence"); when R7
   lands evidence families, `ptr` formation over a TK-EVIDENCE pointee
   should fail closed entirely — there is no runtime representation a
   pointer could legitimately govern. Recording that as an R7 design
   constraint pre-empts nothing and avoids building governance for a kind
   with zero inhabitants.

**IF a pre-V2 need materializes** (a committed consumer needs governed
`ptr span<…>` storage before R6 lands): the concrete complement shape is
the TK-CELL landing's, widened per kind — a per-family "governed" bit in
the TFAM registry consulted by the three callers above, introduction armed
only inside the generated-accessor window (`LAYOUT-INTRO`), with a
negative fixture per widened family. That shape is recorded here so the
defer is reversible without re-deriving it; the default remains defer.

## 7. Probe inventory (evidence for this design)

All run as `bin/hb --load <probe> < /dev/null` on the current native engine.

| # | Program (essence) | Result |
|---|---|---|
| p1 | `: FOO2 ( n -- n n ) dup ;  ' FOO2 execute .s` | rc 0, garbage cell — xt launders arity |
| p2 | `0 0 catch` | SIGSEGV rc 134 — depth-satisfied garbage xt |
| p3 | `s" abc" + .` | rc 0, prints ptr+len sum — literals untyped |
| p4 | immediate `( n -- n n )` called in `( -- )` body | rc 70 — rejected only by accidental arity misfit |
| p5 | immediate `( n -- n n )` called in `( n -- n n )` body | rc 0 — compile-time below-base read AND empty runtime body under a `( n -- n n )` certificate |
| p6a | `: NS-P ( -- ptr attn-stage-q ) NS-X ;` | rc 70 — arity-0 nominal scalar governed |
| p6b | `: SP-P ( -- ptr span<space-global,f32,extent-n> ) SP-X ;` | rc 0 — parametric family ungoverned |
| p8 | `: B ( n -- n n ) ['] A execute ;` | rc 70 — in-body xt already fail-closed (over-strict) |
