---
title: Cut colon compilation onto the checked chain
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.611694+02:00"
---

CG-01, phase 4 of the hard cut: make checked HIR plus the native pipeline the sole compiler for normal colon definitions. habu2.f:7020 COMPILE-EMIT:EM-COMPILE is the single production entry (verified, no drift); route it and bootstrap through the chain, prove self-hosting to a byte-identical fixpoint, run every gate on the sole path, then delete the old emitter and bridges (habu-delete-the-old-679cfd35). An opt-in hook is not completion. Reconcile with habu-cut-over-staged-070d68c8 / habu-self-host-staged-520ab588.

Claim: agent=thecut workspace=.jj-ws/habu-cut-colon-compilation-a5aa3f1f

PREREQUISITES (scout-verified 2026-08-05/06):
1. Data-word addresses (habu-resolve-a-data-a1c8067f) — the one hard capability blocker; lane in flight.
2. Spill rewrite loop into production: migrate-era EMITTED never rewrites (fits-or-refuses; survivable only while the old emitter is the fallback). The pipeline runs allocate -> if spills planned, A64SPILL:REWRITE -> re-validate -> emit. This is CG-28's requirement concretely.
3. Pressure retry: attempt normally; on E-A64RA-SPILL re-elaborate with the CROSS-L split flag (proven one-liner; evidence in habu-split-call-crossed-6eda1613) and re-run. The publisher's VALIDATE/COMMIT split already gives refusal-moves-nothing; the pipeline must keep compile-attempt separate from publish so retries are free.
4. Two whole-tree probes before finalizing order: any definition certified under CNUM-OVERFLOW:TRAP reaching hir arithmetic (E-A64SEL-TRAP refuses it today), and any use of to/^ on typed locals (dialect refuses; corpus-based verdicts do not cover the tree).
5. Seed: derive by transitive closure from the chain's entry (ir/* before native/*; NREACH is not in the closure — decide the seed by closure, not directory).

The dispatch path that reads routine records concurrently must acquire START (LDAR) — the publisher writes with release; the acquire half is unexercised until this lands (noted in habu-re-express-the-13d7558c).

PROBED 2026-08-06 (agent=spillwire, dot habu-wire-the-spill-ca604d57). Prerequisite 4's two
whole-tree probes, measured. Both verdicts: the cut is NOT blocked by either capability.

(a) TRAPPING ARITHMETIC — the trapping set is EMPTY in production, and unreachable from the
migration entry by construction. The overflow policy is a field of the compilation unit's
binding and of nothing else: there is no per-definition and no per-file pragma, and
src/compiler/native/hir.f:200-207 states that nothing re-derives it or carries a default.
The tree has exactly ONE production binding constructor, src/compiler/native/abi.f:57-63
NABI:BINDING, and it passes CNUM-OVERFLOW:WRAP (abi.f:61). The only other CNUM-OVERFLOW:TRAP
mentions under src/ are two wire-code decoders (src/compiler/ir/context.f:234,
src/compiler/ir/attr.f:344) and one equality test (src/compiler/native/hir.f:395 TRAPS?).
Every TRAP-constructing call site in the tree is a test fixture (22 files).
COUNT OF PRODUCTION DEFINITIONS CERTIFIED UNDER CNUM-OVERFLOW:TRAP THAT REACH hir.add/sub/mul:
ZERO. No definitions to name, because there are none.
Migrating one through the chain to watch E-A64SEL-TRAP fire is NOT POSSIBLE and that is the
finding, not a gap in the probe: src/compiler/native/migrate.f IN-CONTEXT opens every
migration with NABI:BINDING, so no migration can be TRAP-certified at all. The refusal is
proved where it can be reached — at the selector, test/compiler/native-select.f:1724-1726
TRAP-REFUSE-CASES, which builds a TRAP binding (TBND, :59-70), runs a hir.add through
selection and pins E-A64SEL-TRAP.
Mechanism, for the record: src/compiler/native/select.f:1935-1940 TRAP-CK throws when the
op's schema says may-trap and TRAP-PRESERVED? (select.f:1888) says the lowering loses it.
Only div, call and wordcall answer true; add, sub and mul answer false. The check is PER OP
(select.f:2676 in RULE, and select.f:2030 in FUSE-INDEX), not once per unit, so a TRAP unit
with no integer arithmetic would select fine today.
VERDICT: the cut does NOT need habu-lower-trapping-arithmetic-5f514ffe. The trapping set is
empty and cannot be non-empty while the migration entry hardcodes the wrapping binding.
Leave that dot open as the capability it is; it orders after the cut, not before it.

(b) to/^ ON TYPED LOCALS — ZERO occurrences in any chain-compiled source. Whole-tree scan of
all 1407 .f files (comment- and string-stripped, then every surviving candidate read):
  `^` as a standalone token: 0 hits in .f, tree-wide. Every one of the 551 raw `^` characters
  is exponent prose, transpose notation, `^=`, or the char literal, all inside comments or
  strings.
  `to` as a rebind: 0 hits in .f, tree-wide. 204 candidates survived comment-stripping; all
  204 are either inside s" " literals or are a READ of a local literally NAMED `to`.
Classified: production (src/, lib/, maki/, tools/) 0 and 0; test/ 0 and 0; docs/example 0
and 0.
The only real rebinds in the tree are 17 in bootstrap/*.fs (Gforth-hosted host code, outside
the chain: bootstrap/cg/{asm,exec,icode,install,link,walk}.fs, bootstrap/src/sigparse.fs),
all on BARE locals, several using Gforth's `{: … | … :}` uninitialized form. None is a typed
local, and none is compiled by the chain.
Why the count is zero rather than merely low: neither word exists in the dialect's
vocabulary, so both are already refused as E-HIR-UNMODELED — stated at
src/compiler/native/elaborate.f:228-232, which also names the two dots.
VERDICT: the cut is NOT blocked by habu-rebind-a-typed-b2a3e369 or habu-take-the-addr-18a38b4f.
There is not one call site in src/, lib/, maki/, tools/, test/ or docs/ to fix.
Two residual risks, neither blocking: (1) 13 locals in .f files are literally NAMED `to`
(12 production, 1 test) — if `to` ever becomes a dialect word, src/core/checker.f:46,53 and
src/core/declaration-transaction.f:215 and tools/lint/diff-frame-write.f:100 become
ambiguous; (2) if bootstrap/cg/*.fs is ever ported onto the chain, those 17 rebinds become
17 blockers, concentrated in loop-carried accumulators — which is exactly the nested case
habu-rebind-a-typed-b2a3e369 says to refuse in its first pass.

PREREQUISITE 2 IS UNDERSTATED, measured the same day on the same tree. Wiring the four
stages into src/compiler/native/migrate.f EMITTED is necessary but NOT sufficient, and the
missing half is the routine's FRAME.
  - Today a spilling body never produces a plan at all. migrate.f ROUTINE builds NABI:LEAF /
    NABI:CALL, which are `0 LEAF-FRAMED` / `0 CALL-FRAMED` (src/compiler/native/abi.f:107,
    :126) — a ZERO-slot frame. So A64RA:NEW-SLOT (regalloc.f:641-646) has no room for the
    first slot and throws E-A64RA-PRESSURE before any spill is decided. Measured through the
    real NMIGRATE:DEFINE entry: a straight-line body holding 8 values at 4 registers returns
    -8329; the same body at 18 registers returns 0.
  - The loop itself WORKS once the frame is non-zero. With the four stages wired and a frame
    declared, that same 8-value body compiled end-to-end through NMIGRATE:DEFINE and returned
    0. So the wiring is not the hard part.
  - The hard part is that the frame must be declared BEFORE the allocator can be asked how
    much of it is needed, and the chain gives exactly one chance to ask. Three structural
    facts, each measured: (1) A64SEL:SELECT ends with IR-BUILD:FREEZE on the a64 builder
    (select.f:3787), so the selected module can be allocated ONCE — re-binding A64RA to that
    builder returns -8061 E-IR-BUILD-FROZEN; (2) A64SEL:BIND-SOURCE needs the HIR builder
    live (select.f:3668-3674) and the first selection freezes it, so selection cannot be
    repeated either; (3) A64SPILL:REWRITE sizes its reserve from A64RA:FRAME (spill.f:1035),
    i.e. the contract of that one allowed walk. Together: the frame the emitted routine
    reserves is the one declared before the allocator was consulted.
  - Declaring a ceiling frame compiles but is wrong to land — it reserves NFROZEN:VMAX slots
    for every spilling routine, which is not "exactly the frame its reserve takes", the thing
    A64RAV is documented to check and habu-derive-a-routine-84ed36b6 owns.
  - So PREREQUISITE 2 cannot close on its own: it needs habu-derive-a-routine-84ed36b6
    landed with it, in one of two shapes — A64SPILL sizing its reserve from A64RA:FRAME-USED
    rather than A64RA:FRAME, or A64RA recording the demand instead of refusing at NEW-SLOT so
    a single walk under today's contract answers how many slots the program needs. The second
    is the one habu-derive-a-routine-84ed36b6 already describes as "a contract whose frame
    field is filled in by the allocator", and it is the only one that keeps the non-spilling
    path to a single walk and therefore bit-identical.
  - Also found, not blocking but real: wider spilling bodies (12 and 20 live values) reach
    E-IR-CTX-SCRATCH -6644 after the rewrite — the migration's context mapping does not fit
    the arenas a rewritten module needs. Needs its own dot before the cut relies on spills.
  - A routine that CALLS is a separate case and stays refused: the selector builds its frame
    (select.f:905-909 PROLOGUE) and the lowering pass keeps that prologue rather than
    resizing it (spill.f ONCE-CK), so nothing after selection may widen it. That is
    habu-exercise-a-call-dda45093, and it starts at the selector.

STOPPED 2026-08-06 (agent=thecut, workspace .jj-ws/habu-cut-colon-compilation-a5aa3f1f,
tree at master 09a6bceb, engine refreshed to fixpoint aaa8882b). NO CUT WAS ATTEMPTED AND
NONE SHOULD BE UNTIL THE THREE BLOCKERS BELOW ARE CLOSED. The leaf's premise — "every
prerequisite is closed; route the entry through the chain" — is false. The prerequisite
list above is not wrong about what it lists; it is incomplete, and what it omits is larger
than everything it names. Measured, not argued:

BLOCKER 1 — THE CHAIN IS A POST-PASS OVER THE OLD EMITTER, NOT AN ALTERNATIVE TO IT.
The chain's only front end is NMIGRATE (src/compiler/native/migrate.f:692 DEFINE). Its
input is the token TAPE, and the tape has exactly one producer: src/compiler/native/feed.f
hangs on the checker's own reader, src/core/checker.f:10231 CHECK-SCAN, which the engine
runs from its check hook AT EVERY `;` — that is, after COMPILE-EMIT:EM-COMPILE has already
emitted the whole body. migrate.f reaches that reader by calling `evaluate`
(migrate.f:88-89 TRUSTED: EV, reached from RECORD -> SCAN at :278-296), which re-enters the
engine's ordinary interpret and compile path. So a migration REQUIRES a successful old-emitter
compile and publication before it can begin, and migrate.f enforces exactly that by name:
PUBLISHED-ONE (:301) refuses unless the dictionary grew by one record, and E-NMIGRATE-VERDICT
(:297) refuses unless the engine's own check certified the definition.
Consequence: "route COMPILE-EMIT:EM-COMPILE through the chain" is circular as written —
EM-COMPILE would call a chain whose entry calls `evaluate`, which re-enters EM-COMPILE.
There is no seam to flip and no flag to add. Making the chain the sole compiler needs a
compile mode that PARSES AND CHECKS WITHOUT EMITTING, so the tape exists before any code
does; the engine has no such mode, and building one is a habu2.f change of the same order as
the emitter it would replace. Nothing in the tree stages this, and no dot owns it. FILE ONE.

BLOCKER 2 — THE CHAIN COMPILES ABOUT A THIRD OF THE DIALECT, AND THAT IS THE WHOLE OF THE
REMAINING WORK. Counted from the tables, not estimated. The chain's modeled vocabulary is
61 spellings, committed as a constant at src/compiler/native/hir-word.f:938 (`61 constant
WORDS`) and registered by HIR-WORD:REGISTER-WORDS (hir-word.f:1184): 4 arithmetic, 6
compare, 6 bitwise, 4 const-op, 4 memory, 9 float, 5 float-compare, 13 control, 2 locals
markers, 8 renames. The engine's compile path carries 70 keyword/op rows (habu2.f:6274,
6291, 6300, 6315, 6370-6417) over 174 primitives (habu1.f FPRIM/FPRIM-L/GDEREF sites, plus
habu2.f and prof.f). Anything not in the 61 is E-HIR-UNMODELED at hir-word.f:815.
REFUSED TODAY, each verified absent from the tables: string literals and char literals
(hir-word.f:922-927 ADMIT-TOKEN throws E-HIR-KIND on any tape kind that is not int literal,
real literal or name); `case`/`of`/`endof`/`endcase`; ADT `match`/`;match`/`construct`;
quotations `[: ;]`; `does>`; `do`/`loop` (only `?do` is modeled), `+loop`, `leave`, `j`,
`again`; the return-stack words `>r`/`r>`/`r@`; `execute`; `['`]/`postpone`/`[char]`/`is`;
and the ordinary primitives `negate 0< mod /mod abs min max tuck -rot ?dup 2swap 2over +!
cell+ char+ chars count type`. The type substrate models two value types — signed 64-bit
integer (elaborate.f:344) and IEEE754 double (hir.f:535) — over 8 flat kinds
(src/compiler/ir/type.f:196-203: int, float, pointer, quotation, code-ref, memory-token,
mask, opaque). There is NO struct, NO array, NO aggregate, NO tagged-union kind anywhere in
the IR, so ADTs, products, structures and value-records are not "unimplemented rows" — they
have no representation to be implemented into. The stdlib is written in the dialect the
ENGINE accepts. It cannot compile through the chain today and the gap is not vocabulary
rows; it is a type substrate plus a dozen control forms.

BLOCKER 3 — THE ENTRY'S FACTS ARE THE CALLER'S, AND ONE OF THEM IS MUTATION-PROVED UNSOUND.
migrate.f's own header (:44-72) names four open dots as the distance between this harness
and a compiler, and ALL FOUR ARE OPEN: habu-bind-checker-env-ed4f9f87 (the declared arity is
restated by the caller, not read off the unit the checker certified),
habu-choose-the-register-a95390ac (the register budget is a hand-chosen number),
habu-resolve-a-callee-0340dfde (a callee's address AND declared effect are the caller's
word), habu-parse-a-migrated-b38a83d9 (the definition arrives as an s" literal, not from the
input stream). None of the four appears in the prerequisite list above.
habu-resolve-a-callee-0340dfde carries a mutation proof dated 2026-08-02: a body
`: LIE-W ( n n -- n n ) LIE-DBL ;` whose callee really takes and leaves one value but is
DECLARED as taking and leaving two selects, allocates, passes the register-allocation
validator, emits and publishes with NO THROW — because the selector builds both the
store/load runs and the two byte counts from the same stated arity, so the two derivations
it holds against each other always agree. Routing production compilation through an entry
whose callee facts are unchecked caller assertions would make every miscount a silently
wrong program. That dot is a hard prerequisite of the cut and is not listed as one.

HARD CAPS, measured through the real entry on this tree (not read off constants):
`: CP-A ( n -- n ) dup + ;` migrates, rc=0. A 559-byte source refuses rc=-8571
E-NMIGRATE-TEXT (TEXT-CAP 512, migrate.f:96). Also: TAPE-CAP 128 tokens (:97), NAME-CAP 64
(:103), CALLEES-MAX 4 (:122) — a definition may declare at most FOUR called words — and
exactly one `create`d data word per migration (DEFINE-DATA, :744; M-DATA-U is a single
slot). These are buffer sizes and are the least interesting of the blockers; they are
recorded so a later reader does not mistake them for the reason.

WHAT THIS MEANS FOR THE ORDER. The cut is not phase 4 of a campaign whose earlier phases
are done; it is the LAST step after the chain becomes a compiler for the language. Ordering
that follows from the above, none of it started: (1) a no-emit compile mode in the engine so
a tape exists without the old emitter (unowned — file it); (2) the type substrate for
aggregates and tagged unions (unowned — file it); (3) the refused control forms and literal
kinds (unowned — file it); (4) habu-resolve-a-callee-0340dfde, habu-bind-checker-env-ed4f9f87,
habu-choose-the-register-a95390ac, which together make the entry state nothing the engine
already knows. Only then does routing EM-COMPILE mean anything, and only then can the
stdlib-compiles proof, the fixpoint and the sole-path gates be attempted.
habu-delete-the-old-679cfd35 stays blocked behind all of it.

RECONCILIATION with the two staged-cutover dots, as this leaf asked for.
habu-cut-over-staged-070d68c8 ("Cut over staged native compiler") and
habu-self-host-staged-520ab588 ("Self-host staged compiler") are BOTH SUBSUMED BY THIS LEAF
and should be closed as such rather than extended: each states the same deliverable in the
older design-document vocabulary (make the staged compiler default; prove a byte-identical
fixpoint; delete the old direct paths; update the size baseline), each carries acceptance
criteria phrased against `docs/compiler-ir-design.md` sections rather than against the
tree, and neither adds a requirement this leaf does not already carry. Keeping three open
dots for one cut is how the prerequisite list came to be believed complete. They are left
open here only because closing another lane's dots is not this claim's to do; the
recommendation is recorded and the merge review should act on it.

ALSO FOUND, not blocking, worth a dot each: docs/compiler-ir-design.md:809-860 ("As
implemented: the straight-line subset") is badly stale — it says "Five opcodes" where
hir.f:247-292 now defines 44, and "a source word means one of four things" where hir.f:364-376
defines 11. src/compiler/native/elaborate.f:10-14 says "the two cell-width memory words"
where the table has four, and omits floats and calls entirely. A stale statement of what the
chain compiles is precisely what let this leaf's premise stand unchallenged, and correcting
it is worth more than it looks.

TWO MORE MEASUREMENTS from the same session, both bearing on the plan above.

PREREQUISITE 5 IS NOT A REORDERING, IT IS AN ADDITION. The leaf says to derive the seed by
transitive closure from the chain's entry, "ir/* before native/*". Measured: NEITHER
src/compiler/ir/* NOR src/compiler/native/* appears anywhere in tools/srclist.f or in
tools/bootstrap.sh's SRC_COMMON. The chain is not in the engine seed at all today - it is
ordinary runtime-loaded source, reached by `require`. So seeding it is not a matter of
ordering ir/* ahead of native/*; it is putting roughly 35,000 lines of compiler into the
image for the first time, with the AOT x9 hazard (ACAP-SCAN-DATA) gating capture exactly as
this leaf warns. Whoever plans that step should size it as new work, not as a sort.

THE JSON-READ-PERF RATCHET REDS THE GATE AT RANDOM ON THIS HOST, ON EITHER TREE. Measured
over ten full suite runs, because a first guess at the mechanism was wrong and is corrected
here rather than left standing.
The budget is NOT fed by a stored history - `stored=30` is the sample count of the current
run. Each budget is a hardcoded baseline (lib/json-read-perf-test.f:111-121) plus ten
percent, scaled by a host calibration factor measured once per run. The factor is a step
function of the PRE-calibration draw alone, and three draws occur on this host:
pre=103 gives the raw-decode row budget=130491136, pre=104 gives 131699387, pre=105 gives
132907638. Nothing else moves it.
On this host the two production-path rows have no real headroom left after that scaling.
Ten runs: six on the tree with these two commits (four green, two red) and four on
unmodified master (four green). Every red is the json-read-perf phase and nothing else. The
margins are the finding: the worst breach was 0.64%, and the smallest was
"repeated escape-heavy decode" at fastest=225709500 against budget=225706732 - OVER BY 2768
NANOSECONDS ON A 225.7 MILLISECOND MEASUREMENT, which is 0.0012%.
The tree is not the discriminator, and that was checked rather than assumed. This tree drew
the tight pre=103 budget and PASSED the raw-decode row on it (mine6: fastest=130067375 vs
130491136); master has simply not drawn the unlucky calibration in four runs. The change in
these two commits is comments, documentation and one dot leaf; the engine is byte-identical
across it (aaa8882b before and after a forced fixpoint rebuild), and none of the edited
files is on the JSON decode path.
So master is exposed to exactly the same coin flip, and the project's blocking merge gate
currently reds on measurement noise a fraction of a percent wide. That is worth its own dot
and is the reason this is written down. It was NOT touched here: retuning a ratchet so that
one's own change goes green is the move the discipline forbids, and the honest report is
that this branch is four-green-two-red over six runs for a reason that is not in the diff.

BLOCKED (2026-08-06, structural audit at 04a462e0 — the 'every prerequisite closed' claim above was FALSE and is retracted): the cut waits on (1) habu-give-the-chain-5ed1f7c5 no-emit compile mode — the chain is today a post-pass whose input exists only after the old emitter succeeds, so routing the entry through it is circular; (2) habu-give-the-ir-f0cfa96a aggregate/tagged-union IR kinds; (3) habu-complete-the-chain-5aab8cee dialect completion (61 of 174-primitive surface); (4) habu-resolve-a-callee-0340dfde and the caller-stated-fact family (one mutation-proved unsound). Seeding is not a reorder — src/compiler is ~35k lines never yet in the image. The audit's refusal inventory and gate evidence live in the 04a462e0 commit and this leaf's history.
