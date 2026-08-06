---
title: Cut colon compilation onto the checked chain
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.611694+02:00"
---

CG-01, phase 4 of the hard cut: make checked HIR plus the native pipeline the sole compiler for normal colon definitions. habu2.f:7020 COMPILE-EMIT:EM-COMPILE is the single production entry (verified, no drift); route it and bootstrap through the chain, prove self-hosting to a byte-identical fixpoint, run every gate on the sole path, then delete the old emitter and bridges (habu-delete-the-old-679cfd35). An opt-in hook is not completion. Reconcile with habu-cut-over-staged-070d68c8 / habu-self-host-staged-520ab588.

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
