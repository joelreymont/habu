---
title: Six zero-arity calling routines emit a needless data-stack access
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-09T23:10:26.155829+02:00\""
---

Measured 2026-08-09: admitting ( -- )-with-call routines turns 6 definitions red with E-A64RAV-DKEEP (-8611, 'a data-stack access the emission had no reason to make'). Either the call-site builder emits a redundant store/load for this shape or the DKEEP clause is over-strict for it; not yet diagnosed. Reduce one, name the emitting site or the over-strict clause, fix at the root with a failing-then-passing test. Files: src/compiler/native/{select,regalloc-verify}.f. Depends: habu-declare-a-routine-0c14617b (the admission that exposes them).

RESCOPED BY THE CENSUS 2026-08-12: not six definitions - ONE HUNDRED
TWELVE first-refusals E-A64RAV-DKEEP across 60+ files (reps argv.f MOCK+;
raw rows /tmp/hb-census-scout/refusals.tsv). This is the validator
refusing the chain's OWN emission for a needless data-stack access - a
chain defect class, the second-largest measured blocker on the cut after
POOL. Diagnose the emission pattern first (why does the chain emit a
data-stack access the routine does not need?), then fix the emitter, not
the validator. Priority raised accordingly.

MECHANISM ESTABLISHED (dkeep scout 2026-08-12, raw /tmp/hb-dkeep/ incl. the
promotable verifier-dumper patch): ONE mechanism, all 111 reproducible rows
- the residency fact has TWO WRITERS. select.f DOP-XFER (planning) kills
the whole data-stack map at every addressed store per the declared aliasing
model; the emission walk keeps its own map inside CALL-SAVE/CALL-RESTORE/
DEXIT-XFER and OMITS the kill, so the need pass plans a save-store the
emission elides, the restore-load's result is read by nothing, and
VDLOAD-CK (the uses-at-0 clause, all 111 rows) refuses the chain's own
emission. Defect landed WITH the pass (784f7e6d, 2026-08-04); the
population grew as admission widened. Both one-line variants (K: kill in
EMIT-ASTORE; O: no kill in DOP-XFER) clear all 111 with zero regressions.

RULED 2026-08-12: SINGLE-WRITER RESTRUCTURE WITH POLICY O (optimistic,
domain-separated). The emitters consume DOP-XFER's one transfer (mask as
argument to EMIT-WORD-CALL/EMIT-CALL/EMIT-TAIL-CALL/EMIT-RETURN + the
region walk; DSAVE-XFER mutates - double application is the hazard).
Policy: an addressed store does NOT kill stack residency, because checked
code cannot form an address into the stack region - a DOMAIN claim, not
optimism: ADDR-MEM's "unrestricted" aliasing is unrestricted among
data-domain memory and excludes the stack domain by the type system (no
checked path mints a stack address; raw casts are trusted-gated). The old
pessimistic comment was never true of the emission and pessimism is not a
substitute for a stated invariant. REQUIRED: (a) the domain claim written
at DOUT-AT and the alias declarations; (b) a fixture attempting the
closest CHECKED approximation of stack aliasing, proving the type system
refuses the path - if one CAN be constructed, STOP: policy K becomes
mandatory; (c) the zero-regression verdict-diff repeated on the final
tree; (d) baselines re-pinned with derivations; (e) the verifier-dumper
promoted to tools/. Fix is select.f-only, medium diff.

Claim: agent=dkeepfix workspace=.jj-ws/habu-dkeep-fix

DOMAIN CLAIM WITHDRAWN, REFUTED BY ITS OWN FIXTURE (dkeepfix lane
2026-08-12). The ruling above required a fixture proving the type system
refuses a checked store into the data-stack region, and said policy K
becomes mandatory if one could be built. One can: `run-in-stack` is a
CHECKED primitive (src/core/checker.f) that installs a buffer the caller
owns as the data stack a routine's slots live in (src/habu/habu1.f
BRUNSTACK sets x19 = base), so checked code holds the address of the
region and an ordinary `c!` reaches a live slot. The domain claim is
false and is withdrawn.

THE FIXTURE BUILT TO MANDATE K INSTEAD PROVED K MISCOMPILES. One checked
body - a call's result in slot 0, the low byte of that slot poked through
the computed address, the value then handed to a second call - measured
three ways on one tree:

    tree                     engine column   chain column
    master dae6641a               200        REFUSED -8611 (this dot)
    policy O (no kill)            200        200   agrees
    policy K (kill kept)          200          7   MISCOMPILES

The engine's own emitter keeps nothing in a register across a store, so
it always shows what the program wrote. A kill only ADDS stores, and an
added store writes the register copy back OVER the poke - so K is weakly
worse on every program of this class and measurably worse on this one.
Memory is authoritative at the transfer points and registers live only
between them: where checked code aliases its own stack, ELISION is the
faithful choice and the kill is the unfaithful one. Policy O stands on
that ground, not on the withdrawn claim. Written at select.f DOUT-AT and
at the a64ir.f alias declarations; the UNRESTRICTED alias declaration is
untouched, being the ordering statement and a different question.

The body is now test/compiler/native-dstack-alias.f (registered in
test/gate-stdlib-cases.f and test/gate-stdlib-inline-lib.f): it is both
the aliasing witness and a minimal checked reproducer of this dot's own
refusal. The scout's instrument is promoted to tools/codegen-verify-dump.f
behind one deferred hook in A64RAV, documented in docs/debugging.md.
