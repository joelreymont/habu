---
title: "Cast definer: 330 nominal casts want one declaration form"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-19T10:05:19.378520+02:00\""
---

Phase 2 of 4fd12d60: 330 TRUSTED: sites are nominal identity casts - 232 with literally empty bodies, 98 pure stack shuffles (roles.f 34 - which GENERATES the TRUSTED: text at roles.f:40 - process-pty-handle.f 18, cad-num-types.f 12, maki/ 111, tail 96). Build a checked cast-declaration form: a definer that states from-type/to-type and mints the identity with the checker enforcing representation-compatibility (same cell count/roles), replacing trust with a structural check. The roles.f generator then emits the new form. Blocks the final deletion.

Probe lead (2026-08-19): a CAST: definer already exists - tools/judge/cost.f:155
uses `CAST: REAL-BITS ( r -- n ) ;`. Find its definition and semantics FIRST;
the capability may be extending CAST: with representation-compatibility
enforcement rather than minting a new form.

Claim: agent=trusted-2 workspace=.jj-ws/habu-trusted

PROBE ANSWER (2026-08-19, trusted-2). The capability is already built and
already tested; nothing was added. `CAST:` is defined in src/core/roles.f and
certified by src/core/checker.f CAST-CERTIFY. It is a STRUCTURAL check, not a
trusted spelling: the checker certifies the body under the identity row
( in -- in ), publishes the declared ( in -- out ), and refuses the declaration
itself with a named code when it lies about shape - 7129 E-CAST-ARITY (any cell
count other than one in, one out), 7130 E-CAST-CLASS (pointer, quotation, atom
or width>1 term on either side), 7131 E-CAST-FAM (undeclared family), 7135
E-CAST-OWNER (minting a cell family this package does not declare), 7137
E-CAST-LINEAR (either side may carry linear ownership). A cast may therefore
rename a cell and may never reshape one, which is exactly the phase-2 contract.
Coverage already exists: test/cast-suite.f (positive) and
test/cast-negative-suite.f (all five codes).

MEASURED GAP vs TRUSTED:. TRUSTED: catches a shape lie only where some caller's
row happens to disagree, and blames the caller. Proven on master: `TRUSTED: L1
( n -- idx idx ) ;` with an empty body hands the caller its own pre-existing
stack cell retyped as idx (probe printed 5 then 77, then E-UNDERFLOW), and
`TRUSTED: L3 ( n -- ptr u8 ) ;` lets `8 L3 c@` compile and SIGSEGV the engine
(rc 134). Both are refused by CAST: at the declaration, by name.

DONE: lib/cad-num-types.f, 12/12 sites (10 private MINT-* plus 2 private
projections). Suites unchanged and green; a shape-lying mint now kills the
build (7130 measured).

BLOCKED, NEEDS A RULING: src/core/roles.f, 0/34. `CAST:` must RUN at
declaration time and it crosses source through sumtype.f's deferred
TDECL-EVAL-XT, which src/core/include.f arms. roles.f is prefix row 894 and
include.f is row 902, so a single CAST: in roles.f dies "defer: unset execution
vector" (rc 76) - measured with one converted pair. Arming the boundary inside
roles.f makes all 34 convert and the engine build clean (measured), but that
mints a SECOND audited evaluate crossing and arms generation eight files early,
which is the exact fail-closed property sumtype.f:923-927 says it is keeping.
A chain reorder cannot fix it either: include.f needs ZBYTE@ from env-base.f
(row 901), env-base.f sits after roles.f, and roles.f's consumers bytes.f
(>LEN/LEN>N) and os/linux/layout.f (>VA/VA>N x6) sit between - measured by
putting include.f first and getting E-UNDEFINED: ZBYTE@. Note also that
converting roles.f breaks the INSTALLED engine at boot, so any landing needs two
steps (reorder, install, then convert). Options for the orchestrator:
 (A) arm a second evaluate boundary in roles.f - one trusted wrapper replaces 34
     trusted casts, but weakens the single-crossing invariant;
 (B) give CAST: a pushback/next-definition window so it delegates to `:` on the
     live stream and needs no evaluate at all - best long-term, removes the
     generated-text fidelity caveat too, but is a checker.f CAST-PEND protocol
     change;
 (C) move roles.f + bytes.f + os/linux/layout.f after include.f - env-base.f and
     habu/layout.f use no roles and no bytes.f words, so an order exists, but it
     touches five mirrored lists (habu2.f, build-fixpoint.f, boot-pin.f,
     diagnose-hb-core.f, bootstrap.sh) and still needs the two-step landing.
Also found: the "generator at roles.f:40" in this leaf's description emits no
declaration. DECL-SIGNATURE is handed the name span and the signature span
separately, so the `TRUSTED: ` / ` ( ` / ` ) ;` literals were never read by
anything. They are deleted in this lane's roles.f hunk - no behaviour change.

REMAINING SWEEP (unclaimed): process-pty-handle.f 18, maki/ 111, tail ~96.
Each needs the same load-position check first: a file may only use CAST: if it
loads after src/core/include.f.
