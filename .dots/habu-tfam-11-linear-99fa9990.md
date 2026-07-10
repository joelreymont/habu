---
title: "TFAM 11: linear semantics for layout values"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.950894+02:00"
---

PLAN.md item 11. LAYOUT-LINEAR?/LAYOUT-LINEAR-COUNT over expanded fields; layout containing linear payload is linear; reject raw drop/copy/branch-loss/unconsumed; MATCH consumes/refines exactly once; extend taint/laundering checks (quotations, KEEP, BI, deferred calls, delayed resolution) from scalar LIN-CON? to layouts. Unlocks the TFAM 8/9/12 linear-reject gates. Gate 17l. Depends: TFAM 7-10, 12.

SLICE 1 LANDED (commit "TFAM 11: expand non-linear parametric layouts wide"): the width/publication core. (a) LOGHID coercion in the unifier (checker.f LOGHID-AT?/LOGHID-EXPAND, new U-ROW arm, non-transport mode only): when a 1-cell logical parametric layout value meets the W-cell hidden expansion of the same family at a boundary/call site, the args unify pairwise against the hidden side's (always fully-resolved non-linear, by the PUSH-LOGICAL invariant) and the logical cell expands via LAYOUT-PUSH-FIELDS, re-pairing the rows. (b) PARAM-ARG-LIN-BLOCK? in PARAM-PAIR-ARGS: a var arg may never bind a linear con through param-arg pairing (inert while LIN-NDECL=0), so hidden groups can never absorb a linear payload mid-unification. (c) sumtype.f arity>0 constructor gate LIFTED: parametric families publish; the constructor's parametric result stays one conservative cell in generic contexts (transports still reject) and expands where instantiation proves non-linearity. Genuinely-linear cases stay fail-closed: linear-arg layout sigs already reject at the sig/borrow layer (pinned ZP5/ZP6/ZP7) and ZP8 pins the possibly-linear transport reject. Fixtures: test/type-ctor-suite.f parametric block (zpoly/zpmix: concrete + ptr-payload + generic wrapper + wrapper-instantiation + cross-family + linear + transport negatives).

REMAINING item-11 work: whole-bundle linear counting over hidden fields (LAYOUT-LINEAR?/LAYOUT-LINEAR-COUNT; lets linear-payload ADTs construct/flow with exact accounting, replacing the fail-closed rejects); EN-MULT/EN-PARAM multiplicity treating a linear-layout param as one linear unit (currently unreachable: linear-arg layout sigs reject upstream); MATCH consume/refine-exactly-once (needs item 9); taint/laundering extension (quotations, KEEP, BI, deferred calls, delayed resolution) from scalar LIN-CON? to layout values; TLP seed retirement now unblocked for the MK rows (item-8 slice 4: tlp-res/tlp-mix constructors publish as of this slice).

SLICE 3 LANDED (commit "TFAM 11: whole-bundle linear accounting"): exact accounting core. LAYOUT-LINEAR?/LAYOUT-LINEAR-COUNT (docs §19 helpers; flat cell-kinded arg scan) + LIN-TYPE-COUNT counts a bundle ONCE at its tag cell (hidden tag-slot sampling) and a conservative 1-cell logical layout by its con args, so the existing per-step conservation (CHECKER-STEP LIN-SNAPSHOT/LIN-CHECK) makes constructor calls conserve: `( ltok -- lq2<ltok,n> ) LQ2:OK` certifies (payload absorbed into the bundle, before=after=1) and `( n -- lq2<ltok,n> ) LQ2:ERR` mints (out-only var unbound at the step, binds at the boundary — option-NONE semantics; no boundary count equation exists, verified). PUSH-LOGICAL expansion condition changed from possibly-linear to LAYOUT-ARGS-OPEN? (any unresolved var arg): width-known linear layouts now expand to hidden fields, so checker rows and runtime cells agree for linear bundles too; open-arg layouts stay one conservative cell. XG-READ-GROUP rejects LINEAR groups (fail-closed for ALL transport classes v1 — dup/drop/swap/>r/r@/2*-class and locals capture all route through the carve). Slice-1's PARAM-ARG-LIN-BLOCK? REMOVED: superseded by counting and it wrongly rejected the err/none mint's var~linear arg bind. EN-MULT already walks EN-PARAM args (verified by reading — no change needed; the census's EN-PARAM concern was already implemented). Calibration finding: earlier own-based "linear sig reject" pins were vacuous (own is an UNKNOWN TYPE in suite contexts — rejections were E-UNDEFINED-type, not linear machinery); real pins now use `deflinear ltok` in test/type-linear-suite.f (A1-A7 accepts incl. padded/multi-cell/wrapper flow-through; R1-R11 rejects incl. copy/drop/swap/rstack/local/unconsumed/branch-loss/forge/payload-reuse/payload-drop; S1-S3 scalar discipline unchanged). Suite wired as GE-TYPE-LINEAR-SUITE.

REMAINING item-11 work: move-class transport relaxation for linear bundles (swap/rot/>r conserve count — currently all-reject v1); MATCH consume/refine-exactly-once (waits on item 9 by design); taint/laundering extension for DELAYED arg resolution (a var-arg bundle local/copy whose arg later binds linear — currently unreachable: open-arg layouts stay 1-cell and all their transports/locals/rstack reject, pinned by ZP8/P5/P6 probes); item-8 TLP UN-row retirement at item 9.
SLICE 4 LANDED (commit "TFAM 11: relax move-class transport for linear bundles"):
move-class transport relaxation. XG-READ-GROUP's blanket linear-bundle reject
(checker.f, the `LAYOUT-LINEAR? -> 0 OK !` arm) is removed: a resolved linear
hidden bundle now reads through XG-READ-HID like a non-linear one, and
XPORT-APPLY's existing LIN-SNAPSHOT/LIN-CHECK conservation (LAYOUT-LINEAR-COUNT
counts the bundle once at its tag) classifies it — a permutation keeps
before=after (ACCEPT), a copy raises the count and a drop lowers it (REJECT).
So swap/rot/-rot/2swap and >r/2>r round trips of a linear bundle certify, while
dup/over/tuck/2dup and drop/nip/2drop still reject, with NO new special-case
code (one consumer graph, §3/§C4). Boundary loss (a move that then drops the
bundle at the sig, or strands it on the return stack) still rejects — the
per-step conservation plus boundary balance catch it. Locals capture is
UNCHANGED: LOC-BIND-GROUPS removes the bundle from the counted rows at bind, so
the same LIN-CHECK rejects a linear layout local (R5/TDLIN-LOCAL stay reject).
OPEN-arg (may-later-bind-linear) layouts are untouched — the LAYOUT-PARAM? reject
below still fail-closes ALL their transports including moves (TDLIN-VAR-TOR), that
is the separate delayed-resolution piece. Fixtures: test/type-linear-suite.f M1-M7
(swap/rot/-rot/2swap/>r r>/2>r 2r>/two-bundle-swap accept) + MR1/MR2 (move-then-
lose / strand reject) + R4/R6/R12/R13 (over/tuck/nip/2dup reject); type-decl-suite
TDLIN-TOR flipped to accept + TDLIN-VAR-TOR added (open-arg move stays reject).

REMAINING item-11 work: MATCH consume/refine-exactly-once (the item-9 MATCH
checker frame — MATCH-BEGIN/MATCH-TOK/MM — now EXISTS; audit its current linear
behavior and add per-arm exactly-once accounting: next tractable slice);
taint/laundering extension for DELAYED arg resolution (open-arg bundle
local/copy whose arg later binds linear — the LAYOUT-PARAM? reject keeps it
fail-closed today, pinned TDLIN-VAR-DUP/TDLIN-VAR-TOR); item-8 TLP UN-row
retirement at item 9.

SLICE 5 LANDED (commit "TFAM 11: MATCH consume-once proof fixtures"): audit +
soundness-proof hardening. AUDIT FINDING: MATCH consume/refine-exactly-once is
ALREADY correctly implemented and sound — no checker gap. Mechanism:
MATCH-FAM-TOK pops the scrutinee bundle once at entry (structurally unusable
after); MATCH-OF-TOK refines each arm with the variant's instantiated payload
(MATCH-PAY-XT/TFAM-MATCH-PAY); the arm body runs under the normal per-step
LIN-CHECK, which enforces the refined linear payload is consumed exactly once;
the join (MATCH-ACCUM/MATCH-SEMI) unifies arm outputs. No MATCH-specific linear
code was needed (composes from refinement + existing conservation, like the
transport slice). Verified by 8 probes (all correct, no soundness hole): full
consume via destructor=accept, leak=reject, double-free=reject, exit-leak=
reject, forward-through-join=accept, forward+free(copy)=reject, return-stack
move-then-consume=accept (proves the slice-4 move relaxation composes with match
refinement), return-stack strand=reject. Landed as test/type-match-suite.f
ML5-ML12 (the block had only ML1-ML4, under-covering the use-after-free
invariant), using FREE-MTOK (abstract linear consumer; engine-suite T-FREE-OWN
pattern) + one census-§6-sanctioned trusted-inventory-classes row
(test/type-match-suite.f test-metaprog, owned by this dot). Test-only, no checker
change (no byte-fixpoint). The one apparent nested-linear-match reject
(reconstruct+inner-match) is E-MATCH-OPEN-ARGS: `construct mlin ok` yields
mlin<mtok,?b> with b unresolved, and matching an open-arg scrutinee is the known
v1 reject (MB19) -> the delayed/open-arg resolution piece, NOT a linear gap.

REMAINING item-11 work: delayed/open-arg resolution (E-MATCH-OPEN-ARGS / MB19 /
LAYOUT-PARAM? reject for partially-determined args — width-resolution +
refinement; the substantive remaining checker slice); item-8 TLP UN-row
retirement at item 9.

PIECE 3 AUDIT (2026-07-10, engine lane) — delayed/open-arg resolution.
RECOMMENDATION: STOP-AND-REPORT. The open-arg reject is over-conservative but
lifting it is a substantial, entangled slice, NOT a minimal one — scope it as a
dedicated feature/dot; the current fail-closed reject is sound v1 meanwhile.

Findings:
1. WIDTH IS NOT THE BLOCKER. v1 params are cell-kinded, so a layout's width
   (slots+tag) is FIXED regardless of args: `mlin<a,b>` is always width 2. So
   "refine an open-arg bundle to its resolved width" is trivial (width is always
   known). The blocker is LINEARITY resolution, not width.
2. OVER-CONSERVATIVE vs the scalar discipline. Scalar generics DEFER linearity to
   call sites: `( a -- a a ) dup` and `( a -- ) drop` both certify (-1) today; a
   call with a concrete linear is caught at the call step by LIN-CHECK. The layout
   analog `( tdlin<a> -- tdlin<a> tdlin<a> ) dup` REJECTS (0). Inconsistent — the
   layout case blanket-rejects where the scalar case admits + defers.
3. BUT THE 1-CELL OPEN-ARG REPRESENTATION IS LOAD-BEARING, not pure conservatism.
   PUSH-LOGICAL keeps an open-arg layout ONE logical cell (LAYOUT-ARGS-OPEN?
   gate). Empirically removing that gate (always expand — width is fixed) BREAKS
   the construct->boundary coercion: a constructor yields a partially-open bundle
   (`lq2<ltok,?b>`, the un-provided variant's arg open), and expanding it breaks
   the raw->hidden SUNI/LOGHID coercion. Reverted-probe result: type-linear A1/A5/
   A6/A7 (all construct-accept), plus type-decl/type-match/type-ctor cases, flip to
   E-REJECTED (declared==inferred effect, yet rejected). So the gate does real work
   holding constructor outputs at 1-cell for boundary unification.
4. NO MINIMAL WIN. Even a MOVE (swap/rot/>r) of an open-arg bundle needs the
   W-cell expansion for correct lowering — a 1-cell move would miscompile the
   W-cell runtime value — so there is no "move-only" narrow slice without the
   expansion that breaks (3).
5. SOUND LIFT SCOPE (a coherent dedicated feature, not a tail item):
   (a) expand open-arg bundles to their fixed width WHILE preserving construct's
       raw->hidden coercion for partially-open bundles (rework SUNI/LOGHID so a
       hidden field carrying an open var arg unifies with a resolved hidden field);
   (b) extend the deferred linear taint to layout arg vars (this dot's
       "delayed-resolution taint": XG-TAINT-SEQ/LIN-TAINT currently taints only
       copied scalar var groups and skips hidden bundle groups on the premise
       "linear layouts never expand"; a copied open-arg bundle must taint its arg
       vars so a LATER linear binding rejects, mirroring scalar LIN-TAINT-SCAN);
   (c) verify call-site LIN-CHECK + LIN-EFF-PASS/EN-PARAM catch linear
       instantiations of a generic bundle transport (the scalar model);
   (d) lift the MATCH open-arg reject (E-MATCH-OPEN-ARGS/MB19) once the scrutinee
       expands.
   Pins that flip when it lands: TDLIN-VAR-DUP/TDLIN-VAR-TOR (type-decl-suite),
   MB19/B19 (type-match-suite), ZP8/P5/P6.

Net: the reject is over-conservative, but its removal is entangled with construct
coercion + deferred linear taint + MATCH, i.e. a dedicated multi-part slice. It is
a documented v1 limitation until that feature is scoped and built; no minimal
in-tail fix exists. Recommend a new dot for the feature; TFAM 11 core (slices
1/3/4/5) is otherwise complete.
