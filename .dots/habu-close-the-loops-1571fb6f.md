---
title: Close the loops clang closed-forms
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-07T13:24:42.724307+02:00\""
---

The real owner of TINY-CALLEE 80 vs 12 (refuted-inlining lane, 2026-08-07, reproduces in 11s): the chain already inlines everything there (BL-COUNT 0) and already has the derived per-site copy rule (inline.f SMALL? = measured body <= in+out+3); the gap is clang computing the closed form seed+4*len branchlessly — induction-variable strength reduction / closed-forming, an optimization class the chain lacks entirely. Likely siblings: SUM-TO (n(n+1)/2), CALL-LOOP-3, MANY-LOCALS — the mechanism-attribution audit (running) confirms the set. Measure-first: name the rows from the audit, the transform operates on the typed IR where induction facts must first SURVIVE the elaborator (the NEON lane recorded that trip counts die in compile-time stacks — that loop-shape work is shared with the vectorizer and lands once), direct-refinement evidence per fold, all answers bit-for-bit incl. boundary trip counts (0, 1, MAX).

Audit confirmation (2026-08-07): the closed-formed set is SUM-TO (Gauss via 128-bit multiply + funnel shift — disassembly in the audit), COUNT-DOWN (csinc), TINY-CALLEE (bic;add lsl#2 — 56 of its 72 gap bytes), MANY-LOCALS (madd by len — byte-neutral but its whole 7.97ns), STORE-LOAD (3*len), PRESSURE-LOOP (sum*len). 112 gap bytes, top of the time list.

CHECKPOINT RULINGS 2026-08-10 (measured basis in the loops lane report):
premise CORRECTED - trip-count facts survive the elaborator as ordinary SSA
(DO-OPEN-DO emits the guard sub, index+limit cross edges as block args); the
'shared with the vectorizer' prerequisite is void. Re-measured gap: 164 bytes
across five rows (the 112 predates branch-collapse). SCOPE RULED: Shape A only
- the affine-accumulator rule acc0 + K*T + m*(start*T + T(T-1)/2) covering
SUM-TO, MANY-LOCALS, TINY-CALLEE (120 of 164 gap bytes, 15.95 of 21.6 gap ns);
Gauss is exact in 64 bits by the parity split (h=T>>1, p=T-1+(T&1), S=h*p -
verified at 0,1,2,3,4,MAX), no smulh, no funnel shift, NO new machine forms.
Refusal boundary: start must be a compile-time constant != MAX (the wrap case
changes the trip count; measured: start>limit runs ONE turn). HOME RULED: a
new HIR->HIR pass between freeze and A64SEL:SELECT (migrate.f SELECTED) - 45
opcodes not 73, and the closed form gets selection, if-conversion and the madd
fold downstream for free. It is the first HIR module copier, NOT a third copy
of the A64 dialect table (that existing combine/spill/emit duplication is
dotted separately). COUNT-DOWN is RE-ATTRIBUTED out of this dot (min(n-1,0)
recurrence; its gap is compare-immediate + loop-invariant hoisting - see
habu-compare-against-a-da4cc639 and the new hoist leaf). STORE-LOAD's memory
recurrence is split to its own leaf. Direct-refinement evidence per fold and
bit-for-bit at trip counts 0, 1, MAX remain binding.

Claim: agent=loops workspace=.jj-ws/habu-close-the-loops
