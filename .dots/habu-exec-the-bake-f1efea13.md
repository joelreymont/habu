---
title: Execute the bake over-engineering audit
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-16T19:07:34.140104+02:00\""
---

The 2026-08-16 read-only audit's verdicts, to execute as one lane AFTER bake-chain-15 lands (most items touch its files). In priority order: (1) READ-SIDE REFUSAL GAP (destruction major): aot-file.f grew ~10 named refusals past the 13-proven set with no standing case (negative section length :475, runs-past-payload :485, sections-fill :490, ?EXACT :500, ?SAME-HEADER :459, ?PAYLOAD-AGAIN :583, SKIP-SECTION :517, closure-ends-inside :553-560, payload-shorter :428) - give each a case or delete the dead ones, mutation-argued. (2) SPLICE LIBRARY: ~85 lines duplicated verbatim between tools/aot-chain-bake.f and test/aot-wid-build.f, third owner arriving with the production driver generation - ONE splice library, all consumers; then the bake tool shrinks to a ~15-line thin caller or dies (no doc references exist). (3) WINDOW LATCH UNIFICATION: one WINDOW-OPEN/CLOSE pair in AOT-ARM latching b/r/d/w (+unarmed variant), deleting ~5 hand-latch quartets and folding WID-SPAN's refusal; PRELUDE-MARK stays separate (process-start is a different moment); forge-by-poke keeps every fixture's wrong-value power. (4) ACAP-W32! deleted for AOT-P32! (one-line cascade - the deferred-cascade claim was false). (5) Cap derivations: AOT-PWIN-MAX 4096 and XTOFF-MAX 4096 get structural-or-measured derivations per aot-decl.f's own standard; if structural (PROT-WID-MAX), delete the provably-dead overflow refusal. (6) wid-build hygiene: 'Twelve modes' vs 16; 'does not exist yet' for a landed producer; three identical emitted helper blocks -> one; XTSITE mode deletion argued vs aot-wide-format's mutation table. (7) O-SECTIONS second-authority field folds into the NEXT version bump only. (8) Differential-bake dot ce93a41c: close as subsumed once the lane's DATA-cursor audit lands as described (same variable as its stimulus; PROBE-BAKED is the behavioural net) - verify the audit's exact shape first. KEEP verdicts recorded by the audit (13 refusal cases all load-bearing, suite's own header constants, ACAP-ALIAS-SEED, per-capture proofs, cross-package accessor spellings) - do not relitigate.

Claim: agent=audit-exec workspace=.jj-ws/habu-audit-exec (items 1, 2, 3, 7, 8 - the rest)
Prior claim: agent=audit-fix workspace=.jj-ws/habu-audit-fix (items 4, 5, 6 only)

ITEMS 4/5/6 LANDED 2026-08-16 (audit-fix, merged db19c865):
ACAP-W32! deleted (one-line cascade as measured); both caps
structural (XTOFF<-XTCELL-CAP, PWIN<-PROT-WID-MAX) with
derivations, the then-dead PWIN refusal deleted, the XTOFF one
kept on the reset asymmetry; wid-build 12->16 count fixed, one
emitted helper block, XTSITE mode deleted on producibility
(its only unique mutant has no producer - ACAP-OUT-CHAIN
returns early for exactly the enabling values). Stale icode.f
parenthetical corrected in passing. Engine delta = the 16KiB
PWIN allot, isolation-proven. REMAINING: items 1/2/3/7/8 after
bake-chain-16 lands; plus the aot-decl.f:208-213 stale
producer claim found as residual.

ITEM 7 (O-SECTIONS) JUDGED 2026-08-17 (audit-exec): NOT WORTH A
BUMP OF ITS OWN. The field, its hard-equality check and its forged
case have to go together, and the header layout is VERSION's, so
the fold lands at v5 - WHENEVER A REAL CHANGE FORCES ONE. Nothing
here forces one: the whole saving is eight header bytes and one
check, against a recapture of every artifact in flight.

AND THE DRIFT PROTECTION IS NOT THE VERSION EQUALITY. Nothing
makes a SEC-N change bump VERSION - the two constants sit in one
file with no rule between them - so version equality does not
subsume the field. THE PRODUCER KEY DOES. CHECK-HEADER accepts
only an artifact whose 32-byte producer key equals the one the
caller hands in (the metabuild's key for the capture host it just
emitted; the read fixture's key for the engine it is running), so
an accepted artifact was written by the exact binary that is
reading it - which compiled this SEC-N. A section count that
differs is unreachable behind that check, and so, for that matter,
are a wrong magic, version and target.

WHAT THE FOUR EARLY FIELDS BUY IS ORDER, and that is a real job:
they are read before the producer key so a stale or foreign
artifact is refused by the thing that is actually wrong instead of
by "produced by a different engine". Keep them until a bump
retires the section count for free.
