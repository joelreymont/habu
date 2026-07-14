\ promote-test.f - R7 promotion transition + store-seal acceptance (sub-dot 4
\ v2-promotion, dot habu-v2-typestate-promotion-2266b236). MODEL-CAD-V2-PLAN.md
\ § Transition words / § Acceptance fixtures (:1817-1882).
\
\ CHECK-QUIET-CANDIDATE! returns -1 certified / 0 type-reject / 1 unresolvable (a sealed word
\ not visible from this top-level context); both 0 and 1 are "fails to certify"
\ (test/checker-assert.f). Every negative is paired with a resolving positive control (per
\ LESSONS), so a verdict-1 seal is proven non-vacuous, not a typo.
\
\ How this closes the census (MODEL-CAD-V2-PLAN.md:1564-1583):
\ - probe 2 (forgeable promotion readout): PR-BAD-NOGRANT shows ART:PROMOTE without a
\   POLICY:granted type-rejects - promotion demands the sealed grant, not a satisfiable
\   PROMOTE-OK? tag readout; PR-BAD-RAWMINT shows the promoted mint is unreachable.
\ - probe 3 (store bypass / planted evidence + replay): the raw row writers left the public
\   MAKI surface, so SB-EVID-PUT / SB-EVID-PUT-G / SB-SCHED-PUT (qualified MAKI: from a
\   cross-package top-level context) no longer resolve, while the read surface (MAKI:EVID-GET
\   / MAKI:SCHED-GET) still does. Nothing outside the promote/store owner can plant a row.

require lib/test.f
require test/checker-assert.f
require maki/evidence/promote.f
require maki/store.f

T-RESET

\ ---- positive controls -------------------------------------------------------
\ the sealed promotion transition certifies with a built artifact + a real grant.
s" PR-OK ( ART:built POLICY:granted -- ART:promoted ) ART:PROMOTE"
   CHECK-QUIET-CANDIDATE! -1 T=
\ MAKI: qualification resolves the store READ surface -> the store-bypass verdict-1s below
\ are real unresolvability of the sealed WRITE surface, not a broken qualification.
s" SB-OK-EVID-GET ( ptr u8 n -- ptr u8 n bool ) MAKI:EVID-GET"
   CHECK-QUIET-CANDIDATE! -1 T=
s" SB-OK-SCHED-GET ( ptr u8 n -- n bool ) MAKI:SCHED-GET"
   CHECK-QUIET-CANDIDATE! -1 T=

\ ---- forged-promotion rejects through the checked path -----------------------
\ ART:PROMOTE without the sealed grant type-rejects: a built artifact is not promotable on
\ its own, so planting gate tags to satisfy a runtime readout cannot reach ART:promoted.
s" PR-BAD-NOGRANT ( ART:built -- ART:promoted ) ART:PROMOTE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ the promoted-stage mint is PRIVATE to ART: unresolvable outside its owner, so no caller
\ can fabricate an ART:promoted around ART:PROMOTE.
s" PR-BAD-RAWMINT ( n -- ART:promoted ) RAW>PROMOTED"
   CHECK-QUIET-CANDIDATE! 1 T=

\ ---- store bypass regression (probe 3): the raw row writers are off the public surface ---
\ Each was a public MAKI word before this sub-dot (MAKI:EVID-PUT / -PUT-G / SCHED-PUT all
\ certified -1); sealed to package-MAKI-private, a qualified cross-package reference no longer
\ resolves (verdict 1). Proven non-vacuous by the SB-OK-* read controls above.
s" SB-EVID-PUT ( ptr u8 n n n n n -- ) MAKI:EVID-PUT"
   CHECK-QUIET-CANDIDATE! 1 T=
s" SB-EVID-PUT-G ( ptr u8 n n n n n bool n -- ) MAKI:EVID-PUT-G"
   CHECK-QUIET-CANDIDATE! 1 T=
s" SB-SCHED-PUT ( ptr u8 n n -- ) MAKI:SCHED-PUT"
   CHECK-QUIET-CANDIDATE! 1 T=

T-REPORT
