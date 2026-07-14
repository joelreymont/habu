\ promote.f - R7 promotion transition + store seal (implementation sub-dot 4
\ v2-promotion, dot habu-v2-typestate-promotion-2266b236). MODEL-CAD-V2-PLAN.md
\ "R7 Design Addendum" § Transition words (:1817-1830), design at :1280-1643.
\
\ ART:PROMOTE is the sealed promotion transition: it consumes the built-artifact witness
\ (ART:built) and the sealed grant (POLICY:granted - mintable ONLY by POLICY:CHECK after the
\ value-level artifact binding held, maki/evidence/policy.f) and mints ART:promoted. There is
\ NO way to reach ART:promoted without a grant, so the forged-promotion probe (planting
\ REPORT:GATE! passes to satisfy the runtime PROMOTE-OK? readout, census probe 2,
\ MODEL-CAD-V2-PLAN.md:1571-1575) is UNREPRESENTABLE at the type layer: promotion now demands a
\ typed proof that a real policy check ran, not a satisfiable tag readout. `promoted`'s only
\ producer is the private RAW>PROMOTED, so a raw n cannot forge a promoted artifact either.
\
\ Companion STORE SEAL (maki/store.f + maki/cad.f, this sub-dot): the raw evidence/schedule
\ row writers (EVID-PUT / EVID-PUT-G / SCHED-PUT) left the public MAKI surface, so nothing
\ outside the promote/store owner (package MAKI) can plant a store row (census probe 3,
\ :1576-1583); and the golden leg/precision provenance now flows as typed EVID values through
\ the promote path (maki/cad.f) instead of the retired maki/golden.f ambient globals.
\
\ DEVIATION (return shape): the R7 addendum types ART:PROMOTE
\ ( ART:built POLICY:granted -- result<ART:promoted,diag-set> ), but result<promoted,_> is not
\ expressible - a result payload is single-cell and `promoted` is a nominal here, yet a
\ polymorphic RESULT-DROP over result<a,b> needs a runtime family lookup (the same wall
\ maki/typestate.f and maki/evidence/policy.f name, dot habu-typestate-result-drop-5ae048a7).
\ This skeleton transition cannot fail (it consumes an ALREADY-granted promotion), so PROMOTE
\ returns ART:promoted DIRECTLY; the error-carrying result arrives with the multi-cell
\ result-payload capability.
\
\ DEVIATION (grant consumption): POLICY:granted is multi-cell (art+pol+tok) so it cannot be a
\ typed local nor a result/option payload (LESSONS multi-cell wall). It is UNMAKEd off the
\ stack and its fields dropped - the grant is the compile-time WITNESS that a real policy
\ check occurred, not read for value here.
\
\ SCOPE: ART:promoted is a fieldless stage nominal like the other maki/typestate.f stages.
\ Threading identity/typed evidence into a REAL store write (so ART:PROMOTE itself becomes the
\ store writer over typed values) waits on public artifact-id / ART:built producers (dot
\ habu-public-producers-for-7084d81c) and a constructible EVID:bundle (TF-CTOR-NAME-LIMIT);
\ until then the RUNTIME store write stays the V1 report-based path in maki/cad.f
\ (PROMOTE-EVIDENCE) over the now-sealed writers, and this file is the checked promotion seal.

require maki/evidence/policy.f     \ POLICY:granted (pulls maki/evidence/schema.f + maki/typestate.f: ART:built)

package ART
public
TYPEFAMILY promoted 0
private
\ RAW>PROMOTED: the only producer of a `promoted` cell, invoked by PROMOTE after it has
\ consumed a real grant (the maki/typestate.f RAW>* stage-mint pattern). Private, so a raw n
\ cannot forge a promoted artifact and no caller can fabricate one around ART:PROMOTE.
TRUSTED: RAW>PROMOTED ( n -- promoted ) ;
public
: PROMOTE ( ART:built POLICY:granted -- promoted )
   POLICY-GRANTED:UNMAKE  \ ART:built art pol tok
   drop drop drop         \ consume the grant fields (grant is a proof-of-check witness)
   drop                   \ consume the ART:built witness
   0 RAW>PROMOTED ;
;package
