\ typestate.f - R7 stage typestate skeleton (MODEL-CAD-V2-PLAN.md "R7 Design
\ Addendum", implementation sub-dot 1 v2-typestate-stage-kinds; dot
\ habu-v2-typestate-stage-a0eb43a2).
\
\ Each Model-CAD IR level is a package (MODEL / TIR / RIR / PLAN / KIR / CAND /
\ ART) exposing sealed arity-0 stage nominals. A stage value exists ONLY if a
\ transition minted it: every RAW>* mint is PRIVATE, so a raw n cannot forge a
\ stage and no caller can fabricate a "verified" object. The transition words
\ thread the pipeline in exactly one legal order, so feeding a wrong-stage value
\ to a transition is a signature mismatch the checker rejects BEFORE runtime -
\ replacing the maki/cad.f runtime order guards (NEED-MODEL / NEED-CAPTURE / each
\ command re-running earlier phases) with a type.
\
\ DEVIATION from the R7 addendum's `result<stage,diag-set>` transition
\ signatures: the skeleton transitions cannot fail (no validation yet - that is
\ sub-dots 2-4's evidence/policy work), and a polymorphic `RESULT-DROP` over
\ `result<a,b>` is not expressible (MATCH on a sum with unknown-width payloads
\ needs a runtime family lookup). So the transitions return the next stage
\ DIRECTLY; the error-carrying `result<_,diag-set>` arrives when the transitions
\ gain a real diagnostic path in the evidence/promotion sub-dots. The stage
\ ORDERING - the acceptance of this sub-dot - is fully enforced either way.
\
\ Bodies are minimal skeletons: consume the input stage(s), mint the output.
\ Identity/provenance threading (carrying the artifact id through the stages) is
\ a refinement for the evidence sub-dots; here each mint takes a fresh 0.

require maki/cad-kinds.f

\ ---- MODEL: declared / elaborated -------------------------------------------
package MODEL
public
TYPEFAMILY decl 0
TYPEFAMILY elab 0
private
TRUSTED: RAW>DECL ( n -- decl ) ;
TRUSTED: RAW>ELAB ( n -- elab ) ;
public
\ DECLARE seeds the pipeline: the only public entry to a MODEL:decl.
: DECLARE ( -- decl )  0 RAW>DECL ;
: ELABORATE ( decl -- elab )  drop  0 RAW>ELAB ;
;package

\ ---- TIR: type/shape solved -------------------------------------------------
package TIR
public
TYPEFAMILY solved 0
private
TRUSTED: RAW>SOLVED ( n -- solved ) ;
public
: SOLVE ( MODEL:elab -- solved )  drop  0 RAW>SOLVED ;
;package

\ ---- RIR: region-legalized --------------------------------------------------
package RIR
public
TYPEFAMILY legal 0
private
TRUSTED: RAW>LEGAL ( n -- legal ) ;
public
: LEGALIZE ( TIR:solved -- legal )  drop  0 RAW>LEGAL ;
;package

\ ---- PLAN: draft / complete -------------------------------------------------
package PLAN
public
TYPEFAMILY draft 0
TYPEFAMILY complete 0
private
TRUSTED: RAW>DRAFT ( n -- draft ) ;
TRUSTED: RAW>COMPLETE ( n -- complete ) ;
public
\ DRAFT seeds an incomplete plan; FINISH promotes it against a legal region ir.
: DRAFT ( -- draft )  0 RAW>DRAFT ;
: FINISH ( RIR:legal draft -- complete )  drop drop  0 RAW>COMPLETE ;
;package

\ ---- KIR: drafted / verified ------------------------------------------------
package KIR
public
TYPEFAMILY drafted 0
TYPEFAMILY verified 0
private
TRUSTED: RAW>DRAFTED ( n -- drafted ) ;
TRUSTED: RAW>VERIFIED ( n -- verified ) ;
public
: DRAFT ( -- drafted )  0 RAW>DRAFTED ;
: VERIFY ( PLAN:complete drafted -- verified )  drop drop  0 RAW>VERIFIED ;
;package

\ ---- CAND: emitted (needs a verified KIR + a target) ------------------------
package CAND
public
TYPEFAMILY emitted 0
private
TRUSTED: RAW>EMITTED ( n -- emitted ) ;
public
: EMIT ( KIR:verified CAD-KIND:target-id -- emitted )  drop drop  0 RAW>EMITTED ;
;package

\ ---- ART: built (needs an emitted candidate + a toolchain) ------------------
package ART
public
TYPEFAMILY built 0
private
TRUSTED: RAW>BUILT ( n -- built ) ;
public
: BUILD ( CAND:emitted CAD-KIND:toolchain-id -- built )  drop drop  0 RAW>BUILT ;
;package
