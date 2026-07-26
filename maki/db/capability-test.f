\ maki/db/capability-test.f - checked acceptance for the finite capability GRANT token
\ (maki/db/capability.f, dot habu-v2-capability-and-0970a96d). Proves the dot's acceptance,
\ each item by a named test:
\   CT-RAW-BAD / CT-GRANT-OK : STATIC unforgeability - a raw n CANNOT stand where a grant is
\                        required (verdict 0), a real grant can (verdict -1)
\   CT-MINT-SEALED     : the private RAW>GRANT mint is unresolvable outside its owner (verdict 1)
\   CT-EQ-BAD          : `=` on a grant value rejects (no scalar laundering; verdict 0)
\   CT-ROOT-CAP/-BUD   : ROOT mints the declared authority (capability mask + budget ceilings)
\   CT-ATT-OK          : ATTENUATE of a subset request -> ok(child); the child carries the subset
\   CT-ATT-ESCAPE-CAP  : a capability bit the parent lacks -> escape-cap (nested exceed rejects)
\   CT-ATT-ESCAPE-BUD  : a budget ceiling above the parent's -> escape-budget naming the dimension
\   CT-NESTED-*        : a grandchild cannot exceed its parent (transitive subset), both ways
\   CT-AUTH / CT-COVERS: the AUTHORIZES? / COVERS? gates (the ACTION:DISPATCH capability precedent)
\   CT-OVERFLOW        : the grant pool fails closed at capacity
\
\ The test reopens package CAPTOK (a friend) so the builder + MATCH read bare; the static
\ fixtures use the shared checker-assert verdict helper.

require lib/prelude.f
require lib/test.f
require test/checker-assert.f
require maki/db/capability.f
require maki/db/budget-dim.f

\ ---- declaration-shape reflection ----------------------------------------------
\ attenuate-result is declared through the unified ENUM front end in full mode, so
\ each carrying arm publishes a named FIELD as a type-registry row keyed
\ (family, variant). The readers live in REFLECT (test/checker-assert.f); this
\ package holds only the identity this suite pins - the family tail plus the
\ constructor package its variants carry, the (package, tail) pair that owns family
\ identity, which also keeps the dimension pins off the unrelated arity-0 `dim` cell
\ family that shares a tail with BUDGET:dim.
package CAPTOK-PINS
public

: AR$ ( -- ptr u8 n ptr u8 n )   s" attenuate-result" s" CAPTOK-ATTENUATE--RESULT" ;

;package

package CAPTOK

\ ---- checker verdict wrappers (the maki/db/action-test precedent) ---------------
: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO ( ptr u8 n -- )    CHECK-QUIET-CANDIDATE! 0 T= ;
: UNRES ( ptr u8 n -- ) CHECK-QUIET-CANDIDATE! 1 T= ;

\ ---- typed attenuate-result decoders -------------------------------------------
: AR-CODE ( attenuate-result<CAPTOK:grant> -- n )   \ 0 ok / 1 escape-cap / 2 escape-budget
   MATCH attenuate-result
      ok            OF drop 0 ENDOF
      escape-cap    OF 1 ENDOF
      escape-budget OF drop 2 ENDOF
   ;MATCH ;

: AR-ESCAPE-DIM ( attenuate-result<CAPTOK:grant> -- n )   \ escaped dimension ordinal, else -1
   MATCH attenuate-result
      ok            OF drop -1 ENDOF
      escape-cap    OF -1 ENDOF
      escape-budget OF BUDGET:DIM>N ENDOF
   ;MATCH ;

: AR-CHILD-CAP ( attenuate-result<CAPTOK:grant> -- n bool )   \ child cap mask + ok
   MATCH attenuate-result
      ok            OF CAP-MASK@ true ENDOF
      escape-cap    OF 0 false ENDOF
      escape-budget OF drop 0 false ENDOF
   ;MATCH ;

\ ---- named-payload round-trip through the production producer -------------------
\ `retries` is ordinal 4: deliberately NOT compute-time, whose ordinal is 0 and
\ would make a dropped or zeroed payload read back as a legitimate dimension.
: CT-AR-DIM ( -- BUDGET:dim )        BUDGET-DIM:RETRIES ;
: CT-AR-DIM-NONZERO ( -- bool )      CT-AR-DIM BUDGET:DIM>N 0<> ;
: CT-AR-MK-BUD ( BUDGET:dim -- attenuate-result<CAPTOK:grant> )   AR-ESCAPE-BUDGET ;
: CT-AR-RT-BUD ( -- n )              CT-AR-DIM CT-AR-MK-BUD AR-CODE ;
: CT-AR-RT-DIM ( -- n )              CT-AR-DIM CT-AR-MK-BUD AR-ESCAPE-DIM ;
: CT-AR-CAP-DIM ( -- n )             AR-ESCAPE-CAP AR-ESCAPE-DIM ;   \ payloadless arm

: AR-CHILD-BUD-COMPUTE ( attenuate-result<CAPTOK:grant> -- n bool )   \ child compute-time ceiling + ok
   MATCH attenuate-result
      ok            OF BUDGET-DIM:COMPUTE-TIME BUDGET@ true ENDOF
      escape-cap    OF 0 false ENDOF
      escape-budget OF drop 0 false ENDOF
   ;MATCH ;

\ ---- fixtures ------------------------------------------------------------------
\ A root grant: capability bits {0,1,2} (mask 7), compute-time ceiling 100, device-time 50.
: MK-ROOT ( -- CAPTOK:grant )
   NEW  7 CAP!
   BUDGET-DIM:COMPUTE-TIME 100 BUDGET!
   BUDGET-DIM:DEVICE-TIME  50  BUDGET!
   ROOT ;

: ATT ( CAPTOK:grant n BUDGET:dim n -- attenuate-result<CAPTOK:grant> )   \ parent then request (cap; dim=amt)
   {: parent:CAPTOK:grant cap:n dim:BUDGET:dim amt:n :}
   NEW  cap CAP!  dim amt BUDGET!  parent ATTENUATE ;

\ ---- dynamic acceptance --------------------------------------------------------
: CT-ROOT-CAP ( -- n )   RESET MK-ROOT CAP-MASK@ ;
: CT-ROOT-BUD ( -- n )   RESET MK-ROOT BUDGET-DIM:COMPUTE-TIME BUDGET@ ;
: CT-ROOT-BUD2 ( -- n )  RESET MK-ROOT BUDGET-DIM:DEVICE-TIME BUDGET@ ;

\ subset request (cap 3 <= 7; compute 40 <= 100) -> ok; child carries the subset
: CT-ATT-OK ( -- n )        RESET MK-ROOT 3 BUDGET-DIM:COMPUTE-TIME 40 ATT AR-CODE ;
: CT-ATT-CHILD-CAP ( -- n bool )
   RESET MK-ROOT 3 BUDGET-DIM:COMPUTE-TIME 40 ATT AR-CHILD-CAP ;
: CT-ATT-CHILD-BUD ( -- n bool )
   RESET MK-ROOT 3 BUDGET-DIM:COMPUTE-TIME 40 ATT AR-CHILD-BUD-COMPUTE ;

\ escape: a capability bit (8) the parent (mask 7) lacks -> escape-cap
: CT-ATT-ESCAPE-CAP ( -- n )   RESET MK-ROOT 8 BUDGET-DIM:COMPUTE-TIME 40 ATT AR-CODE ;
\ escape: a compute-time ceiling (200) above the parent's (100) -> escape-budget(compute-time=0)
: CT-ATT-ESCAPE-BUD ( -- n )   RESET MK-ROOT 3 BUDGET-DIM:COMPUTE-TIME 200 ATT AR-CODE ;
: CT-ATT-ESCAPE-DIM ( -- n )   RESET MK-ROOT 3 BUDGET-DIM:COMPUTE-TIME 200 ATT AR-ESCAPE-DIM ;

\ nested: a grandchild cannot exceed its (already attenuated) parent - transitive subset.
: MK-CHILD ( -- CAPTOK:grant )   \ child: cap 3, compute-time 40 (a strict subset of MK-ROOT)
   MK-ROOT 3 BUDGET-DIM:COMPUTE-TIME 40 ATT
   MATCH attenuate-result
      ok            OF ENDOF
      escape-cap    OF MK-ROOT ENDOF
      escape-budget OF drop MK-ROOT ENDOF
   ;MATCH ;
\ grandchild wanting compute 50 > child's 40 -> escape-budget (nested exceed rejects)
: CT-NESTED-EXCEED ( -- n )   RESET MK-CHILD 1 BUDGET-DIM:COMPUTE-TIME 50 ATT AR-CODE ;
\ grandchild within the child -> ok (nested subset accepts)
: CT-NESTED-OK ( -- n )       RESET MK-CHILD 1 BUDGET-DIM:COMPUTE-TIME 20 ATT AR-CODE ;

\ AUTHORIZES? / COVERS? gates
: CT-AUTH-YES ( -- bool )   RESET MK-ROOT 3 AUTHORIZES? ;
: CT-AUTH-NO ( -- bool )    RESET MK-ROOT 8 AUTHORIZES? ;
: CT-COVERS-YES ( -- bool ) RESET MK-ROOT BUDGET-DIM:COMPUTE-TIME 100 COVERS? ;
: CT-COVERS-NO ( -- bool )  RESET MK-ROOT BUDGET-DIM:COMPUTE-TIME 101 COVERS? ;

\ capacity: the grant pool fails closed (E-CAPTOK-CAP) past GRANT-CAP.
: CT-OVERFLOW ( -- )   RESET  65 0 ?do NEW 0 CAP! ROOT drop loop ;

T-RESET

\ ---- STATIC unforgeability (raw values cannot forge; the cad-kinds verdict pattern) ----
\ a raw n cannot stand where a grant is required; a real grant can.
s" CT-RAW-BAD ( n -- n ) CAPTOK:CAP-MASK@" NO
s" CT-GRANT-OK ( CAPTOK:grant -- n ) CAPTOK:CAP-MASK@" YES
\ the private representation mint is unresolvable outside its owning file (sealed).
s" CT-MINT-SEALED ( n -- CAPTOK:grant ) CAPTOK:RAW>GRANT" UNRES
\ `=` on a grant rejects (an ADT value never laundry-compares through the scalar prim).
s" CT-EQ-BAD ( CAPTOK:grant CAPTOK:grant -- bool ) =" NO

\ ---- ROOT mints the declared authority -----------------------------------------
CT-ROOT-CAP 7 T=
CT-ROOT-BUD 100 T=
CT-ROOT-BUD2 50 T=

\ ---- ATTENUATE subset accept: ok + the child carries the subset ----------------
CT-ATT-OK 0 T=
CT-ATT-CHILD-CAP TTRUE 3 T=
CT-ATT-CHILD-BUD TTRUE 40 T=

\ ---- ATTENUATE escape rejects (nested exceed rejects), both axes ---------------
CT-ATT-ESCAPE-CAP 1 T=
CT-ATT-ESCAPE-BUD 2 T=
CT-ATT-ESCAPE-DIM 0 T=

\ ---- nested (transitive) subset: grandchild cannot exceed its parent -----------
CT-NESTED-EXCEED 2 T=
CT-NESTED-OK 0 T=

\ ---- AUTHORIZES? / COVERS? -----------------------------------------------------
CT-AUTH-YES TTRUE
CT-AUTH-NO TFALSE
CT-COVERS-YES TTRUE
CT-COVERS-NO TFALSE

\ ---- capacity fail-closed ------------------------------------------------------
' CT-OVERFLOW E-CAPTOK-CAP TTHROWS

\ ==== attenuate-result as a full-mode payload ENUM ==============================
\ The generated constructors, by exact spelling and exact effect. The family stays
\ arity 1, so the ok arm's payload is the type PARAMETER and every pin below names
\ the instantiation explicitly.
s" AR-P-OK ( CAPTOK:grant -- CAPTOK:attenuate-result<CAPTOK:grant> ) CAPTOK-ATTENUATE--RESULT:OK" YES
s" AR-P-CAP ( -- CAPTOK:attenuate-result<CAPTOK:grant> ) CAPTOK-ATTENUATE--RESULT:ESCAPE-CAP" YES
s" AR-P-BUD ( BUDGET:dim -- CAPTOK:attenuate-result<CAPTOK:grant> ) CAPTOK-ATTENUATE--RESULT:ESCAPE-BUDGET" YES
\ each arm's payload is mandatory, typed, and its own: a raw cell cannot forge the
\ child grant, the two carrying arms cannot trade payload types even though both are
\ single cells, a payloadless ok is not constructible, and the result is not a bare
\ scalar. The parameter is real: an ok built from one instantiation is not another.
s" AR-F-RAW ( n -- CAPTOK:attenuate-result<CAPTOK:grant> ) CAPTOK-ATTENUATE--RESULT:OK" NO
s" AR-F-DIM-AS-CHILD ( BUDGET:dim -- CAPTOK:attenuate-result<CAPTOK:grant> ) CAPTOK-ATTENUATE--RESULT:OK" NO
s" AR-F-CHILD-AS-DIM ( CAPTOK:grant -- CAPTOK:attenuate-result<CAPTOK:grant> ) CAPTOK-ATTENUATE--RESULT:ESCAPE-BUDGET" NO
s" AR-F-NOPAY ( -- CAPTOK:attenuate-result<CAPTOK:grant> ) CAPTOK-ATTENUATE--RESULT:OK" NO
s" AR-F-CAPPAY ( CAPTOK:grant -- CAPTOK:attenuate-result<CAPTOK:grant> ) CAPTOK-ATTENUATE--RESULT:ESCAPE-CAP" NO
s" AR-F-BARE ( CAPTOK:grant -- n ) CAPTOK-ATTENUATE--RESULT:OK" NO
s" AR-F-INST ( CAPTOK:grant -- CAPTOK:attenuate-result<BUDGET:dim> ) CAPTOK-ATTENUATE--RESULT:OK" NO
\ MATCH arm bindings are per-arm and typed: the ok arm binds the child grant, the
\ escape-budget arm binds a dimension, and exchanging the two bindings rejects.
s" AR-M-OK ( CAPTOK:attenuate-result<CAPTOK:grant> -- n ) MATCH CAPTOK:attenuate-result ok OF {: c:CAPTOK:grant :} 0 ENDOF escape-cap OF 1 ENDOF escape-budget OF {: d:BUDGET:dim :} 2 ENDOF ;MATCH" YES
s" AR-M-SWAP ( CAPTOK:attenuate-result<CAPTOK:grant> -- n ) MATCH CAPTOK:attenuate-result ok OF {: d:BUDGET:dim :} 0 ENDOF escape-cap OF 1 ENDOF escape-budget OF {: c:CAPTOK:grant :} 2 ENDOF ;MATCH" NO

\ the three arms keep their names and order, the family stays arity 1, the ok and
\ escape-budget arms each carry exactly one named cell at payload slot 0 - `child`
\ and `dim` - and the payloadless escape-cap arm carries none.
CAPTOK-PINS:AR$ REFLECT:FAMS 1 T=
CAPTOK-PINS:AR$ REFLECT:VARS 3 T=
CAPTOK-PINS:AR$ REFLECT:ARITY 1 T=          \ still parametric over the grant type
CAPTOK-PINS:AR$ REFLECT:WIDTH 2 T=          \ one payload cell plus one tag cell
CAPTOK-PINS:AR$ 0 REFLECT:ARM$ s" ok" T$=
CAPTOK-PINS:AR$ 1 REFLECT:ARM$ s" escape-cap" T$=
CAPTOK-PINS:AR$ 2 REFLECT:ARM$ s" escape-budget" T$=
CAPTOK-PINS:AR$ 0 REFLECT:ARM-FLDS 1 T=
CAPTOK-PINS:AR$ 1 REFLECT:ARM-FLDS 0 T=
CAPTOK-PINS:AR$ 2 REFLECT:ARM-FLDS 1 T=
CAPTOK-PINS:AR$ 0 s" child" REFLECT:ARM-SLOT 0 T=
CAPTOK-PINS:AR$ 2 s" dim" REFLECT:ARM-SLOT 0 T=
\ the two payload names are per-arm, so neither answers on the other's arm.
CAPTOK-PINS:AR$ 0 s" dim" REFLECT:ARM-SLOT -1 T=
CAPTOK-PINS:AR$ 2 s" child" REFLECT:ARM-SLOT -1 T=
CAPTOK-PINS:AR$ 1 s" child" REFLECT:ARM-SLOT -1 T=

\ constructed directly through the production producers and matched straight back.
\ The escaping dimension under test is `retries` (ordinal 4) rather than
\ `compute-time`, whose ordinal is 0: a dropped or zeroed dimension payload would
\ read back as 0 and pass.
CT-AR-DIM-NONZERO TTRUE                          \ the dimension under test is not ordinal 0
CT-AR-RT-BUD 2 T=                                \ escape-budget dispatches to its own arm
CT-AR-RT-DIM 4 T=                                \ and carries `retries`, not a zeroed ordinal
CT-AR-CAP-DIM -1 T=                              \ the no-payload arm of AR-ESCAPE-DIM is live

public

\ att-twin is attenuate-result's SHAPE under a different name - same arity 1, same
\ three arms in the same order, same named payload cells. Identity is nominal even
\ for a PARAMETRIC family: the twin does not unify with attenuate-result at the same
\ instantiation, in either direction. Public because a private family publishes no
\ constructors, so the positive control builds through the twin's own constructor.
ENUM att-twin 1
   VARIANT att-twin-ok FIELD child a ;VARIANT
   VARIANT att-twin-cap ;VARIANT
   VARIANT att-twin-bud FIELD dim BUDGET:dim ;VARIANT
;ENUM

private

s" AR-TW ( CAPTOK:grant -- att-twin<CAPTOK:grant> ) CAPTOK-ATT--TWIN:ATT-TWIN-OK" YES
s" AR-TW-X1 ( CAPTOK:grant -- att-twin<CAPTOK:grant> ) CAPTOK-ATTENUATE--RESULT:OK" NO
s" AR-TW-X2 ( CAPTOK:grant -- CAPTOK:attenuate-result<CAPTOK:grant> ) CAPTOK-ATT--TWIN:ATT-TWIN-OK" NO

CAPTOK:RESET

T-REPORT

;package
