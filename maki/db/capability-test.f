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

CAPTOK:RESET

T-REPORT

;package
