\ Source grants and native ABI facts share a row without sharing authority.
\ Run directly on the product, or as the native-window-owner child fixture.
require lib/test.f

package EFFECT-AUTHORITY-TEST

\ These are inspection and declaration boundaries over the actual source owner.
TRUSTED: JUDGE ( ptr u8 n -- n ) CHECK! ;
TRUSTED: ABI ( ptr u8 n -- n ) CHECK-UNJUDGED! ;
TRUSTED: ABI-MIN ( ptr u8 n -- n ) SIG-MIN-IN ;
TRUSTED: SOURCE-MIN ( ptr u8 n -- n ) EFFECT-EXTERNAL-MIN-IN ;
TRUSTED: MIN-LATCH ( -- n ) REC-MIN-IN@ ;
TRUSTED: ENFORCED? ( -- bool ) CHECKER-EFFECT-AUTHORITY:ENFORCED? ;
TRUSTED: ABI-SCOPE ( [ -- ] -- n ) CHECKER-EFFECT-AUTHORITY:SCAN ;
TRUSTED: DECLARE ( ptr u8 n ptr u8 n -- ) CHECKER-USIG-ADD ;
TRUSTED: ABI-ROW ( ptr u8 n ptr u8 n -- )
   CHECKER-RECORD-NAME RES-FALSE USIG-ADD-AS ;
TRUSTED: SCOPE+ ( -- ) CHECKER-SCOPE-START ;
TRUSTED: SCOPE- ( -- ) CHECKER-SCOPE-DONE ;
TRUSTED: MULTI+ ( -- ) MULTI-ERR-BEGIN ;
TRUSTED: MULTI- ( -- n ) MULTI-ERR-END ;
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;
TRUSTED: SELECT ( n -- ) set-tier ;

: EXPLICIT-IN-ABI ( -- )
   s" n -- n" s" EAUTH-EXPLICIT" DECLARE ;

: THROW-IN-ABI ( -- ) 7329 throw ;

: ABI-DUPLICATE ( -- )
   s" EAUTH-JUDGED ( n -- n )" ABI drop ;

: SCAN-CASES ( -- )
   s" a judged effect grants its own minimum" T-LABEL
   s" EAUTH-JUDGED ( n -- n )" JUDGE -1 T=
   MIN-LATCH 1 T=
   s" EAUTH-JUDGED" SOURCE-MIN 1 T=
   s" EAUTH-JUDGED" CHECKER-RESOLVES? TTRUE
   s" a successful native scan records shape without authority" T-LABEL
   s" EAUTH-ABI ( n -- n )" ABI -1 T=
   MIN-LATCH 0 T=
   s" EAUTH-ABI" ABI-MIN 1 T=
   s" EAUTH-ABI" SOURCE-MIN -1 T=
   s" EAUTH-ABI" CHECKER-RESOLVES? TFALSE
   s" EAUTH-ABI-CALL ( n -- n ) EAUTH-ABI" CHECK-CANDIDATE! 0 T=
   s" EAUTH-ABI-USES ( n -- n ) EAUTH-ABI" ABI -1 T=
   s" EAUTH-ABI-USES" SOURCE-MIN -1 T=
   s" explicit declarations grant authority inside the ABI scope" T-LABEL
   [: EXPLICIT-IN-ABI ;] ABI-SCOPE 0 T=
   ENFORCED? TTRUE
   s" EAUTH-EXPLICIT" SOURCE-MIN 1 T=
   s" EAUTH-EXPLICIT" CHECKER-RESOLVES? TTRUE
   s" EAUTH-EXPLICIT-CALL ( n -- n ) EAUTH-EXPLICIT" CHECK-CANDIDATE! -1 T=
   s" an ABI-scope throw restores enforcement" T-LABEL
   [: THROW-IN-ABI ;] ABI-SCOPE 7329 T=
   ENFORCED? TTRUE
   SCOPE+
   [: ABI-DUPLICATE ;] catch 78 T=
   ENFORCED? TTRUE
   SCOPE-
   s" EAUTH-JUDGED" SOURCE-MIN 1 T=
   s" a failed enforced check leaves no granted row" T-LABEL
   s" EAUTH-BAD ( n -- n ) drop" JUDGE 0 T=
   s" EAUTH-BAD" SOURCE-MIN -1 T=
   MULTI+
   s" EAUTH-MULTI-BAD ( n -- n ) drop" JUDGE 0 T=
   MULTI- 1 T=
   s" EAUTH-MULTI-BAD" ABI-MIN 1 T=
   s" EAUTH-MULTI-BAD" SOURCE-MIN -1 T= ;

: PRIM-CASES ( -- )
   s" PRIM authority uses the PRIM effect, not the unjudged user's shape" T-LABEL
   SCOPE+
   s" -- n" s" dup" ABI-ROW
   s" dup" ABI-MIN 0 T=
   s" dup" SOURCE-MIN 1 T=
   s" EAUTH-PRIM-GOOD ( n -- n n ) dup" CHECK-CANDIDATE! -1 T=
   s" EAUTH-PRIM-BAD ( -- n ) dup" CHECK-CANDIDATE! 0 T=
   s" trusted-only PRIM policy still wins over a user row" T-LABEL
   s" n --" s" set-tier" DECLARE
   s" EAUTH-PRIM-TRUST ( -- ) 0 set-tier" CHECK-CANDIDATE! 0 T=
   SCOPE- ;

: ROLLBACK-CASES ( -- )
   s" rollback restores the grant of the previous binding" T-LABEL
   s" EAUTH-ROLL ( n -- n )" JUDGE -1 T=
   SCOPE+
   s" n n -- n" s" EAUTH-ROLL" ABI-ROW
   s" EAUTH-ROLL" ABI-MIN 2 T=
   s" EAUTH-ROLL" SOURCE-MIN -1 T=
   SCOPE-
   s" EAUTH-ROLL" SOURCE-MIN 1 T=
   s" regrowth cannot retain a retired grant" T-LABEL
   SCOPE+
   s" -- n" s" EAUTH-REUSED" DECLARE
   s" EAUTH-REUSED" SOURCE-MIN 0 T=
   SCOPE-
   s" n -- n" s" EAUTH-REUSED" ABI-ROW
   s" EAUTH-REUSED" SOURCE-MIN -1 T=
   s" n -- n" s" EAUTH-REUSED" DECLARE
   s" EAUTH-REUSED" SOURCE-MIN 1 T= ;

\ The evaluator owns the real publication and redefinition rollback below.
\ The saved check hook is restored even when a definition throws.
variable SAVED-HOOK
TRUSTED: UNCHECKED+ ( -- ) check@ SAVED-HOOK ! 0 set-check ;
TRUSTED: UNCHECKED- ( -- ) SAVED-HOOK @ set-check ;

: PREHOOK-DECLARATIONS ( -- )
   s" : EAUTH-RAW ( n -- n ) ;" EV
   s" TRUSTED: EAUTH-TRUSTED ( n -- n ) ;" EV
   s" defer EAUTH-DEFER ( n -- n )" EV ;

: BAD-REDEFINITION ( -- )
   s" : EAUTH-LIVE ( n -- n ) drop ;" EV ;

: LIVE-CASES ( -- )
   s" real pre-hook native publication preserves explicit declarations" T-LABEL
   1 SELECT
   UNCHECKED+
   [: PREHOOK-DECLARATIONS ;] catch
   UNCHECKED- 0 T=
   s" EAUTH-RAW" ABI-MIN 1 T=
   s" EAUTH-RAW" SOURCE-MIN -1 T=
   s" EAUTH-TRUSTED" SOURCE-MIN 1 T=
   s" EAUTH-DEFER" SOURCE-MIN 1 T=
   s" package EAUTH-SHADOW private : EAUTH-RAW ( n -- n ) ; public EXPORT EAUTH-RAW ;package" EV
   s" EAUTH-SHADOW:EAUTH-RAW" SOURCE-MIN 1 T=
   s" EAUTH-RAW" SOURCE-MIN -1 T=
   s" package EAUTH-ALIAS public EXPORT EAUTH-RAW ;package" EV
   s" EAUTH-ALIAS:EAUTH-RAW" ABI-MIN 1 T=
   s" EAUTH-ALIAS:EAUTH-RAW" SOURCE-MIN -1 T=
   s" EAUTH-ALIAS-CALL ( n -- n ) EAUTH-ALIAS:EAUTH-RAW" CHECK-CANDIDATE! 0 T=
   s" : EAUTH-LIVE ( n -- n ) ;" EV
   s" 17 EAUTH-LIVE" EV-N 17 T=
   s" undefine EAUTH-LIVE" EV
   [: BAD-REDEFINITION ;] catch 70 T=
   s" EAUTH-LIVE" SOURCE-MIN -1 T=
   s" : EAUTH-LIVE ( n -- n ) ;" EV
   s" EAUTH-LIVE" SOURCE-MIN 1 T=
   s" 23 EAUTH-LIVE" EV-N 23 T= ;

: RUN ( -- )
   T-RESET SCOPE+
   SCAN-CASES PRIM-CASES ROLLBACK-CASES
   SCOPE-
   LIVE-CASES
   T-REPORT
   s" effect authority: ok" type cr ;

: ACTION ( -- [ -- ] ) [: RUN ;] ;
ACTION
;package
execute
