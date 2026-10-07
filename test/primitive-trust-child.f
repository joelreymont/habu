\ The native-build handoff loads the current checker. Replaying the reset
\ declaration supplies the same user effect a native owner transfer publishes;
\ a JIT-hosted window need not have recorded that pre-hook declaration itself.
\ The window loads lib/tier.f ahead of this file (test/primitive-trust.f ARGS).
package PRIMITIVE-TRUST-TEST

: ASSERT ( bool -- )
   if exit then s" primitive trust assertion failed" 76 die ;
: =ASSERT ( n n -- ) = ASSERT ;

TRUSTED: RESET-DECLARATION ( -- )
   s" CHECKER-RESET-SOURCE" s" --" TRUST-DECL ;

\ Inspect only the user row; the public effect query also answers
\ primitive axioms and could not prove that the bypass's precondition exists.
: USER-ROW ( -- n )
   s" CHECKER-RESET-SOURCE" CHECKER-GLOBAL-SYM? USIG-NEWEST
   dup 0= if s" reset has no user effect" 76 die then
   dup 1- E-PTR ER.ACTIVE @ 0= if s" reset user effect is inactive" 76 die then ;

\ The cases compile their declarations through evaluate-closed; TIER:SELECT
\ chooses the compiler used for them. A closed text leaves nothing, so one that
\ computes a value stores it in RESULT, qualified: the texts run inside the
\ package each tier opens, where this package's private words are out of scope.
public
variable RESULT
private

variable ROW0

: REFUSE-CANDIDATE ( -- )
   s" PF-BAD-RESET ( -- ) CHECKER-RESET-SOURCE" CHECK-CANDIDATE! 0 =ASSERT ;

: RUN-TIER ( n -- ) {: tier:n :}
   tier TIER:SELECT
   tier 0= if s" package PTRUST-JIT" else s" package PTRUST-NATIVE" then
   evaluate-closed
   REFUSE-CANDIDATE
   \ This authorized caller still compiles from the retained user graph.
   \ Running reset itself would discard the checker under the remaining cases.
   s" TRUSTED: RESET-CALL ( -- ) CHECKER-RESET-SOURCE ;" evaluate-closed
   s" ' RESET-CALL dup 4 + code-origin PRIMITIVE-TRUST-TEST:RESULT !"
      evaluate-closed
   RESULT @ tier =ASSERT
   \ So does a TRUSTED: caller of a trusted-only seed primitive: tier 1 builds
   \ its call window from the primitive's global row.
   s" TRUSTED: CHECK-SELECT ( n -- ) set-check ; check@ CHECK-SELECT"
      evaluate-closed
   s" ' CHECK-SELECT dup 4 + code-origin PRIMITIVE-TRUST-TEST:RESULT !"
      evaluate-closed
   RESULT @ tier =ASSERT
   s" : LOCAL-CALL ( n -- n ) {: set-check:n :} set-check ; 41 LOCAL-CALL PRIMITIVE-TRUST-TEST:RESULT !"
      evaluate-closed
   RESULT @ 41 =ASSERT
   \ A package word with the same spelling has its own symbol and effect.
   s" : CHECKER-RESET-SOURCE ( -- n ) 42 ;" evaluate-closed
   s" PF-SHADOW ( -- n ) CHECKER-RESET-SOURCE" CHECK-CANDIDATE! -1 =ASSERT
   s" : SHADOW-CALL ( -- n ) CHECKER-RESET-SOURCE ; SHADOW-CALL PRIMITIVE-TRUST-TEST:RESULT !"
      evaluate-closed
   RESULT @ 42 =ASSERT
   s" ;package" evaluate-closed ;

public
: RUN ( -- )
   RESET-DECLARATION
   USER-ROW ROW0 !
   s" PF-GOOD ( -- n ) 42" CHECK-CANDIDATE! -1 =ASSERT
   REFUSE-CANDIDATE
   0 RUN-TIER
   1 RUN-TIER
   REFUSE-CANDIDATE
   USER-ROW ROW0 @ =ASSERT
   s" primitive trust: ok" type cr ;

;package
PRIMITIVE-TRUST-TEST:RUN
