\ The native-build handoff loads the current checker. Replaying the reset
\ declaration supplies the same user effect a native owner transfer publishes;
\ a JIT-hosted window need not have recorded that pre-hook declaration itself.
package PRIMITIVE-TRUST-TEST

: ASSERT ( bool -- )
   if exit then s" primitive trust assertion failed" 76 die ;
: =ASSERT ( n n -- ) = ASSERT ;

TRUSTED: RESET-DECLARATION ( -- )
   s" CHECKER-RESET-SOURCE" s" --" TRUST-DECL ;

\ Inspect only the user row; the public effect query also answers
\ primitive axioms and could not prove that the bypass's precondition exists.
TRUSTED: USER-ROW ( -- n )
   s" CHECKER-RESET-SOURCE" CHECKER-GLOBAL-SYM? USIG-NEWEST
   dup 0= if s" reset has no user effect" 76 die then
   dup 1- E-PTR ER.ACTIVE @ 0= if s" reset user effect is inactive" 76 die then ;

\ These test boundaries compile the supplied declarations through the real
\ evaluator; SELECT chooses the compiler used for those declarations.
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;
TRUSTED: SELECT ( n -- ) set-tier ;

variable ROW0

: REFUSE-CANDIDATE ( -- )
   s" PF-BAD-RESET ( -- ) CHECKER-RESET-SOURCE" CHECK-CANDIDATE! 0 =ASSERT ;

: RUN-TIER ( n -- ) {: tier:n :}
   tier SELECT
   tier 0= if s" package PTRUST-JIT" else s" package PTRUST-NATIVE" then EV
   REFUSE-CANDIDATE
   \ This authorized caller still compiles from the retained user graph.
   \ Running reset itself would discard the checker under the remaining cases.
   s" TRUSTED: RESET-CALL ( -- ) CHECKER-RESET-SOURCE ;" EV
   s" ' RESET-CALL dup 4 + code-origin" EV-N tier =ASSERT
   s" TRUSTED: TIER-SELECT ( n -- ) set-tier ; tier@ TIER-SELECT" EV
   tier@ tier =ASSERT
   s" : LOCAL-CALL ( n -- n ) {: set-tier:n :} set-tier ; 41 LOCAL-CALL" EV-N 41 =ASSERT
   \ A package word with the same spelling has its own symbol and effect.
   s" : CHECKER-RESET-SOURCE ( -- n ) 42 ;" EV
   s" PF-SHADOW ( -- n ) CHECKER-RESET-SOURCE" CHECK-CANDIDATE! -1 =ASSERT
   s" : SHADOW-CALL ( -- n ) CHECKER-RESET-SOURCE ; SHADOW-CALL" EV-N 42 =ASSERT
   s" ;package" EV ;

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
