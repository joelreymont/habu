\ Exercise the same CASE source through both real compiler tiers.
require lib/test.f
require lib/test/subject.f
require lib/test/outcome.f

package CASE-LOWERING-TEST

$2000 constant CAP
create OUT CAP allot
create ERR CAP allot

: CHECK-RESULT ( len len outcome n -- ) {: want:n :}
   MATCH outcome
      exited OF want T= ENDOF
      signaled OF drop false TTRUE ENDOF
      timeout OF false TTRUE ENDOF
   ;MATCH
   LEN>N {: erru:n :} LEN>N {: outu:n :}
   want 0= if
      erru 0<> if ERR erru type then
      erru 0 T=
      OUT outu S\" test: ok\n" T$=
   else
      outu 0<> if OUT outu type then
      outu 0 T=
      ERR erru s" at 'endcase'" CONTAINS? TTRUE
   then ;


: RUN-SOURCE ( ptr u8 n n n -- ) {: source:ptr size:n tier:n want:n :}
   SB-RESET
   tier 0= if s" 0 set-tier " else s" 1 set-tier " then SB-APPEND
   source size SB-APPEND
   SB$ OUT CAP >LEN ERR CAP >LEN 20000 >MS SUBJECT:RUN
   want CHECK-RESULT ;


: CHECK-TIER ( n -- ) {: tier:n :}
   s" CASE consuming defaults, return-stack joins and early exits" T-LABEL
   s" require test/compiler/native-case-subject.f" tier 0 RUN-SOURCE
   s" ENDCASE still requires its selector" T-LABEL
   s" : CASE-EMPTY-BAD ( n -- ) case drop endcase ;" tier 70 RUN-SOURCE
   s" CASE still rejects inconsistent data-stack results" T-LABEL
   s" : CASE-JOIN-BAD ( n n -- n ) case 1 of drop 0 endof 2drop 0 endcase ;"
   tier 70 RUN-SOURCE
   s" CASE still rejects inconsistent return-stack rows" T-LABEL
   s" : CASE-RETURN-BAD ( n -- n ) case 1 of 10 >r endof endcase r> ;"
   tier 70 RUN-SOURCE ;


public

: RUN ( -- )
   T-RESET
   0 CHECK-TIER 1 CHECK-TIER
   T-REPORT ;

;package

CASE-LOWERING-TEST:RUN
