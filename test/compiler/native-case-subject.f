\ CASE defaults may consume values below their selector before the join.
require lib/prelude.f
require lib/string.f
require lib/test.f

package CASE-LOWERING
using option

: DROP-PREFIX ( n n -- )
   case 1 of drop endof nip endcase ;


: DEFAULT-ONLY ( n n n -- n )
   case >r + r> endcase ;


: EMPTY-RESULT ( n n n -- )
   case
      1 of 2drop endof
      >r 2drop r>
   endcase ;


: NESTED ( n n n -- n )
   case
      1 of
         case
            2 of 10 + endof
            >r 20 + r>
         endcase
      endof
      3 of drop 30 + endof
      >r drop 40 + r>
   endcase ;


: RETURNED ( n n n n -- n n )
   >r
   case
      1 of + endof
      >r - r>
   endcase
   r> ;


: ARM-RETURN ( n n n -- n n )
   case
      1 of + 101 >r endof
      >r - r> 202 >r
   endcase
   r> ;


: ARM-EXIT ( n n n -- n )
   case
      1 of + exit endof
      2 of * endof
      >r - r>
   endcase ;


: DEFAULT-EXIT ( n n n -- n )
   case
      1 of + endof
      drop - exit
   endcase ;


: ALL-EXIT ( n n n -- n )
   case
      1 of + exit endof
      drop - exit
   endcase ;


\ The reported composition: option payload, an early exit, strings and >r.
: SELECT? ( ptr u8 n -- bool )
   STR>NUMBER? MATCH option
      none OF false exit ENDOF
      some OF ENDOF
   ;MATCH {: ordinal:n :}
   s" value" ordinal case
      1 of s" value" STR= endof
      2 of s" other" STR= endof
      >r 2drop false r>
   endcase ;


: STACK-CASES ( -- )
   s" defaults consume entry values and preserve the caller's stack" T-LABEL
   777 41 1 DROP-PREFIX 777 T=
   778 42 2 DROP-PREFIX 778 T=
   7 3 9 DEFAULT-ONLY 10 T=
   779 4 5 1 EMPTY-RESULT 779 T=
   780 4 5 9 EMPTY-RESULT 780 T=
   s" nested CASE selects each arm and default independently" T-LABEL
   5 2 1 NESTED 15 T=
   5 9 1 NESTED 25 T=
   5 7 3 NESTED 35 T=
   5 7 8 NESTED 45 T= ;


: RETURN-CASES ( -- )
   s" default temporaries preserve a parked return value" T-LABEL
   7 3 1 91 RETURNED 91 T= 10 T=
   7 3 9 92 RETURNED 92 T= 4 T=
   s" joined arms may return their own common return-stack row" T-LABEL
   7 3 1 ARM-RETURN 101 T= 10 T=
   7 3 9 ARM-RETURN 202 T= 4 T= ;


: EXIT-CASES ( -- )
   s" early exits do not contribute a default selector or a join row" T-LABEL
   7 3 1 ARM-EXIT 10 T=
   7 3 2 ARM-EXIT 21 T=
   7 3 9 ARM-EXIT 4 T=
   7 3 1 DEFAULT-EXIT 10 T=
   7 3 9 DEFAULT-EXIT 4 T=
   7 3 1 ALL-EXIT 10 T=
   7 3 9 ALL-EXIT 4 T= ;


: RUN ( -- )
   T-RESET STACK-CASES RETURN-CASES EXIT-CASES
   s" an option payload feeds a consuming string CASE" T-LABEL
   s" 1" SELECT? TTRUE
   s" 2" SELECT? TFALSE
   s" 3" SELECT? TFALSE
   s" bad" SELECT? TFALSE
   T-REPORT ;

RUN
;using
;package
