\ CASE defaults may consume values below their selector before the join.
require src/lib/prelude.f
require src/lib/string.f
require src/lib/test.f

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


\ A64IR:NTH's ordinal selector exposed this shape: many returning arms and a
\ throwing default. Test the selections through the real compiler at both tiers.
: LARGE-CASE ( n -- n )
   case
      0  of 7 endof
      1  of 10 endof
      2  of 13 endof
      3  of 16 endof
      4  of 19 endof
      5  of 22 endof
      6  of 25 endof
      7  of 28 endof
      8  of 31 endof
      9  of 34 endof
      10 of 37 endof
      11 of 40 endof
      12 of 43 endof
      13 of 46 endof
      14 of 49 endof
      15 of 52 endof
      16 of 55 endof
      17 of 58 endof
      18 of 61 endof
      19 of 64 endof
      20 of 67 endof
      21 of 70 endof
      22 of 73 endof
      23 of 76 endof
      24 of 79 endof
      25 of 82 endof
      26 of 85 endof
      27 of 88 endof
      28 of 91 endof
      29 of 94 endof
      30 of 97 endof
      31 of 100 endof
      32 of 103 endof
      33 of 106 endof
      34 of 109 endof
      35 of 112 endof
      36 of 115 endof
      37 of 118 endof
      38 of 121 endof
      39 of 124 endof
      40 of 127 endof
      41 of 130 endof
      42 of 133 endof
      43 of 136 endof
      44 of 139 endof
      45 of 142 endof
      46 of 145 endof
      47 of 148 endof
      48 of 151 endof
      49 of 154 endof
      50 of 157 endof
      51 of 160 endof
      52 of 163 endof
      53 of 166 endof
      54 of 169 endof
      55 of 172 endof
      56 of 175 endof
      57 of 178 endof
      58 of 181 endof
      59 of 184 endof
      60 of 187 endof
      61 of 190 endof
      62 of 193 endof
      63 of 196 endof
      64 of 199 endof
      65 of 202 endof
      66 of 205 endof
      67 of 208 endof
      68 of 211 endof
      69 of 214 endof
      70 of 217 endof
      71 of 220 endof
      72 of 223 endof
      73 of 226 endof
      74 of 229 endof
      75 of 232 endof
      76 of 235 endof
      77 of 238 endof
      78 of 241 endof
      79 of 244 endof
      80 of 247 endof
      81 of 250 endof
      82 of 253 endof
      83 of 256 endof
      84 of 259 endof
      -1 throw
   endcase ;


: LARGE-LOW ( -- )
   -1 LARGE-CASE drop ;


: LARGE-HIGH ( -- )
   85 LARGE-CASE drop ;


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


: LARGE-CASES ( -- )
   s" a large CASE selects every arm and preserves the caller's stack" T-LABEL
   777
   85 0 ?do i LARGE-CASE i 3 * 7 + T= loop
   777 T=
   s" a large CASE refuses selectors outside its vocabulary" T-LABEL
   [: LARGE-LOW ;] -1 TTHROWSQ
   [: LARGE-HIGH ;] -1 TTHROWSQ ;


: RUN ( -- )
   T-RESET STACK-CASES RETURN-CASES EXIT-CASES
   LARGE-CASES
   s" an option payload feeds a consuming string CASE" T-LABEL
   s" 1" SELECT? TTRUE
   s" 2" SELECT? TFALSE
   s" 3" SELECT? TFALSE
   s" bad" SELECT? TFALSE
   T-REPORT ;

RUN
;using
;package
