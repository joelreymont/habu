\ Tokens retain spans in the arena-owned, codepoint-decoded query.
\ String and local-name escapes are decoded later, when reading terms.

require src/lib/sparql/syntax.f
require src/lib/utf8-scalar.f

package SPARQL
private

1 constant T-WORD
2 constant T-IRI
3 constant T-STRING
4 constant T-VAR
5 constant T-NAME
6 constant T-BNODE
7 constant T-INTEGER
8 constant T-DECIMAL
9 constant T-DOUBLE
10 constant T-LANG
11 constant T-PUNCT

: AT-BYTE ( ptr syntax n -- n )
   {: state offset:n :}
   state CHECK-BUDGET
   offset 0 < if -1 exit then
   offset state SYNTAX-LENGTH @ >= if -1 exit then
   state SYNTAX-SOURCE @ offset + c@ ;

: PEEK ( ptr syntax -- n )
   dup SYNTAX-POSITION @ AT-BYTE ;

: STEP ( ptr syntax -- )
   dup CHECK-BUDGET 1 swap SYNTAX-POSITION +! ;

: BETWEEN? ( n n n -- bool )
   {: value:n low:n high:n :}
   value low >= value high <= and ;

: DIGIT? ( n -- bool ) 48 57 BETWEEN? ;
: LETTER? ( n -- bool )
   dup 65 90 BETWEEN? swap 97 122 BETWEEN? or ;

: WHITE? ( n -- bool )
   dup 32 = over 9 = or over 10 = or swap 13 = or ;

: HEX? ( n -- bool )
   dup DIGIT? over 65 70 BETWEEN? or
   swap 97 102 BETWEEN? or ;

: HEX-VALUE ( n -- n )
   dup HEX? 0= if E-SYNTAX throw then
   dup DIGIT? if 48 - else ASCII-LOWER 87 - then ;

: PN-BASE? ( n -- bool )
   {: value:n :}
   value LETTER? if true exit then
   value $C0 $D6 BETWEEN? value $D8 $F6 BETWEEN? or
   value $F8 $2FF BETWEEN? or
   value $370 $37D BETWEEN? or
   value $37F $1FFF BETWEEN? or
   value $200C $200D BETWEEN? or
   value $2070 $218F BETWEEN? or
   value $2C00 $2FEF BETWEEN? or
   value $3001 $D7FF BETWEEN? or
   value $F900 $FDCF BETWEEN? or
   value $FDF0 $FFFD BETWEEN? or
   value $10000 $EFFFF BETWEEN? or ;

: PN-U? ( n -- bool ) dup 95 = swap PN-BASE? or ;

: VAR-CHAR? ( n -- bool )
   {: value:n :}
   value PN-U? value DIGIT? or value $B7 = or
   value $300 $36F BETWEEN? or
   value $203F $2040 BETWEEN? or ;

: PN-CHAR? ( n -- bool )
   dup 45 = swap VAR-CHAR? or ;

: SPAN-SCALAR ( ptr u8 n n -- n n )
   UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF ENDOF
      raw-byte OF 2drop E-SYNTAX throw ENDOF
   ;MATCH ;

: SCALAR ( ptr syntax -- n n )
   {: state :}
   state CHECK-BUDGET
   state SYNTAX-SOURCE @ state SYNTAX-LENGTH @
   state SYNTAX-POSITION @ SPAN-SCALAR ;

: CODEPOINT! ( n ptr u8 -- n )
   {: point:n out :}
   point 0 < point $10FFFF > or
   point $D800 $DFFF BETWEEN? or if E-SYNTAX throw then
   point $80 < if point out c! 1 exit then
   point $800 < if
      point 6 rshift $C0 or out c!
      point $3F and $80 or out 1+ c! 2 exit
   then
   point $10000 < if
      point 12 rshift $E0 or out c!
      point 6 rshift $3F and $80 or out 1+ c!
      point $3F and $80 or out 2 + c! 3 exit
   then
   point 18 rshift $F0 or out c!
   point 12 rshift $3F and $80 or out 1+ c!
   point 6 rshift $3F and $80 or out 2 + c!
   point $3F and $80 or out 3 + c! 4 ;

: HEX-CODEPOINT ( ptr u8 n -- n )
   {: text size:n :}
   0 size 0 ?do 16 * text i + c@ HEX-VALUE + loop ;

: ESCAPE-DIGITS ( ptr syntax -- n )
   {: state :}
   state PEEK 92 <> if 0 exit then
   state state SYNTAX-POSITION @ 1+ AT-BYTE
   case 117 of 4 endof 85 of 8 endof 0 swap endcase ;

: ESCAPED-SCALAR ( ptr syntax -- n n )
   {: state :}
   state ESCAPE-DIGITS {: digits:n :}
   digits 0= if state SCALAR exit then
   state SYNTAX-POSITION @ {: offset:n :}
   state SYNTAX-LENGTH @ offset - digits 2 + <
      if E-SYNTAX throw then
   state SYNTAX-SOURCE @ offset 2 + + digits HEX-CODEPOINT
   offset digits 2 + + ;

\ SPARQL 1.1 section 19.2 precedes the grammar, even in comments.
\ Read only original input; an emitted backslash is never rescanned.
: CODEPOINT-ESCAPES ( ptr syntax -- )
   {: state :}
   state CHECK-BUDGET
   state SYNTAX-ARENA @ ARENA:ALLOCATOR
      state SYNTAX-LENGTH @ ALLOC:BYTES drop {: out :}
   0 begin state PEEK -1 <> while
      {: used:n :}
      state ESCAPED-SCALAR {: point:n next:n :}
      point out used + CODEPOINT! used +
      next state SYNTAX-POSITION !
   repeat state SYNTAX-LENGTH !
   out state SYNTAX-SOURCE ! 0 state SYNTAX-POSITION ! ;

: SKIP-COMMENT ( ptr syntax -- )
   {: state :}
   begin state PEEK dup -1 <> over 10 <> and swap 13 <> and while
      state STEP
   repeat ;

: SKIP-WHITE ( ptr syntax -- )
   {: state :}
   begin
      state PEEK WHITE? if state STEP true else
         state PEEK 35 = if state SKIP-COMMENT true
         else false then
      then
   while repeat ;

: TOKEN+ ( ptr syntax n n -- )
   {: state kind:n start:n :}
   state SYNTAX-TOKENS @ VEC:PUSH {: token :}
   kind token TOKEN-KIND ! start token TOKEN-OFFSET !
   state SYNTAX-SOURCE @ start + token TOKEN-TEXT !
   state SYNTAX-POSITION @ start - token TOKEN-SIZE ! ;

: REQUIRE-HEX ( ptr syntax n -- )
   {: state count:n :}
   count 0 ?do
      state PEEK HEX? 0= if E-SYNTAX throw then
      state STEP
   loop ;

: LOCAL-ESCAPE? ( n -- bool )
   s" _~.-!$&'()*+,;=/?#@%" rot INDEX-OF
   MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

: STRING-ESCAPE ( ptr syntax -- )
   {: state :}
   state STEP
   state PEEK dup 34 = over 39 = or over 92 = or if
      drop state STEP exit
   then
   s" tbnrf" rot INDEX-OF
   MATCH option
      none OF E-SYNTAX throw ENDOF
      some OF drop state STEP ENDOF
   ;MATCH ;

: QUOTED? ( ptr syntax n n -- bool )
   {: state quote:n width:n :}
   width 0 ?do
      state state SYNTAX-POSITION @ i + AT-BYTE
      quote <> if false unloop exit then
   loop true ;

: SCAN-STRING ( ptr syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state PEEK {: quote:n :}
   state quote 3 QUOTED? if 3 else 1 then {: width:n :}
   width state SYNTAX-POSITION +!
   begin state quote width QUOTED? 0= while
      state PEEK -1 = if E-SYNTAX throw then
      width 1 = state PEEK dup 10 = swap 13 = or and
      if E-SYNTAX throw then
      state PEEK 92 = if state STRING-ESCAPE
      else state STEP then
   repeat
   width state SYNTAX-POSITION +!
   state T-STRING start TOKEN+ ;

: IRI-BYTE? ( n -- bool )
   {: value:n :}
   value 32 <= if false exit then
   value 34 = value 92 = or if false exit then
   value s" <>{}|^`" rot INDEX-OF
   MATCH option
      none OF true ENDOF
      some OF drop false ENDOF
   ;MATCH ;

: IRI-AHEAD? ( ptr syntax -- bool )
   {: state :}
   state SYNTAX-POSITION @ 1+
   begin dup state SYNTAX-LENGTH @ < while
      state over AT-BYTE dup 62 = if
         2drop true exit
      then
      IRI-BYTE? 0= if drop false exit then 1+
   repeat drop false ;

: SCAN-IRI ( ptr syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state STEP
   begin state PEEK 62 <> while
      state PEEK IRI-BYTE? 0= if E-SYNTAX throw then
      state STEP
   repeat state STEP state T-IRI start TOKEN+ ;

: SCAN-DIGITS ( ptr syntax -- )
   {: state :}
   begin state PEEK DIGIT? while state STEP repeat ;

: EXPONENT ( ptr syntax -- )
   {: state :}
   state STEP
   state PEEK dup 43 = swap 45 = or if state STEP then
   state PEEK DIGIT? 0= if E-SYNTAX throw then
   state SCAN-DIGITS ;

: SCAN-NUMBER ( ptr syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state SCAN-DIGITS
   state PEEK 46 = if
      state state SYNTAX-POSITION @ 1+ AT-BYTE
      dup DIGIT? over 101 = or swap 69 = or
   else false then if
      state STEP state SCAN-DIGITS T-DECIMAL
   else T-INTEGER then
   state PEEK dup 101 = swap 69 = or if
      drop state EXPONENT T-DOUBLE
   then
   state swap start TOKEN+ ;

: SCAN-VARIABLE ( ptr syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state STEP state SCALAR drop
   dup PN-U? swap DIGIT? or 0= if E-SYNTAX throw then
   begin state PEEK -1 <> while
      state SCALAR {: scalar:n next:n :}
      scalar VAR-CHAR? 0= if
         state T-VAR start TOKEN+ exit
      then
      next state SYNTAX-POSITION !
   repeat state T-VAR start TOKEN+ ;

: NAME-STEP? ( ptr syntax -- bool )
   {: state :}
   state PEEK -1 = if false exit then
   state PEEK 92 = if
      state STEP state PEEK LOCAL-ESCAPE? 0= if
         E-SYNTAX throw
      then state STEP true exit
   then
   state PEEK 37 = if
      state STEP state 2 REQUIRE-HEX true exit
   then
   state SCALAR {: scalar:n next:n :}
   scalar PN-CHAR? scalar 46 = or scalar 58 = or if
      next state SYNTAX-POSITION ! true
   else false then ;

: TRIM-DOTS ( ptr syntax n -- )
   {: state start:n :}
   begin state SYNTAX-POSITION @ start > if
      state state SYNTAX-POSITION @ 1- AT-BYTE 46 =
      state state SYNTAX-POSITION @ 2 - AT-BYTE 92 <> and
   else false then while
      -1 state SYNTAX-POSITION +!
   repeat ;

: NAME-CHAR? ( n n n -- bool )
   {: scalar:n offset:n mode:n :}
   offset 0= if
      mode 0= if scalar PN-BASE? exit then
      scalar PN-U? scalar DIGIT? or
   else scalar PN-CHAR? scalar 46 = or then
   mode 1 = scalar 58 = and or ;

\ A prefix, local name and blank-node label have different first/last
\ character rules. Percent escapes are preserved, local escapes decoded later.
: NAME-PART ( ptr syntax ptr u8 n n -- )
   {: state text size:n mode:n :}
   size 0= if mode 2 = if E-SYNTAX throw then exit then
   0 begin dup size < while
      {: offset:n :} state CHECK-BUDGET
      text offset + c@ {: byte:n :}
      byte 92 = byte 37 = or if
         mode 1 <> if E-SYNTAX throw then
         byte 92 = if 2 else 3 then offset +
      else
         text size offset SPAN-SCALAR {: scalar:n next:n :}
         scalar offset mode NAME-CHAR? 0= if E-SYNTAX throw then
         scalar 46 = next size = and if E-SYNTAX throw then
         next
      then
   repeat drop ;

: CHECK-NAME ( ptr syntax ptr u8 n -- n )
   {: state text size:n :}
   text size s" _:" STARTS-WITH? if
      state text 2 + size 2 - 2 NAME-PART T-BNODE exit
   then
   text size 58 INDEX-OF MATCH option
      none OF T-WORD ENDOF
      some OF
         IDX>N {: colon:n :}
         state text colon 0 NAME-PART
         state text colon 1+ + size colon 1+ - 1 NAME-PART
         T-NAME
      ENDOF
   ;MATCH ;

: SCAN-NAME ( ptr syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   begin state NAME-STEP? while repeat
   state start TRIM-DOTS
   state SYNTAX-POSITION @ start = if E-SYNTAX throw then
   state SYNTAX-SOURCE @ start +
   state SYNTAX-POSITION @ start - {: text size:n :}
   state text size CHECK-NAME state swap start TOKEN+ ;

: SCAN-LANGUAGE ( ptr syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state STEP
   state PEEK LETTER? 0= if E-SYNTAX throw then
   begin state PEEK LETTER? while state STEP repeat
   begin state PEEK 45 = while
      state STEP
      state PEEK dup LETTER? swap DIGIT? or 0= if
         E-SYNTAX throw
      then
      begin state PEEK dup LETTER? swap DIGIT? or while
         state STEP
      repeat
   repeat state T-LANG start TOKEN+ ;

: TWO-PUNCT? ( n n -- bool )
   {: first:n second:n :}
   second 61 = if
      first 60 = first 62 = or first 33 = or exit
   then
   first second = if
      first 38 = first 124 = or first 94 = or exit
   then false ;

: SCAN-PUNCT ( ptr syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state PEEK {: first:n :} state STEP
   first state PEEK TWO-PUNCT? if state STEP then
   state T-PUNCT start TOKEN+ ;

: LEX-ONE ( ptr syntax -- )
   {: state :}
   state PEEK {: char:n :}
   char dup 34 = swap 39 = or if state SCAN-STRING exit then
   char 60 = if state IRI-AHEAD? if state SCAN-IRI exit then then
   char dup 63 = swap 36 = or if
      state state SYNTAX-POSITION @ 1+ AT-BYTE
      dup PN-U? swap DIGIT? or if state SCAN-VARIABLE exit then
   then
   char DIGIT? if state SCAN-NUMBER exit then
   char 46 = if
      state state SYNTAX-POSITION @ 1+ AT-BYTE DIGIT?
      if state SCAN-NUMBER exit then
   then
   char 64 = if state SCAN-LANGUAGE exit then
   char PN-U? char 58 = or if state SCAN-NAME exit then
   char s" {}()[];,.=<>!+-*/|^?" rot INDEX-OF
   MATCH option
      none OF
         char dup 38 = swap 124 = or 0= if E-SYNTAX throw then
         state SCAN-PUNCT
      ENDOF
      some OF drop state SCAN-PUNCT ENDOF
   ;MATCH ;

: LEX ( ptr syntax -- )
   {: state :}
   state CODEPOINT-ESCAPES
   state SKIP-WHITE
   begin state PEEK -1 <> while
      state LEX-ONE state SKIP-WHITE
   repeat ;

;package
