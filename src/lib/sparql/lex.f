\ Tokens retain spans in the arena-owned, codepoint-decoded query.
\ String and local-name escapes are decoded later, when reading terms.

require src/lib/sparql/syntax.f
require src/lib/sparql/segs.f
require src/lib/utf8-scalar.f
require src/lib/vector.f

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

: AT-BYTE ( ^syntax n -- n )
   {: state offset:n :}
   state SYNTAX-SOURCE @ str> {: text size:n :}
   offset 0 < if -1 exit then
   offset size >= if -1 exit then
   text offset + c@ ;

: PEEK ( ^syntax -- n )
   dup SYNTAX-POSITION @ AT-BYTE ;

: STEP ( ^syntax -- )
   1 swap SYNTAX-POSITION +! ;

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
   dup DIGIT? if 48 - else STR:ASCII-LOWER 87 - then ;

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

\ What a token may start with, expected where none does.
: ANY-TOKEN ( -- str )
   s" a keyword, name, variable, IRI, literal or punctuation" ;

\ Rejects at the decoded query's byte the address points to.
: REJECT-HERE ( ^syntax ^u8 str str -- )
   {: state at:^ x:str f:str :}
   at state SYNTAX-SOURCE @ str> drop - state SYNTAX-POSITION !
   state x f REJECT-BYTE ;

: SPAN-SCALAR ( ^syntax str n -- n n )
   {: state source:str at:n :}
   source at UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF ENDOF
      raw-byte OF
         2drop state source str> drop at +
            s" UTF-8 text" s" write the query in UTF-8" REJECT-HERE
      ENDOF
   ;MATCH ;

: SCALAR ( ^syntax -- n n )
   {: state :}
   state state SYNTAX-SOURCE @
   state SYNTAX-POSITION @ SPAN-SCALAR ;

\ The code point at the offset, -1 past the end.
: SCALAR-AT ( ^syntax n -- n )
   {: state at:n :}
   state at AT-BYTE -1 = if -1 exit then
   state state SYNTAX-SOURCE @ at SPAN-SCALAR drop ;

: HEX-DIGITS ( ^syntax n -- n )
   {: state count:n :}
   0 count 0 ?do
      state PEEK HEX? 0= if
         state s\" a hex digit (\\u takes 4, \\U 8)"
            s\" write \\u with 4 hex digits, as in \\u00E9" REJECT-BYTE
      then
      16 * state PEEK HEX-VALUE + state STEP
   loop ;

: ESCAPE-DIGITS ( ^syntax -- n )
   {: state :}
   state PEEK 92 <> if 0 exit then
   state state SYNTAX-POSITION @ 1+ AT-BYTE
   case 117 of 4 endof 85 of 8 endof 0 swap endcase ;

: ESCAPED-SCALAR ( ^syntax -- n n )
   {: state :}
   state ESCAPE-DIGITS {: digits:n :}
   digits 0= if state SCALAR exit then
   state SYNTAX-POSITION @ {: offset:n :}
   2 state SYNTAX-POSITION +!
   state digits HEX-DIGITS {: point:n :}
   point $10FFFF > point $D800 $DFFF BETWEEN? or if
      offset state SYNTAX-POSITION !
      state s" a Unicode scalar value: at most 10FFFF, not D800 to DFFF"
         s\" write the character itself or one \\U escape, as in \\U0001F600"
         REJECT-BYTE
   then point state SYNTAX-POSITION @ ;

\ SPARQL 1.1 section 19.2 precedes the grammar, even in comments.
\ Read only original input; an emitted backslash is never rescanned. Each
\ escape's end is recorded, so a rejection places itself in the original.
: CODEPOINT-ESCAPES ( ^syntax -- )
   {: state :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR {: alloc :}
   alloc state SYNTAX-SOURCE @ str> nip ALLOC:BYTES drop
      {: out :}
   alloc 0 [type] escape VEC:NEW {: shifts :}
   0 begin state PEEK -1 <> while
      {: used:n :}
      state ESCAPE-DIGITS 0<> {: escape:bool :}
      state ESCAPED-SCALAR {: point:n next:n :}
      point out used + UTF8:SCALAR! out - {: wrote:n :}
      escape if
         shifts VEC:PUSH {: pair :}
         wrote pair ESCAPE-DECODED ! next pair ESCAPE-ORIGINAL !
      then
      next state SYNTAX-POSITION ! wrote
   repeat out swap >str state SYNTAX-SOURCE !
   shifts state SYNTAX-ESCAPES !
   0 state SYNTAX-POSITION ! ;

: SKIP-COMMENT ( ^syntax -- )
   {: state :}
   begin state PEEK dup -1 <> over 10 <> and swap 13 <> and while
      state STEP
   repeat ;

: SKIP-WHITE ( ^syntax -- )
   {: state :}
   begin
      state PEEK WHITE? if state STEP true else
         state PEEK 35 = if state SKIP-COMMENT true
         else false then
      then
   while repeat ;

: TOKEN+ ( ^syntax n n -- )
   {: state kind:n start:n :}
   state SYNTAX-TOKENS @ SEG-PUSH {: token :}
   kind token TOKEN-KIND !
   state SYNTAX-SOURCE @ str> drop start +
   state SYNTAX-POSITION @ start - >str token TOKEN-TEXT ! ;

: REQUIRE-HEX ( ^syntax n -- )
   {: state count:n :}
   count 0 ?do
      state PEEK HEX? 0= if
         state s" a hex digit after %"
            s\" write % as \\% or follow it with two hex digits" REJECT-BYTE
      then state STEP
   loop ;

: LOCAL-ESCAPE? ( n -- bool )
   s" _~.-!$&'()*+,;=/?#@%" swap STR:INDEX-OF
   MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

: STRING-ESCAPE ( ^syntax -- )
   {: state :}
   state STEP
   state PEEK dup 34 = over 39 = or over 92 = or if
      drop state STEP exit
   then
   s" tbnrf" swap STR:INDEX-OF
   MATCH option
      none OF
         state s\" an escape: \\t \\b \\n \\r \\f \\\" \\' or \\\\"
            s\" write a backslash as \\\\" REJECT-BYTE
      ENDOF
      some OF drop state STEP ENDOF
   ;MATCH ;

: QUOTED? ( ^syntax n n -- bool )
   {: state quote:n width:n :}
   width 0 ?do
      state state SYNTAX-POSITION @ i + AT-BYTE
      quote <> if false unloop exit then
   loop true ;

: SCAN-STRING ( ^syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state PEEK {: quote:n :}
   state quote 3 QUOTED? if 3 else 1 then {: width:n :}
   width state SYNTAX-POSITION +!
   begin state quote width QUOTED? 0= while
      state PEEK -1 = if
         state s" the string's closing quote"
            s" close the string with the quote that opens it" REJECT-BYTE
      then
      width 1 = state PEEK dup 10 = swap 13 = or and if
         state s" the closing quote before the line ends"
            s\" write the line break as \\n" REJECT-BYTE
      then
      state PEEK 0= if
         state s" a string without U+0000" s" remove it" REJECT-BYTE
      then
      state PEEK 92 = if state STRING-ESCAPE
      else state STEP then
   repeat
   width state SYNTAX-POSITION +!
   state T-STRING start TOKEN+ ;

: IRI-BYTE? ( n -- bool )
   {: value:n :}
   value 32 <= if false exit then
   value 34 = value 92 = or if false exit then
   value s" <>{}|^`" swap STR:INDEX-OF
   MATCH option
      none OF true ENDOF
      some OF drop false ENDOF
   ;MATCH ;

: IRI-AHEAD? ( ^syntax -- bool )
   {: state :}
   state SYNTAX-POSITION @ 1+
   begin dup state SYNTAX-SOURCE @ str> nip < while
      state over AT-BYTE dup 62 = if
         2drop true exit
      then
      IRI-BYTE? 0= if drop false exit then 1+
   repeat drop false ;

: SCAN-IRI ( ^syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state STEP
   begin state PEEK 62 <> while state STEP repeat
   state STEP state T-IRI start TOKEN+ ;

: SCAN-DIGITS ( ^syntax -- )
   {: state :}
   begin state PEEK DIGIT? while state STEP repeat ;

\ At the exponent of a number. An `e` without digits starts a name.
: EXPONENT? ( ^syntax n -- bool )
   {: state start:n :}
   state start AT-BYTE dup 101 = swap 69 = or 0= if false exit then
   start 1+ state over AT-BYTE dup 43 = swap 45 = or if 1+ then
   state swap AT-BYTE DIGIT? ;

: SCAN-EXPONENT ( ^syntax -- )
   {: state :}
   state STEP
   state PEEK DIGIT? 0= if state STEP then
   state SCAN-DIGITS ;

: SCAN-NUMBER ( ^syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state SCAN-DIGITS
   state PEEK 46 = if
      state SYNTAX-POSITION @ 1+ {: next:n :}
      state next AT-BYTE DIGIT? state next EXPONENT? or
   else false then if
      state STEP state SCAN-DIGITS T-DECIMAL
   else T-INTEGER then
   state state SYNTAX-POSITION @ EXPONENT? if
      drop state SCAN-EXPONENT T-DOUBLE
   then
   state swap start TOKEN+ ;

: SCAN-VARIABLE ( ^syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state STEP
   begin state PEEK -1 <> while
      state SCALAR {: scalar:n next:n :}
      scalar VAR-CHAR? 0= if
         state T-VAR start TOKEN+ exit
      then
      next state SYNTAX-POSITION !
   repeat state T-VAR start TOKEN+ ;

\ Past a local name's escape, if one is here.
: ESCAPE-STEP? ( ^syntax -- bool )
   {: state :}
   state PEEK 92 = if
      state STEP state PEEK LOCAL-ESCAPE? 0= if
         state s" a character a local name escapes: _~.-!$&'()*+,;=/?#@%"
            s" remove the backslash" REJECT-BYTE
      then state STEP true exit
   then
   state PEEK 37 = dup if
      state STEP state 2 REQUIRE-HEX
   then ;

: NAME-STEP? ( ^syntax -- bool )
   {: state :}
   state PEEK -1 = if false exit then
   state SCALAR {: scalar:n next:n :}
   scalar PN-CHAR? scalar 46 = or if
      next state SYNTAX-POSITION ! true
   else false then ;

: LOCAL-STEP? ( ^syntax -- bool )
   {: state :}
   state PEEK 58 = if state STEP true exit then
   state ESCAPE-STEP? if true exit then
   state NAME-STEP? ;

: TRIM-DOTS ( ^syntax n -- )
   {: state start:n :}
   begin state SYNTAX-POSITION @ start > if
      state state SYNTAX-POSITION @ 1- AT-BYTE 46 =
      state state SYNTAX-POSITION @ 2 - AT-BYTE 92 <> and
   else false then while
      -1 state SYNTAX-POSITION +!
   repeat ;

\ No keyword has a dot or a minus.
: WORD-BREAK? ( n -- bool ) dup 46 = swap 45 = or ;

\ At a break that ends a word. Without it every word of a run of them would
\ read on to the run's end.
: WORD-EDGE? ( ^syntax -- bool )
   {: state :}
   state PEEK WORD-BREAK?
   state SYNTAX-POSITION @ state SYNTAX-WORD-END @ < and ;

\ Ends the word just read at its first break.
: END-WORD ( ^syntax str -- )
   str> {: state text size:n :}
   size 0 ?do
      text i + c@ WORD-BREAK? if
         state SYNTAX-POSITION @ state SYNTAX-WORD-END !
         i size - state SYNTAX-POSITION +!
         unloop exit
      then
   loop ;

: LABEL-START? ( n -- bool )
   dup PN-U? swap DIGIT? or ;

: SCAN-LABEL ( ^syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   2 state SYNTAX-POSITION +!
   state state SYNTAX-POSITION @ SCALAR-AT LABEL-START? 0= if
      state s" a blank node label after _:"
         s" write a label after _:, as in _:b1" REJECT-BYTE
   then
   begin state NAME-STEP? while repeat
   state start TRIM-DOTS
   state T-BNODE start TOKEN+ ;

: LOCAL-START? ( ^syntax -- bool )
   {: state :}
   state state SYNTAX-POSITION @ SCALAR-AT {: first:n :}
   first LABEL-START? first 58 = or
   first 92 = or first 37 = or ;

: SCAN-LOCAL ( ^syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state LOCAL-START? 0= if exit then
   begin state LOCAL-STEP? while repeat
   state start TRIM-DOTS ;

\ The name whose prefix, from offset n, the position's colon ends.
: SCAN-PREFIXED ( ^syntax n -- )
   {: state start:n :}
   state start AT-BYTE 95 = if
      start state SYNTAX-POSITION !
      state s" a name character" s" remove the character"
         REJECT-BYTE
   then
   state STEP state SCAN-LOCAL
   state T-NAME start TOKEN+ ;

: SCAN-WORD ( ^syntax n -- )
   {: state start:n :}
   state SYNTAX-SOURCE @ str> drop start +
   state SYNTAX-POSITION @ start - >str {: text:str :}
   state text END-WORD
   state T-WORD start TOKEN+ ;

: LABEL? ( ^syntax -- bool )
   {: state :}
   state PEEK 95 =
   state state SYNTAX-POSITION @ 1+ AT-BYTE 58 = and ;

\ A colon right after the text ends a prefix; after a trimmed dot it starts
\ a name of its own.
: SCAN-NAME ( ^syntax -- )
   {: state :}
   state LABEL? if state SCAN-LABEL exit then
   state SYNTAX-POSITION @ {: start:n :}
   begin state WORD-EDGE? if false else state NAME-STEP? then
   while repeat
   state PEEK dup 92 = swap 37 = or if
      state s" a name character; escapes go only after the colon"
         s" remove the escape" REJECT-BYTE
   then
   state start TRIM-DOTS
   state PEEK 58 = if state start SCAN-PREFIXED
   else state start SCAN-WORD then ;

: SCAN-LANGUAGE ( ^syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state STEP
   state PEEK LETTER? 0= if
      state s" a letter after @"
         s" write a language tag after @, as in @en" REJECT-BYTE
   then
   begin state PEEK LETTER? while state STEP repeat
   begin state PEEK 45 = while
      state STEP
      state PEEK dup LETTER? swap DIGIT? or 0= if
         state s" a letter or digit after - in a language tag"
            s" follow the - with letters or digits, as in @en-GB" REJECT-BYTE
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

: SCAN-PUNCT ( ^syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state PEEK {: first:n :} state STEP
   first state PEEK TWO-PUNCT? if state STEP then
   state T-PUNCT start TOKEN+ ;

: LEX-ONE ( ^syntax -- )
   {: state :}
   state PEEK {: char:n :}
   char dup 34 = swap 39 = or if state SCAN-STRING exit then
   char 60 = if state IRI-AHEAD? if state SCAN-IRI exit then then
   char dup 63 = swap 36 = or if
      state state SYNTAX-POSITION @ 1+ SCALAR-AT
      dup PN-U? swap DIGIT? or if state SCAN-VARIABLE exit then
   then
   char DIGIT? if state SCAN-NUMBER exit then
   char 46 = if
      state state SYNTAX-POSITION @ 1+ AT-BYTE DIGIT?
      if state SCAN-NUMBER exit then
   then
   char 64 = if state SCAN-LANGUAGE exit then
   state SCALAR drop PN-U? char 58 = or if
      state SCAN-NAME exit
   then
   char s" {}()[];,.=<>!+-*/|^?" swap STR:INDEX-OF
   MATCH option
      none OF
         char dup 38 = swap 124 = or 0= if
            state ANY-TOKEN s" remove the character" REJECT-BYTE
         then
         state SCAN-PUNCT
      ENDOF
      some OF drop state SCAN-PUNCT ENDOF
   ;MATCH ;

: LEX ( ^syntax -- )
   {: state :}
   state CODEPOINT-ESCAPES
   state SKIP-WHITE
   begin state PEEK -1 <> while
      state LEX-ONE state SKIP-WHITE
   repeat ;

;package
