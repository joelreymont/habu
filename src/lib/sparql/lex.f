\ Tokens retain spans in the codepoint-decoded query: the query as given
\ when it holds no \u or \U escape, else an arena copy.
\ String and local-name escapes are decoded later, when reading terms.
\ LEX reads the whole query once, so a lexer rejection precedes any parser
\ one, and keeps no token; the parser then lexes each token again as it
\ reads on. A token lexed from its start reads as in that first pass.

require src/lib/sparql/syntax.f
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

: HEX-DIGITS ( ^syntax n n -- n )
   {: state offset:n count:n :}
   0 count 0 ?do
      state PEEK HEX? 0= if
         state offset s\" a hex digit (\\u takes 4, \\U 8)"
            s\" write \\u with 4 hex digits, as in \\u00E9" REJECT-SPAN
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
   state offset digits HEX-DIGITS {: point:n :}
   point $10FFFF > point $D800 $DFFF BETWEEN? or if
      state offset
         s" a Unicode scalar value: at most 10FFFF, not D800 to DFFF"
         s\" write the character itself or one \\U escape, as in \\U0001F600"
         REJECT-SPAN
   then point state SYNTAX-POSITION @ ;

\ Past each code point before the first \u or \U escape; false at the end.
: TO-ESCAPE? ( ^syntax -- bool )
   {: state :}
   begin state PEEK -1 <> while
      state ESCAPE-DIGITS 0<> if true exit then
      state SCALAR nip state SYNTAX-POSITION !
   repeat false ;

\ Decodes from the first escape, at the position, into a copy holding the
\ bytes before it. Read only original input; an emitted backslash is never
\ rescanned. Each escape's end is recorded, so a rejection places itself in
\ the original.
: DECODE-ESCAPES ( ^syntax -- )
   {: state :}
   state SYNTAX-SOURCE @ str> {: text size:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR size ALLOC:BYTES drop
      {: out :}
   state SYNTAX-POSITION @ {: start:n :}
   text out start BYTE-COPY
   state SYNTAX-ESCAPES @ {: shifts :}
   start begin state PEEK -1 <> while
      {: used:n :}
      state ESCAPE-DIGITS 0<> {: escape:bool :}
      state ESCAPED-SCALAR {: point:n next:n :}
      point out used + UTF8:SCALAR! out - {: wrote:n :}
      escape if
         shifts VEC:PUSH {: pair :}
         wrote pair ESCAPE-DECODED ! next pair ESCAPE-ORIGINAL !
      then
      next state SYNTAX-POSITION ! wrote
   repeat out swap >str state SYNTAX-SOURCE ! ;

\ SPARQL 1.1 section 19.2 precedes the grammar, even in comments.
: CODEPOINT-ESCAPES ( ^syntax -- )
   {: state :}
   state TO-ESCAPE? if state DECODE-ESCAPES then
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

: TOKEN! ( ^syntax n n -- )
   {: state kind:n start:n :}
   state SYNTAX-NEXT {: token :}
   kind token TOKEN-KIND !
   state SYNTAX-SOURCE @ str> drop start +
   state SYNTAX-POSITION @ start - >str token TOKEN-TEXT ! ;

\ Steps over count hex digits; the position follows a %.
: REQUIRE-HEX ( ^syntax n -- )
   {: state count:n :}
   state SYNTAX-POSITION @ 1- {: percent:n :}
   count 0 ?do
      state PEEK HEX? 0= if
         percent 1+ state SYNTAX-POSITION !
         state percent s" a hex digit after %"
            s\" write % as \\% or follow it with two hex digits" REJECT-SPAN
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
   state SYNTAX-POSITION @ {: slash:n :}
   state STEP
   state PEEK dup 34 = over 39 = or over 92 = or if
      drop state STEP exit
   then
   s" tbnrf" swap STR:INDEX-OF
   MATCH option
      none OF
         state slash
            s\" an escape: \\t \\b \\n \\r \\f \\\" \\' or \\\\"
            s\" write a backslash as \\\\" REJECT-SPAN
      ENDOF
      some OF drop state STEP ENDOF
   ;MATCH ;

: QUOTED? ( ^syntax n n -- bool )
   {: state quote:n width:n :}
   width 0 ?do
      state state SYNTAX-POSITION @ i + AT-BYTE
      quote <> if false unloop exit then
   loop true ;

\ Whether the quote byte is at or after offset n, outside an escape.
: CLOSED? ( ^syntax n n -- bool )
   {: state at:n quote:n :}
   at begin state over AT-BYTE dup -1 <> while
      dup quote = if 2drop true exit then
      92 = if 1+ then 1+
   repeat 2drop false ;

\ Whether byte n ends a line or a group a string may be in.
: STRING-STOP? ( n -- bool )
   dup -1 = over 10 = or over 13 = or
   over 125 = or over 41 = or swap 93 = or ;

\ Rejects a string no quote closes, at the first `}`, `)` or `]` from
\ offset n on its line, else at the line's end.
: UNCLOSED ( ^syntax n -- )
   {: state from:n :}
   from begin state over AT-BYTE STRING-STOP? 0= while 1+ repeat
   state SYNTAX-POSITION !
   state s" the string's closing quote"
      s" close the string with the quote that opens it" REJECT-BYTE ;

\ Where the line break at offset n ends, past its CRLF or LF.
: BREAK-END ( ^syntax n -- n )
   {: state at:n :}
   state at AT-BYTE 13 =  state at 1+ AT-BYTE 10 =  and
   if at 2 + else at 1+ then ;

\ Rejects the line break at the position, whole, in the one-quote string
\ opened at offset n by the quote byte; unclosed if no quote follows.
: LINE-BREAK ( ^syntax n n -- )
   {: state start:n quote:n :}
   state SYNTAX-POSITION @ {: at:n :}
   state at quote CLOSED? 0= if state start 1+ UNCLOSED then
   state at BREAK-END state SYNTAX-POSITION !
   state at s" the closing quote before the line ends"
      s\" write the line break as \\n" REJECT-SPAN ;

: SCAN-STRING ( ^syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state PEEK {: quote:n :}
   state quote 3 QUOTED? if 3 else 1 then {: width:n :}
   width state SYNTAX-POSITION +!
   begin state quote width QUOTED? 0= while
      state PEEK -1 = if state start width + UNCLOSED then
      width 1 = state PEEK dup 10 = swap 13 = or and if
         state start quote LINE-BREAK
      then
      state PEEK 0= if
         state s" a string without U+0000" s" remove it" REJECT-BYTE
      then
      state PEEK 92 = if state STRING-ESCAPE
      else state STEP then
   repeat
   width state SYNTAX-POSITION +!
   state T-STRING start TOKEN! ;

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
   state STEP state T-IRI start TOKEN! ;

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
   state swap start TOKEN! ;

: SCAN-VARIABLE ( ^syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state STEP
   begin state PEEK -1 <> while
      state SCALAR {: scalar:n next:n :}
      scalar VAR-CHAR? 0= if
         state T-VAR start TOKEN! exit
      then
      next state SYNTAX-POSITION !
   repeat state T-VAR start TOKEN! ;

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
      state start s" a blank node label after _:"
         s" write a label after _:, as in _:b1" REJECT-SPAN
   then
   begin state NAME-STEP? while repeat
   state start TRIM-DOTS
   state T-BNODE start TOKEN! ;

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
   state T-NAME start TOKEN! ;

: SCAN-WORD ( ^syntax n -- )
   {: state start:n :}
   state SYNTAX-SOURCE @ str> drop start +
   state SYNTAX-POSITION @ start - >str {: text:str :}
   state text END-WORD
   state T-WORD start TOKEN! ;

: LABEL? ( ^syntax -- bool )
   {: state :}
   state PEEK 95 =
   state state SYNTAX-POSITION @ 1+ AT-BYTE 58 = and ;

\ Rejects the prefixed name from offset n whole: its prefix ends at the
\ position, in dots up to the colon at offset c.
: DOTTED-PREFIX ( ^syntax n n -- )
   {: state start:n colon:n :}
   state SYNTAX-POSITION @ {: trimmed:n :}
   colon state SYNTAX-POSITION !  state STEP state SCAN-LOCAL
   state SYNTAX-SOURCE @ str> drop {: text :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: fix :}
   s" replace it with " fix VEC:APPEND
   text start + trimmed start - >str fix VEC:APPEND
   text colon + state SYNTAX-POSITION @ colon - >str fix VEC:APPEND
   state start s" a prefix that does not end in ." fix VEC:BYTES
      REJECT-SPAN ;

\ Whether the text from offset n to the position is true or false, the
\ words that end a triple, so a dot may follow them.
: BOOLEAN-WORD? ( ^syntax n -- bool )
   {: state start:n :}
   state SYNTAX-SOURCE @ str> drop start +
   state SYNTAX-POSITION @ start - >str {: text:str :}
   text s" true" STR:=CI  text s" false" STR:=CI  or ;

\ A colon right after the text ends a prefix. A prefix cannot end in a dot,
\ so a colon right after trimmed dots ends a name rejected whole, unless
\ the text is true or false: `true.:p` is `true`, `.` and `:p`.
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
   state SYNTAX-POSITION @ {: stop:n :}
   state start TRIM-DOTS
   state PEEK 58 = if state start SCAN-PREFIXED exit then
   state stop AT-BYTE 58 =  state start BOOLEAN-WORD? 0=  and if
      state start stop DOTTED-PREFIX
   then
   state start SCAN-WORD ;

\ Rejects the language tag from offset n through the letters, digits and
\ - after the position.
: REJECT-TAG ( ^syntax n str str -- )
   {: state start:n x:str f:str :}
   begin state PEEK dup LETTER? over DIGIT? or swap 45 = or while
      state STEP
   repeat state start x f REJECT-SPAN ;

: SCAN-LANGUAGE ( ^syntax -- )
   {: state :}
   state SYNTAX-POSITION @ {: start:n :}
   state STEP
   state PEEK LETTER? 0= if
      state start s" a letter after @"
         s" write a language tag after @, as in @en" REJECT-TAG
   then
   begin state PEEK LETTER? while state STEP repeat
   begin state PEEK 45 = while
      state STEP
      state PEEK dup LETTER? swap DIGIT? or 0= if
         state start s" a letter or digit after - in a language tag"
            s" follow the - with letters or digits, as in @en-GB" REJECT-TAG
      then
      begin state PEEK dup LETTER? swap DIGIT? or while
         state STEP
      repeat
   repeat state T-LANG start TOKEN! ;

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
   state T-PUNCT start TOKEN! ;

\ Whether code point n ends a malformed variable: a blank, or a character
\ that starts a comment, string, IRI, variable, name, tag or punctuation.
: ENDS-VARIABLE? ( n -- bool )
   dup WHITE? swap
   s\" {}()[];,.=<>!+-*/|^?&$@:\"'#" swap STR:INDEX-OF
   MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH or ;

\ Whether the position holds a code point of a malformed variable.
: IN-VARIABLE? ( ^syntax -- bool )
   dup PEEK -1 = if drop false exit then
   SCALAR drop ENDS-VARIABLE? 0= ;

\ Rejects the variable at the position, its ? or $ the code point n, through
\ the code points up to one that ends it.
: BAD-VARIABLE ( ^syntax n -- )
   {: state sigil:n :}
   state SYNTAX-POSITION @ {: start:n :}
   state STEP
   begin state IN-VARIABLE? while
      state SCALAR nip state SYNTAX-POSITION !
   repeat
   sigil 63 = if
      state start s" a variable name after ?"
         s" write a name after ?, as in ?x" REJECT-SPAN
   then
   state start s" a variable name after $"
      s" write a name after $, as in $x" REJECT-SPAN ;

: LEX-ONE ( ^syntax -- )
   {: state :}
   state PEEK {: char:n :}
   char dup 34 = swap 39 = or if state SCAN-STRING exit then
   char 60 = if state IRI-AHEAD? if state SCAN-IRI exit then then
   char dup 63 = swap 36 = or if
      state state SYNTAX-POSITION @ 1+ SCALAR-AT {: next:n :}
      next PN-U? next DIGIT? or if state SCAN-VARIABLE exit then
      next -1 <> next ENDS-VARIABLE? 0= and if
         state char BAD-VARIABLE
      then
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

\ Lexes the token at the position, and the white space after it, as the
\ next token; at the end, one of kind 0 and no text.
: LEX-NEXT ( ^syntax -- )
   {: state :}
   state PEEK -1 = if
      state 0 state SYNTAX-POSITION @ TOKEN! exit
   then
   state LEX-ONE state SKIP-WHITE ;

\ Lexes on from decoded offset n, a token's start or the query's.
: LEX-FROM ( ^syntax n -- )
   {: state at:n :}
   at state SYNTAX-POSITION !
   0 state SYNTAX-WORD-END ! ;

\ The token's decoded offset.
: TOKEN-OFFSET ( ^syntax ^token -- n )
   {: state token :}
   token TOKEN-TEXT @ str> drop
   state SYNTAX-SOURCE @ str> drop - ;

: TOKEN-COPY ( ^token ^token -- )
   {: from to :}
   from TOKEN-KIND @ to TOKEN-KIND !
   from TOKEN-TEXT @ to TOKEN-TEXT ! ;

: NEXT-TOKEN ( ^syntax -- )
   {: state :}
   state SYNTAX-CURRENT state SYNTAX-PREVIOUS TOKEN-COPY
   state SYNTAX-NEXT state SYNTAX-CURRENT TOKEN-COPY
   state state SYNTAX-CURRENT TOKEN-OFFSET state SYNTAX-CURSOR !
   state LEX-NEXT ;

\ The decoded offset of the token before the current one.
: PREVIOUS-AT ( ^syntax -- n )
   dup SYNTAX-PREVIOUS TOKEN-OFFSET ;

\ A scan lexes into the parser's next token and moves the lexer, so it
\ runs under SCANNING.

\ Lexes the token at decoded offset n as the next token, the lexer then
\ at the offset of the token after it.
: LEX-AT ( ^syntax n -- ^token )
   {: state at:n :}
   state at LEX-FROM state LEX-NEXT state SYNTAX-NEXT ;

\ The decoded offset of the token after the one at offset n.
: PAST-TOKEN ( ^syntax n -- n )
   {: state at:n :}
   state at LEX-AT drop state SYNTAX-POSITION @ ;

\ The decoded offset of the query's first token.
: FIRST-AT ( ^syntax -- n )
   {: state :}
   state 0 LEX-FROM state SKIP-WHITE state SYNTAX-POSITION @ ;

\ Runs the scan, then puts the lexer's position and word end and the
\ parser's next token back.
: SCANNING ( R ^syntax [ R -- S ] -- S )
   {: state scan :}
   state SYNTAX-POSITION @ {: at:n :}
   state SYNTAX-WORD-END @ {: end:n :}
   state SYNTAX-NEXT {: next :}
   next TOKEN-KIND @ {: kind:n :}
   next TOKEN-TEXT @ {: text:str :}
   scan execute
   at state SYNTAX-POSITION !  end state SYNTAX-WORD-END !
   kind next TOKEN-KIND !  text next TOKEN-TEXT ! ;

: LEX ( ^syntax -- )
   {: state :}
   state CODEPOINT-ESCAPES
   state SKIP-WHITE
   begin state PEEK -1 <> while state LEX-NEXT repeat
   state 0 LEX-FROM state SKIP-WHITE
   state LEX-NEXT state NEXT-TOKEN ;

\ Rejects at the token at decoded offset n, found its text; at the query's
\ length, at its end.
: REJECT-TOKEN ( ^syntax n str str -- )
   {: state at:n x:str f:str :}
   state at LEX-FROM state LEX-NEXT
   state at x state SYNTAX-NEXT TOKEN-TEXT @ f REJECTED ;

\ Rejects at the parser's current token.
: REJECT ( ^syntax str str -- )
   {: state x:str f:str :}
   state state SYNTAX-CURSOR @ x f REJECT-TOKEN ;

: NEST ( ^syntax -- )
   dup SYNTAX-NESTING @ 256 >= if
      s" at most 256 nested groups or expressions"
         s" nest fewer groups or expressions" REJECT
   then
   1 swap SYNTAX-NESTING +! ;

: UNNEST ( ^syntax -- ) -1 swap SYNTAX-NESTING +! ;

\ Rejects at the token node n was read at.
: REJECT-NODE ( ^syntax n str str -- )
   {: state index:n x:str f:str :}
   state state index NODE-TOKEN@ x f REJECT-TOKEN ;

;package
