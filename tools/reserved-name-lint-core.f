\ reserved-name-lint-core.f - reject definition names reserved by parser/control dispatch.
\
\ Also rejects number-shaped names (E-NUMERIC-DEFINITION): hb parses numeric
\ literals BEFORE dictionary lookup (test/gate-dictionary-lib.f GD-LITERAL-FIRST),
\ so a definition named like a literal - all digits (42), float (.0, 1.5, -.5),
\ or $hex ($FF) - is unreachable. Names with a dot-digit tail (U.0) are also
\ rejected: one inserted space turns the tail into a float literal, and
\ generators misread them as float spellings (the lib/fmt.f .0/U.0 incident).
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, and tools/lint/source-lex.f.

$20 constant RNL-NUM-CAP
$0A constant RNL-LF
$20 constant RNL-SP
$3A constant RNL-COLON-C
$2E constant RNL-DOT-C
$2D constant RNL-MINUS-C
$24 constant RNL-DOLLAR-C

create RNL-NUM RNL-NUM-CAP allot
create RNL-LF-BUF 1 allot
create RNL-SDQ  $73 c, $22 c,
create RNL-CDQ  $63 c, $22 c,
create RNL-DOTQ $2E c, $22 c,

variable RNL-SRC-A
variable RNL-SRC-U
variable RNL-SRC-CAP
variable RNL-FILE-A
variable RNL-FILE-U
variable RNL-BAD
variable RNL-I
variable RNL-IN-DEF
variable RNL-JSON
variable RNL-OUT-FD
variable RNL-NUM-I

: RNL-SRC-A-FIELD ( -- ptr ptr u8 )
   RNL-SRC-A 0 ptr-field ;

: RNL-FILE-A-FIELD ( -- ptr ptr u8 )
   RNL-FILE-A 0 ptr-field ;

: RNL-SRC-A@ ( -- ptr u8 )
   RNL-SRC-A-FIELD @ ;

: RNL-FILE-A@ ( -- ptr u8 )
   RNL-FILE-A-FIELD @ ;

: RNL-SRC-A! ( ptr u8 -- )
   RNL-SRC-A-FIELD ! ;

: RNL-FILE-A! ( ptr u8 -- )
   RNL-FILE-A-FIELD ! ;

: RNL-JSON! ( bool -- )
   RNL-JSON ! ;

: RNL-OUT-FD! ( fd -- )
   RNL-OUT-FD ! ;

: RNL-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   fd a u LINT-OUT-WRITE ;

: RNL-OUT ( ptr u8 n -- )
   RNL-OUT-FD @ -rot RNL-WRITE ;

: RNL-C ( n -- )
   RNL-LF-BUF c!
   RNL-LF-BUF 1 RNL-OUT ;

: RNL-NL ( -- )
   RNL-LF RNL-C ;

: RNL-U$ ( n -- ptr u8 n ) {: u:n :}
   RNL-NUM-CAP RNL-NUM-I !
   u 0= if
      RNL-NUM-I @ 1- RNL-NUM-I !
      $30 RNL-NUM RNL-NUM-I @ + c!
      RNL-NUM RNL-NUM-I @ + 1
      exit
   then
   u begin dup 0 > while
      dup 10 mod $30 +
      RNL-NUM-I @ 1- RNL-NUM-I !
      RNL-NUM RNL-NUM-I @ + c!
      10 /
   repeat drop
   RNL-NUM RNL-NUM-I @ + RNL-NUM-CAP RNL-NUM-I @ - ;

: RNL-WORD? ( n -- bool ) {: k :}
   k L# @ >= if LINT-FALSE exit then
   k LK@ L-WORD = ;

: RNL-TOK-END ( n -- n ) {: k :}
   k LB@ k LEX-TOK nip + ;

: RNL-PARSE-NEXT? ( n -- bool ) {: k :}
   k LEX-TOK s" char" LINT-STR=CI if LINT-TRUE exit then
   k LEX-TOK s" [char]" LINT-STR=CI if LINT-TRUE exit then
   k LEX-TOK s" '" LINT-STR= if LINT-TRUE exit then
   k LEX-TOK s" [']" LINT-STR= if LINT-TRUE exit then
   k LEX-TOK s" postpone" LINT-STR=CI ;

: RNL-COLON-DEFINER? ( n -- bool ) {: k :}
   k LEX-TOK s" :" LINT-STR= if LINT-TRUE exit then
   k LEX-TOK s" +:" LINT-STR= if LINT-TRUE exit then
   k LEX-TOK s" TRUSTED:" LINT-STR=CI if LINT-TRUE exit then
   k LEX-TOK s" KERNEL:" LINT-STR=CI ;

: RNL-DATA-DEFINER? ( n -- bool ) {: k :}
   k LEX-TOK s" create" LINT-STR=CI if LINT-TRUE exit then
   k LEX-TOK s" variable" LINT-STR=CI if LINT-TRUE exit then
   k LEX-TOK s" constant" LINT-STR=CI if LINT-TRUE exit then
   k LEX-TOK s" LAYOUT-BUFFER" LINT-STR=CI ;

: RNL-RESERVED-CONTROL? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" if" LINT-STR=CI if LINT-TRUE exit then
   a u s" then" LINT-STR=CI if LINT-TRUE exit then
   a u s" else" LINT-STR=CI if LINT-TRUE exit then
   a u s" begin" LINT-STR=CI if LINT-TRUE exit then
   a u s" until" LINT-STR=CI if LINT-TRUE exit then
   a u s" again" LINT-STR=CI if LINT-TRUE exit then
   a u s" while" LINT-STR=CI if LINT-TRUE exit then
   a u s" repeat" LINT-STR=CI if LINT-TRUE exit then
   a u s" case" LINT-STR=CI if LINT-TRUE exit then
   a u s" of" LINT-STR=CI if LINT-TRUE exit then
   a u s" endof" LINT-STR=CI if LINT-TRUE exit then
   a u s" endcase" LINT-STR=CI if LINT-TRUE exit then
   a u s" match" LINT-STR=CI if LINT-TRUE exit then      \ TFAM 9: MATCH eliminator control form
   a u s" ;match" LINT-STR=CI if LINT-TRUE exit then     \ TFAM 9: MATCH block close (;FOO convention)
   a u s" construct" LINT-STR=CI if LINT-TRUE exit then  \ TFAM 9: private constructor token form
   a u s" do" LINT-STR=CI if LINT-TRUE exit then
   a u s" ?do" LINT-STR=CI if LINT-TRUE exit then
   a u s" loop" LINT-STR=CI if LINT-TRUE exit then
   a u s" +loop" LINT-STR=CI if LINT-TRUE exit then
   a u s" i" LINT-STR=CI if LINT-TRUE exit then
   a u s" j" LINT-STR=CI if LINT-TRUE exit then
   a u s" leave" LINT-STR=CI if LINT-TRUE exit then
   a u s" unloop" LINT-STR=CI if LINT-TRUE exit then
   a u s" exit" LINT-STR=CI if LINT-TRUE exit then
   a u s" recurse" LINT-STR=CI ;

: RNL-RESERVED-PARSER? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u RNL-SDQ 2 LINT-STR=CI if LINT-TRUE exit then
   a u RNL-CDQ 2 LINT-STR=CI if LINT-TRUE exit then
   a u RNL-DOTQ 2 LINT-STR= if LINT-TRUE exit then
   a u s" type" LINT-STR=CI if LINT-TRUE exit then
   a u s" '" LINT-STR= if LINT-TRUE exit then
   a u s" [']" LINT-STR= if LINT-TRUE exit then
   a u s" {:" LINT-STR= if LINT-TRUE exit then
   a u s" :}" LINT-STR= if LINT-TRUE exit then
   a u s" [:" LINT-STR= if LINT-TRUE exit then
   a u s" ;]" LINT-STR= if LINT-TRUE exit then
   a u s" char" LINT-STR=CI if LINT-TRUE exit then
   a u s" [char]" LINT-STR=CI if LINT-TRUE exit then
   a u s" immediate" LINT-STR=CI if LINT-TRUE exit then
   a u s" postpone" LINT-STR=CI if LINT-TRUE exit then
   a u s" compile," LINT-STR=CI if LINT-TRUE exit then
   a u s" does>" LINT-STR=CI if LINT-TRUE exit then
   a u s" trusted:" LINT-STR=CI if LINT-TRUE exit then
   a u s" trust" LINT-STR=CI if LINT-TRUE exit then
   a u s" kernel:" LINT-STR=CI if LINT-TRUE exit then
   a u s" check-does!" LINT-STR=CI ;

: RNL-RESERVED-DEFINER? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" create" LINT-STR=CI if LINT-TRUE exit then
   a u s" variable" LINT-STR=CI if LINT-TRUE exit then
   a u s" constant" LINT-STR=CI if LINT-TRUE exit then
   a u s" layout-buffer" LINT-STR=CI if LINT-TRUE exit then
   a u s" package" LINT-STR=CI if LINT-TRUE exit then
   a u s" public" LINT-STR=CI if LINT-TRUE exit then
   a u s" private" LINT-STR=CI if LINT-TRUE exit then
   a u s" ;package" LINT-STR=CI if LINT-TRUE exit then
   a u s" export" LINT-STR=CI if LINT-TRUE exit then    \ EXPORT re-export keyword + hb-build directive
   a u s" typefamily" LINT-STR=CI if LINT-TRUE exit then
   a u s" sumtype" LINT-STR=CI if LINT-TRUE exit then
   a u s" variant" LINT-STR=CI if LINT-TRUE exit then
   a u s" ;variant" LINT-STR=CI if LINT-TRUE exit then
   a u s" ;sumtype" LINT-STR=CI if LINT-TRUE exit then
   a u s" enum" LINT-STR=CI if LINT-TRUE exit then       \ TFAM 14: enum block definer
   a u s" ;enum" LINT-STR=CI if LINT-TRUE exit then      \ TFAM 14: enum block close (;FOO)
   a u s" product" LINT-STR=CI if LINT-TRUE exit then    \ TFAM 15: product block definer
   a u s" field" LINT-STR=CI if LINT-TRUE exit then      \ TFAM 15: product field keyword
   a u s" ;product" LINT-STR=CI if LINT-TRUE exit then   \ TFAM 15: product block close (;FOO)
   a u s" undefine" LINT-STR=CI ;

: RNL-RESERVED-LOADER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" include" LINT-STR=CI if LINT-TRUE exit then
   a u s" included" LINT-STR=CI if LINT-TRUE exit then
   a u s" require" LINT-STR=CI if LINT-TRUE exit then
   a u s" required" LINT-STR=CI if LINT-TRUE exit then
   a u s" provided" LINT-STR=CI ;

: RNL-RESERVED? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u RNL-RESERVED-CONTROL? if LINT-TRUE exit then
   a u RNL-RESERVED-PARSER? if LINT-TRUE exit then
   a u RNL-RESERVED-DEFINER? if LINT-TRUE exit then
   a u RNL-RESERVED-LOADER? ;

\ ---- number-shaped names (numeric parse wins over the dictionary) ---------
: RNL-DIGIT? ( n -- bool )
   dup $2F > swap $3A < and ;

: RNL-HEX-LETTER? ( n -- bool ) {: c:n :}
   c $40 > c $47 < and if LINT-TRUE exit then
   c $60 > c $67 < and ;

: RNL-HEX-DIGIT? ( n -- bool ) {: c:n :}
   c RNL-DIGIT? if LINT-TRUE exit then
   c RNL-HEX-LETTER? ;

: RNL-DIGITS? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ nonempty, all decimal digits
   u 0= if LINT-FALSE exit then
   u 0 ?do
      a i + c@ RNL-DIGIT? 0= if LINT-FALSE unloop exit then
   loop LINT-TRUE ;

: RNL-DIGITS-OPT? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ empty or all digits
   u 0= if LINT-TRUE exit then
   a u RNL-DIGITS? ;

: RNL-HEX-DIGITS? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ nonempty, all hex digits
   u 0= if LINT-FALSE exit then
   u 0 ?do
      a i + c@ RNL-HEX-DIGIT? 0= if LINT-FALSE unloop exit then
   loop LINT-TRUE ;

: RNL-FLOAT-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n dot:n :}   \ digits* '.'@dot digits+
   a dot RNL-DIGITS-OPT?
   a dot 1+ + u dot 1+ - RNL-DIGITS?
   and ;

: RNL-FLOAT-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u RNL-DOT-C LINT-INDEX-OF MATCH option
     none OF LINT-FALSE ENDOF
     some OF a u rot RNL-FLOAT-AT? ENDOF
   ;MATCH ;

: RNL-HEX-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 < if LINT-FALSE exit then
   a c@ RNL-DOLLAR-C <> if LINT-FALSE exit then
   a 1+ u 1- RNL-HEX-DIGITS? ;

: RNL-NUM-BODY? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u RNL-DIGITS? if LINT-TRUE exit then
   a u RNL-FLOAT-NAME? if LINT-TRUE exit then
   a u RNL-HEX-NAME? ;

: RNL-NUM-CLAIMED? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ number parser owns the token
   a u RNL-NUM-BODY? if LINT-TRUE exit then
   u 2 < if LINT-FALSE exit then
   a c@ RNL-MINUS-C <> if LINT-FALSE exit then
   a 1+ u 1- RNL-NUM-BODY? ;

: RNL-DOT-DIGIT? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ contains a ".<digit>" tail
   u 2 < if LINT-FALSE exit then
   u 1- 0 ?do
      a i + c@ RNL-DOT-C =
      a i + 1+ c@ RNL-DIGIT? and if LINT-TRUE unloop exit then
   loop LINT-FALSE ;

: RNL-NUMERIC-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u RNL-NUM-CLAIMED? if LINT-TRUE exit then
   a u RNL-DOT-DIGIT? ;

: RNL-JSON-FINDING-AS ( n ptr u8 n ptr u8 n ptr u8 n -- )
   {: k:n code:ptr codeu:n why:ptr whyu:n fix:ptr fixu:n :}
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY code codeu LJW-STRING LJW-COMMA
   s" file" LJW-KEY RNL-FILE-A@ RNL-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY k LL@ LJW-U LJW-COMMA
   s" column" LJW-KEY k LC@ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY k LB@ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY k RNL-TOK-END LJW-U LJW-COMMA
   s" word" LJW-KEY k LEX-TOK LJW-STRING LJW-COMMA
   s" reason" LJW-KEY why whyu LJW-STRING LJW-COMMA
   s" suggestion" LJW-KEY fix fixu LJW-STRING
   LJW-OBJECT-END
   LJW$ RNL-OUT RNL-NL ;

: RNL-JSON-FINDING ( n -- )
   s" E-RESERVED-DEFINITION"
   s" parser/control words cannot be source definition names"
   s" Rename the definition after prefix stripping; use names like IX/JX for loop counters."
   RNL-JSON-FINDING-AS ;

: RNL-JSON-NUM-FINDING ( n -- )
   s" E-NUMERIC-DEFINITION"
   s" numeric literals parse before dictionary lookup; a number-shaped name is unreachable or float-confusable"
   s" Rename so the name cannot parse as a number: avoid digit-only, dot-digit, and $hex shapes."
   RNL-JSON-FINDING-AS ;

: RNL-TEXT-FINDING-AS ( n ptr u8 n ptr u8 n -- )
   {: k:n code:ptr codeu:n why:ptr whyu:n :}
   code codeu RNL-OUT
   RNL-SP RNL-C
   RNL-FILE-A@ RNL-FILE-U @ RNL-OUT
   RNL-COLON-C RNL-C k LL@ RNL-U$ RNL-OUT
   RNL-COLON-C RNL-C k LC@ RNL-U$ RNL-OUT
   s" : `" RNL-OUT
   k LEX-TOK RNL-OUT
   why whyu RNL-OUT
   RNL-NL ;

: RNL-TEXT-FINDING ( n -- )
   s" E-RESERVED-DEFINITION"
   s" ` is reserved by parser/control dispatch; rename after prefix stripping"
   RNL-TEXT-FINDING-AS ;

: RNL-TEXT-NUM-FINDING ( n -- )
   s" E-NUMERIC-DEFINITION"
   s" ` is number-shaped; numeric literals parse before dictionary lookup, rename it"
   RNL-TEXT-FINDING-AS ;

: RNL-REPORT ( n -- ) {: k :}
   RNL-BAD @ 1+ RNL-BAD !
   RNL-JSON @ if k RNL-JSON-FINDING exit then
   k RNL-TEXT-FINDING ;

: RNL-REPORT-NUM ( n -- ) {: k:n :}
   RNL-BAD @ 1+ RNL-BAD !
   RNL-JSON @ if k RNL-JSON-NUM-FINDING exit then
   k RNL-TEXT-NUM-FINDING ;

: RNL-CHECK-NAME ( n -- ) {: k:n :}
   k RNL-WORD? 0= if exit then
   k LEX-TOK RNL-RESERVED? if k RNL-REPORT exit then
   k LEX-TOK RNL-NUMERIC-NAME? if k RNL-REPORT-NUM then ;

: RNL-HANDLE-IN-DEF ( -- )
   RNL-I @ RNL-PARSE-NEXT? if RNL-I @ 1+ RNL-I ! exit then
   RNL-I @ LEX-TOK s" ;" LINT-STR= if 0 RNL-IN-DEF ! then ;

: RNL-HANDLE-TOP ( -- )
   RNL-I @ RNL-COLON-DEFINER? if
      RNL-I @ 1+ RNL-CHECK-NAME
      -1 RNL-IN-DEF !
      exit
   then
   RNL-I @ RNL-DATA-DEFINER? if RNL-I @ 1+ RNL-CHECK-NAME then ;

: RNL-SCAN-TOKEN ( -- )
   RNL-I @ RNL-WORD? 0= if exit then
   RNL-IN-DEF @ if RNL-HANDLE-IN-DEF else RNL-HANDLE-TOP then ;

: RNL-SCAN ( -- )
   0 RNL-I !
   0 RNL-IN-DEF !
   begin RNL-I @ L# @ < while
      RNL-SCAN-TOKEN
      RNL-I @ 1+ RNL-I !
   repeat ;

: RNL-ALLOC-NEED ( n -- n ) {: n :}
   n 0 <= if 1 exit then
   n ;

: RNL-ALLOC-SOURCE ( n -- )
   RNL-ALLOC-NEED MEM-ALLOC-64K-SPAN RNL-SRC-CAP ! RNL-SRC-A! ;

: RNL-LOAD-SOURCE ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu FILE-SIZE RNL-ALLOC-SOURCE
   path pathu RNL-SRC-A@ RNL-SRC-CAP @ READ-ALL RNL-SRC-U ! ;

: RESERVED-NAME-LINT-RESET ( -- )
   0 RNL-BAD !
   0 RNL-JSON !
   1 >FD RNL-OUT-FD! ;

: RESERVED-NAME-LINT-FILE-AS ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu label:ptr labelu :}
   label RNL-FILE-A! labelu RNL-FILE-U !
   path pathu RNL-LOAD-SOURCE
   RNL-SRC-A@ RNL-SRC-U @ LEX-SOURCE
   RNL-SCAN ;

: RESERVED-NAME-LINT-FILE ( ptr u8 n -- )
   2dup RESERVED-NAME-LINT-FILE-AS ;

: RESERVED-NAME-LINT-FINISH ( -- )
   RNL-BAD @ 0 > if 1 throw then ;
