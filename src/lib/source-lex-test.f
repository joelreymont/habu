\ source-lex-test.f - focused tests for src/lib/source-lex.f, package LINT-LEX.
\ Run: bin/hb --load src/lib/source-lex-test.f

require src/core/exit-status.f
require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/vector.f
require src/lib/source-lex.f
require src/lib/fmt.f                        \ FMT:.INT - one-line number text

package LINT-LEX-TEST
private

variable TEST-N
: ASSERT  ( bool -- )
   IF
      TEST-N @ 1+ TEST-N !
      exit
   THEN
   s" source-lex-test failed at assertion " type TEST-N @ FMT:.INT cr
   s" source-lex-test failed" EXIT-STATUS:FAILURE die ;
: ASSERT=  ( n n -- )  = ASSERT ;
: ASSERT$  ( str str -- )  STR:= ASSERT ;

$22 constant DQUOTE
\ The fixtures are vectors RUN makes and frees.
TYPED-VARIABLE LEX-B ^VEC:vector<u8>
TYPED-VARIABLE BAD-B ^VEC:vector<u8>
TYPED-VARIABLE ESC-B ^VEC:vector<u8>
TYPED-VARIABLE ROW-B ^VEC:vector<u8>
9000 constant BIG-LEX-TOKENS
TYPED-VARIABLE BIG-LEX-A ^u8
variable BIG-LEX-U

: BIG-LEX-A@ ( -- ^u8 )
   BIG-LEX-A @ ;

: BIG-LEX-A! ( ^u8 -- )
   BIG-LEX-A ! ;

: BIG-LEX$ ( -- str ) BIG-LEX-A@ BIG-LEX-U @ >str ;

: BIG-LEX-PUT ( n -- ) {: k :}
   120 BIG-LEX-A@ k 2 * + c!
   32 BIG-LEX-A@ k 2 * 1+ + c! ;

: FIX-NEW  ( -- ^VEC:vector<u8> )  HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;
: FIX-RESET  ( ^VEC:vector<u8> -- )  0 VEC:RESIZE ;
: FIX-C+  ( n ^VEC:vector<u8> -- )  VEC:PUSH c! ;
: FIX$  ( ^VEC:vector<u8> -- str )  VEC:BYTES ;

: LEX+  ( str -- )  LEX-B @ VEC:APPEND ;
: LEX-C+  ( n -- )  LEX-B @ FIX-C+ ;

: INIT-LEX-FIX  ( -- )
   LEX-B @ FIX-RESET
   s" : SQ ( n -- n ) s" LEX+  DQUOTE LEX-C+
   s"  hi : ; ( x )" LEX+  DQUOTE LEX-C+
   s"  dup " LEX+  92 LEX-C+
   s"  skip die" LEX+  10 LEX-C+  99 LEX-C+  DQUOTE LEX-C+
   s"  z" LEX+  DQUOTE LEX-C+
   s"  ;" LEX+  10 LEX-C+ ;
: LEX-FIX$  ( -- str )  LEX-B @ FIX$ ;

\ `: BAD s" nope` - the literal opened at byte 6 never closes. It lives in its
\ own buffer so the good fixture above is never clobbered and any test that
\ needs a malformed scan can establish one for itself.
: UNTERM-FIX$  ( -- str )
   BAD-B @ FIX-RESET
   s" : BAD s" BAD-B @ VEC:APPEND
   DQUOTE BAD-B @ FIX-C+
   s"  nope" BAD-B @ VEC:APPEND
   BAD-B @ FIX$ ;

: ESC-FIX-RESET  ( -- )  ESC-B @ FIX-RESET ;
: ESC-FIX+  ( str -- )  ESC-B @ VEC:APPEND ;
: ESC-FIX-C+  ( n -- )  ESC-B @ FIX-C+ ;
: ESC-FIX$  ( -- str )  ESC-B @ FIX$ ;

\ `: E S\" a\" b" dup ;` - the S\" opener honours backslash escapes, so the
\ escaped quote at byte 10 must NOT close the literal. Building it byte-wise
\ keeps the fixture out of this file's own string syntax.
: ESC-OPENER$  ( -- str )
   ESC-FIX-RESET
   s" : E S" ESC-FIX+  92 ESC-FIX-C+  DQUOTE ESC-FIX-C+
   s"  a" ESC-FIX+     92 ESC-FIX-C+  DQUOTE ESC-FIX-C+
   s"  b" ESC-FIX+     DQUOTE ESC-FIX-C+
   s"  dup ;" ESC-FIX+
   ESC-FIX$ ;

\ `: P s" a\" c ;` - the plain s" opener does NOT honour backslash escapes, so
\ the same byte pair closes the literal at byte 9.
: PLAIN-OPENER$  ( -- str )
   ESC-FIX-RESET
   s" : P s" ESC-FIX+  DQUOTE ESC-FIX-C+
   s"  a" ESC-FIX+     92 ESC-FIX-C+  DQUOTE ESC-FIX-C+
   s"  c ;" ESC-FIX+
   ESC-FIX$ ;

\ `: Q s" : FORGED ;" s" " dup ;` - two literals, the first holding text that
\ would be a definition if it were ever tokenized and the second holding
\ nothing at all.
: PAYLOAD-FIX$  ( -- str )
   ESC-FIX-RESET
   s" : Q s" ESC-FIX+  DQUOTE ESC-FIX-C+
   s"  : FORGED ;" ESC-FIX+  DQUOTE ESC-FIX-C+
   s"  s" ESC-FIX+  DQUOTE ESC-FIX-C+  $20 ESC-FIX-C+  DQUOTE ESC-FIX-C+
   s"  dup ;" ESC-FIX+
   ESC-FIX$ ;

\ Rows are built byte-wise: a primitive can be NAMED `s"`, `s\"` or `."`, and
\ writing those names as literals here would end this file's own strings.
$28 constant ROW-LPAREN-C

: ROW-RESET  ( -- )  ROW-B @ FIX-RESET ;
: ROW+  ( str -- )  ROW-B @ VEC:APPEND ;
: ROW-C+  ( n -- )  ROW-B @ FIX-C+ ;
: ROW-Q  ( -- )  DQUOTE ROW-C+ ;
: ROW-BS  ( -- )  92 ROW-C+ ;
: ROW-NL  ( -- )  10 ROW-C+ ;
: ROW$  ( -- str )  ROW-B @ FIX$ ;

\ Scan a fixture that is exactly one row plus nothing else, so the whole fixture
\ text is the expected REGISTRY token span.
: LEX-ROW  ( -- )  ROW$ LINT-LEX:SOURCE ;

variable REGN
variable REG-I
: REG-COUNT  ( -- n )
   0 REGN !  0 REG-I !
   begin REG-I @ LINT-LEX:COUNT < while
      REG-I @ LINT-LEX:KIND@ LINT-LEX:REGISTRY = IF REGN @ 1+ REGN ! THEN
      REG-I @ 1+ REG-I !
   repeat
   REGN @ ;

\ An exact length plus the opener and closer spelling pins the whole span: a row
\ that closed early or late cannot have the right byte count.
: ASSERT-ROW-SPAN  ( n n n -- ) {: k:n line:n len:n :}
   k LINT-LEX:KIND@ LINT-LEX:REGISTRY ASSERT=
   k LINT-LEX:LINE@ line ASSERT=
   k LINT-LEX:COL@ 1 ASSERT=
   k LINT-LEX:TOKEN STR:LEN len ASSERT= ;

: ASSERT-BARE-ROW  ( n n n -- ) {: k:n line:n len:n :}
   k line len ASSERT-ROW-SPAN
   k LINT-LEX:TOKEN s" EPRIM:" STR:STARTS-WITH? ASSERT
   k LINT-LEX:TOKEN s" EPRIM;" STR:ENDS-WITH? ASSERT ;

: ASSERT-PKG-ROW  ( n n n -- ) {: k:n line:n len:n :}
   k line len ASSERT-ROW-SPAN
   k LINT-LEX:TOKEN s" EPPRIM:" STR:STARTS-WITH? ASSERT
   k LINT-LEX:TOKEN s" ECLOSE-PRIVATE" STR:ENDS-WITH? ASSERT ;

: ASSERT-BAD-AT  ( n n n -- ) {: byte:n line:n col:n :}
   LINT-LEX:ERROR? ASSERT
   LINT-LEX:ERROR-KIND@ LINT-LEX:MALFORMED-REGISTRY ASSERT=
   LINT-LEX:ERROR-BYTE@ byte ASSERT=
   LINT-LEX:ERROR-LINE@ line ASSERT=
   LINT-LEX:ERROR-COL@ col ASSERT= ;

: ASSERT-ONE-ROW  ( -- )
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 1 ASSERT=
   0 LINT-LEX:KIND@ LINT-LEX:REGISTRY ASSERT=
   0 LINT-LEX:TOKEN ROW$ ASSERT$ ;

: INIT-BIG-LEX  ( -- )
   BIG-LEX-TOKENS 2 * {: cap:n :}
   cap MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BIG-LEX-A!
   cap BIG-LEX-U !
   0 begin dup BIG-LEX-TOKENS < while
      dup BIG-LEX-PUT
      1+
   repeat drop ;

: INIT-FIXTURES  ( -- )
   FIX-NEW LEX-B !  FIX-NEW BAD-B !  FIX-NEW ESC-B !  FIX-NEW ROW-B !
   INIT-LEX-FIX
   INIT-BIG-LEX ;

: FREE-FIXTURES  ( -- )
   LEX-B @ VEC:DELETE  BAD-B @ VEC:DELETE
   ESC-B @ VEC:DELETE  ROW-B @ VEC:DELETE ;

: TEST-LEXER  ( -- )
   LEX-FIX$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 7 ASSERT=
   0 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=    0 LINT-LEX:TOKEN s" :" ASSERT$
   0 LINT-LEX:BYTE@ 0 ASSERT=  0 LINT-LEX:LINE@ 1 ASSERT=  0 LINT-LEX:COL@ 1 ASSERT=
   1 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=    1 LINT-LEX:TOKEN s" SQ" ASSERT$
   1 LINT-LEX:BYTE@ 2 ASSERT=  1 LINT-LEX:LINE@ 1 ASSERT=  1 LINT-LEX:COL@ 3 ASSERT=
   2 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT= 2 LINT-LEX:CONTENT s"  n -- n " ASSERT$
   2 LINT-LEX:BYTE@ 5 ASSERT=  2 LINT-LEX:LINE@ 1 ASSERT=  2 LINT-LEX:COL@ 6 ASSERT=
   3 LINT-LEX:TOKEN STR:LEN 2 ASSERT=  3 LINT-LEX:TOKEN STR:DATA c@ 115 ASSERT=  3 LINT-LEX:TOKEN STR:DATA 1+ c@ DQUOTE ASSERT=
   3 LINT-LEX:BYTE@ 16 ASSERT=  3 LINT-LEX:LINE@ 1 ASSERT=  3 LINT-LEX:COL@ 17 ASSERT=
   4 LINT-LEX:TOKEN s" dup" ASSERT$
   4 LINT-LEX:BYTE@ 33 ASSERT=  4 LINT-LEX:LINE@ 1 ASSERT=  4 LINT-LEX:COL@ 34 ASSERT=
   5 LINT-LEX:TOKEN STR:LEN 2 ASSERT=  5 LINT-LEX:TOKEN STR:DATA c@ 99 ASSERT=  5 LINT-LEX:TOKEN STR:DATA 1+ c@ DQUOTE ASSERT=
   5 LINT-LEX:BYTE@ 48 ASSERT=  5 LINT-LEX:LINE@ 2 ASSERT=  5 LINT-LEX:COL@ 1 ASSERT=
   6 LINT-LEX:TOKEN s" ;" ASSERT$
   6 LINT-LEX:BYTE@ 54 ASSERT=  6 LINT-LEX:LINE@ 2 ASSERT=  6 LINT-LEX:COL@ 7 ASSERT= ;

\ A left parenthesis attached to the rest of a token is a Forth name, while a
\ standalone parenthesis still opens an inert comment.  The fake definers in
\ the trailing comment prove they never reach a structural lint consumer.
: TEST-LEXER-PAREN-NAME ( -- )
   s" : (CMP) ( n -- n ) dup ; ( : FORGED ; TRUSTED: BAD ; )" LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 6 ASSERT=
   0 LINT-LEX:TOKEN s" :" ASSERT$
   1 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=
   1 LINT-LEX:TOKEN s" (CMP)" ASSERT$
   2 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT=
   2 LINT-LEX:CONTENT s"  n -- n " ASSERT$
   3 LINT-LEX:TOKEN s" dup" ASSERT$
   4 LINT-LEX:TOKEN s" ;" ASSERT$
   5 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT=
   5 LINT-LEX:CONTENT s"  : FORGED ; TRUSTED: BAD ; " ASSERT$
   s" (" LINT-LEX:SOURCE
   LINT-LEX:COUNT 1 ASSERT=
   0 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT=
   0 LINT-LEX:CONTENT STR:LEN 0 ASSERT= ;

\ A parenthesized name lexes as one WORD wherever it stands, not only where a
\ definer just parsed it. In call position nothing precedes it, so only the
\ attached-paren rule itself keeps `(X)` out of the comment path.
: TEST-LEXER-PAREN-CALL ( -- )
   s" : USE (CMP) (X) 2drop ;" LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 6 ASSERT=
   0 LINT-LEX:TOKEN s" :" ASSERT$
   1 LINT-LEX:TOKEN s" USE" ASSERT$
   2 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=   2 LINT-LEX:TOKEN s" (CMP)" ASSERT$
   3 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=   3 LINT-LEX:TOKEN s" (X)" ASSERT$
   4 LINT-LEX:TOKEN s" 2drop" ASSERT$
   5 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ `.( ... )` is the printing comment. Its body is text the engine prints, never
\ code, so the whole span is one inert COMMENT token and the declaration spelled
\ inside it must not reach a consumer. `.(X)` has no delimiter after the opener,
\ so `parse-name` returns one ordinary word instead, and an opener that never
\ closes ends at end of input.
: TEST-LEXER-PRINT-PAREN ( -- )
   s" .( -9001 constant E-XA )  -9001 constant E-XB" LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 4 ASSERT=
   0 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT=
   0 LINT-LEX:TOKEN s" .( -9001 constant E-XA )" ASSERT$
   0 LINT-LEX:CONTENT s"  -9001 constant E-XA " ASSERT$
   0 LINT-LEX:BYTE@ 0 ASSERT=  0 LINT-LEX:LINE@ 1 ASSERT=  0 LINT-LEX:COL@ 1 ASSERT=
   1 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=   1 LINT-LEX:TOKEN s" -9001" ASSERT$
   2 LINT-LEX:TOKEN s" constant" ASSERT$
   3 LINT-LEX:TOKEN s" E-XB" ASSERT$
   s" .(X) drop" LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 2 ASSERT=
   0 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=   0 LINT-LEX:TOKEN s" .(X)" ASSERT$
   1 LINT-LEX:TOKEN s" drop" ASSERT$
   s" dup .(" LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 2 ASSERT=
   0 LINT-LEX:TOKEN s" dup" ASSERT$
   1 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT=
   1 LINT-LEX:TOKEN s" .(" ASSERT$
   1 LINT-LEX:CONTENT STR:LEN 0 ASSERT= ;

\ After a definer the engine parses the next word as a name and never executes
\ it, so `: .( ( -- ) cr ;` DEFINES a word spelled `.(` and the `( -- )` after it
\ is that definition's own stack comment. The later `.( hi )` still prints.
: TEST-LEXER-PRINT-NAME-POS ( -- )
   s" : .( ( -- ) cr ; .( hi ) dup" LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 7 ASSERT=
   0 LINT-LEX:TOKEN s" :" ASSERT$
   1 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=    1 LINT-LEX:TOKEN s" .(" ASSERT$
   2 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT= 2 LINT-LEX:CONTENT s"  -- " ASSERT$
   3 LINT-LEX:TOKEN s" cr" ASSERT$
   4 LINT-LEX:TOKEN s" ;" ASSERT$
   5 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT= 5 LINT-LEX:CONTENT s"  hi " ASSERT$
   6 LINT-LEX:TOKEN s" dup" ASSERT$ ;

\ An operand is data and names nothing: after `' :` the `:` defines nothing, so
\ the `.(` after it opens a printing comment.
: TEST-LEXER-OPERAND-NAMES-NOTHING ( -- )
   s" ' : .( z ) x" LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 4 ASSERT=
   1 LINT-LEX:TOKEN s" :" ASSERT$
   2 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT= 2 LINT-LEX:CONTENT s"  z " ASSERT$
   3 LINT-LEX:TOKEN s" x" ASSERT$ ;

\ A parsing keyword the engine runs takes the next whitespace-delimited token raw,
\ whatever it spells: `char \` is one word and hides nothing after it, and
\ `['] (` opens no comment. Named after a definer, `char` is a name and takes
\ nothing, so the `( -- n )` after it is that definition's stack comment.
: TEST-LEXER-PARSER-OPERAND ( -- )
   s" : char ( -- n ) 7 ; char \ x ['] ( y" LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 11 ASSERT=
   1 LINT-LEX:TOKEN s" char" ASSERT$
   2 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT= 2 LINT-LEX:CONTENT s"  -- n " ASSERT$
   5 LINT-LEX:TOKEN s" char" ASSERT$
   6 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=    6 LINT-LEX:TOKEN s" \" ASSERT$
   7 LINT-LEX:TOKEN s" x" ASSERT$
   9 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=    9 LINT-LEX:TOKEN s" (" ASSERT$
   10 LINT-LEX:TOKEN s" y" ASSERT$ ;

\ A body looks a token up among its live locals before the parsing keywords and
\ the string openers, byte for byte, as the engine and the checker do, so a
\ local named `char`, `[']` or `s"` takes no operand and opens nothing, and the
\ `;` after them ends the definition. A `{: … :}` group reads its names raw, each
\ a marked operand, and its closer is a plain word. After the `;` the `char` is
\ the top-level keyword again. A group that never closes ends with the input.
: TEST-LEXER-LOCAL-NAMES ( -- )
   ROW-RESET
   s" : F ( n -- n ) {: char ['] s" ROW+ ROW-Q
   s"  :} char ['] s" ROW+ ROW-Q
   s"  ; char X" ROW+
   ROW$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 14 ASSERT=
   3 LINT-LEX:TOKEN s" {:" ASSERT$        3 LINT-LEX:OPERAND? 0= ASSERT
   4 LINT-LEX:TOKEN s" char" ASSERT$      4 LINT-LEX:OPERAND? ASSERT
   5 LINT-LEX:TOKEN s" [']" ASSERT$       5 LINT-LEX:OPERAND? ASSERT
   6 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT= 6 LINT-LEX:CONTENT STR:LEN 0 ASSERT=
   6 LINT-LEX:OPERAND? ASSERT
   7 LINT-LEX:TOKEN s" :}" ASSERT$        7 LINT-LEX:OPERAND? 0= ASSERT
   8 LINT-LEX:TOKEN s" char" ASSERT$      8 LINT-LEX:OPERAND? 0= ASSERT
   9 LINT-LEX:TOKEN s" [']" ASSERT$       9 LINT-LEX:OPERAND? 0= ASSERT
   10 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT= 10 LINT-LEX:CONTENT STR:LEN 0 ASSERT=
   10 LINT-LEX:OPERAND? 0= ASSERT
   11 LINT-LEX:TOKEN s" ;" ASSERT$        11 LINT-LEX:OPERAND? 0= ASSERT
   13 LINT-LEX:TOKEN s" X" ASSERT$        13 LINT-LEX:OPERAND? ASSERT
   s" : U {: a b" LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 5 ASSERT=
   4 LINT-LEX:TOKEN s" b" ASSERT$         4 LINT-LEX:OPERAND? ASSERT ;

\ A local lives to its block's end, inside a quotation too, and `else` drops the
\ true arm's. A body `char` or `CHAR` takes nothing, whichever opener began it.
: TEST-LEXER-LOCAL-SCOPE ( -- )
   s" : G ( n -- ) {: [char] :} if {: char :} char else char A then [: [char] ;] [char] ; char B"
   LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 22 ASSERT=
   10 LINT-LEX:TOKEN s" char" ASSERT$     10 LINT-LEX:OPERAND? 0= ASSERT
   11 LINT-LEX:TOKEN s" else" ASSERT$
   13 LINT-LEX:TOKEN s" A" ASSERT$        13 LINT-LEX:OPERAND? 0= ASSERT
   16 LINT-LEX:TOKEN s" [char]" ASSERT$   16 LINT-LEX:OPERAND? 0= ASSERT
   17 LINT-LEX:TOKEN s" ;]" ASSERT$       17 LINT-LEX:OPERAND? 0= ASSERT
   19 LINT-LEX:TOKEN s" ;" ASSERT$        19 LINT-LEX:OPERAND? 0= ASSERT
   21 LINT-LEX:TOKEN s" B" ASSERT$        21 LINT-LEX:OPERAND? ASSERT
   s" : H ( n -- n ) {: char :} CHAR ; char ; ' x" LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 12 ASSERT=
   7 LINT-LEX:TOKEN s" ;" ASSERT$         7 LINT-LEX:OPERAND? 0= ASSERT
   8 LINT-LEX:TOKEN s" char" ASSERT$      8 LINT-LEX:OPERAND? 0= ASSERT
   9 LINT-LEX:OPERAND? ASSERT
   11 LINT-LEX:TOKEN s" x" ASSERT$        11 LINT-LEX:OPERAND? ASSERT
   s\" TRUSTED: .( ( -- n ) char \\ x\n; kernel: .( ( -- n ) char \\ y\n;"
   LINT-LEX:SOURCE
   LINT-LEX:COUNT 10 ASSERT=
   1 LINT-LEX:TOKEN s" .(" ASSERT$        2 LINT-LEX:CONTENT s"  -- n " ASSERT$
   4 LINT-LEX:TOKEN s" ;" ASSERT$
   6 LINT-LEX:TOKEN s" .(" ASSERT$        7 LINT-LEX:CONTENT s"  -- n " ASSERT$
   9 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ OPERAND scans the rest of the source again from its token with the locals that
\ are live there: the `char` after the operand it marks is still the local. A
\ `{:` taken as an operand opens no group, so the word after it is no name.
: TEST-LEXER-LOCAL-RESCAN ( -- )
   s" : P ( n -- n ) {: char :} create char char ;" LINT-LEX:SOURCE
   6 LINT-LEX:OPERAND
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 10 ASSERT=
   7 LINT-LEX:TOKEN s" char" ASSERT$      7 LINT-LEX:OPERAND? ASSERT
   8 LINT-LEX:TOKEN s" char" ASSERT$      8 LINT-LEX:OPERAND? 0= ASSERT
   9 LINT-LEX:TOKEN s" ;" ASSERT$         9 LINT-LEX:OPERAND? 0= ASSERT
   s" create {: x :}" LINT-LEX:SOURCE
   0 LINT-LEX:OPERAND
   LINT-LEX:COUNT 4 ASSERT=
   1 LINT-LEX:TOKEN s" {:" ASSERT$        1 LINT-LEX:OPERAND? ASSERT
   2 LINT-LEX:TOKEN s" x" ASSERT$         2 LINT-LEX:OPERAND? 0= ASSERT ;

: TEST-ONE-ENGINE-DELIM ( n -- ) {: c:n :}
   ROW-RESET
   s" LEFT" ROW+  c ROW-C+
   ROW-LPAREN-C ROW-C+  c ROW-C+  s" hidden )" ROW+
   c ROW-C+  s" RIGHT" ROW+
   LEX-ROW
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 3 ASSERT=
   0 LINT-LEX:TOKEN s" LEFT" ASSERT$
   1 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT=
   1 LINT-LEX:CONTENT STR:DATA c@ c ASSERT=
   2 LINT-LEX:TOKEN s" RIGHT" ASSERT$ ;

: TEST-DELIM-RANGE ( n n -- ) {: lo:n hi:n :}
   lo begin dup hi < while
      dup TEST-ONE-ENGINE-DELIM
      1+
   repeat drop ;

\ Every engine-only control delimiter must separate top-level words and make a
\ following parenthesis a standalone comment opener.
: TEST-LEXER-ENGINE-DELIMS ( -- )
   0 $09 TEST-DELIM-RANGE
   $0B $0D TEST-DELIM-RANGE
   $0E $20 TEST-DELIM-RANGE ;

\ A clean scan must leave the generic diagnostic record at its cleared state, so
\ ERROR-KIND@ alone distinguishes "no diagnostic" from a real one.
: TEST-LEXER-NO-ERROR ( -- )
   LEX-FIX$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:ERROR-KIND@ LINT-LEX:BAD-STRING <> ASSERT
   LINT-LEX:ERROR-KIND@ 0 ASSERT=
   LINT-LEX:ERROR-BYTE@ 0 ASSERT=
   LINT-LEX:ERROR-LINE@ 0 ASSERT=
   LINT-LEX:ERROR-COL@ 0 ASSERT= ;

: TEST-LEXER-UNTERM-QUOTE ( -- )
   UNTERM-FIX$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? ASSERT
   LINT-LEX:ERROR-KIND@ LINT-LEX:BAD-STRING ASSERT=
   LINT-LEX:ERROR-BYTE@ 6 ASSERT=
   LINT-LEX:ERROR-LINE@ 1 ASSERT=
   LINT-LEX:ERROR-COL@ 7 ASSERT= ;

\ SOURCE clears prior state before every scan, so a good file scanned after a
\ malformed one reports no diagnostic and a full, exact token table. The
\ malformed scan is established here rather than inherited from a sibling test,
\ so reordering RUN cannot quietly turn the cleared-state pins into no-ops.
: TEST-LEXER-REUSE-AFTER-ERROR ( -- )
   UNTERM-FIX$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? ASSERT
   LEX-FIX$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:ERROR-KIND@ 0 ASSERT=
   LINT-LEX:ERROR-BYTE@ 0 ASSERT=
   LINT-LEX:ERROR-LINE@ 0 ASSERT=
   LINT-LEX:ERROR-COL@ 0 ASSERT=
   LINT-LEX:COUNT 7 ASSERT=
   0 LINT-LEX:TOKEN s" :" ASSERT$
   6 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ RELEASE unmaps the tables, and runs again with none mapped; the next SOURCE
\ maps them again and answers the same tokens.
: TEST-LEXER-RELEASE ( -- )
   LEX-FIX$ LINT-LEX:SOURCE
   LINT-LEX:RELEASE
   LINT-LEX:COUNT 0 ASSERT=
   LINT-LEX:RELEASE
   LEX-FIX$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 7 ASSERT=
   0 LINT-LEX:TOKEN s" :" ASSERT$
   6 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ Escaped-quote spans: the S\" opener treats \" as literal text, the plain s"
\ opener does not. The token that follows each literal pins where it closed, so
\ neither can be satisfied by counting quotes.
: TEST-LEXER-ESC-QUOTE ( -- )
   ESC-OPENER$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 5 ASSERT=
   2 LINT-LEX:TOKEN STR:LEN 3 ASSERT=
   2 LINT-LEX:BYTE@ 4 ASSERT=  2 LINT-LEX:COL@ 5 ASSERT=
   3 LINT-LEX:TOKEN s" dup" ASSERT$
   3 LINT-LEX:BYTE@ 15 ASSERT=  3 LINT-LEX:LINE@ 1 ASSERT=  3 LINT-LEX:COL@ 16 ASSERT=
   4 LINT-LEX:TOKEN s" ;" ASSERT$
   4 LINT-LEX:BYTE@ 19 ASSERT=
   PLAIN-OPENER$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 5 ASSERT=
   2 LINT-LEX:TOKEN STR:LEN 2 ASSERT=
   2 LINT-LEX:BYTE@ 4 ASSERT=
   3 LINT-LEX:TOKEN s" c" ASSERT$
   3 LINT-LEX:BYTE@ 11 ASSERT=  3 LINT-LEX:COL@ 12 ASSERT=
   4 LINT-LEX:TOKEN s" ;" ASSERT$
   4 LINT-LEX:BYTE@ 13 ASSERT= ;

\ A string literal's payload is reported through CONTENT, the same reader a
\ paren comment's body uses, and it is still not tokenized: the token count and
\ the token after each literal pin that. Reporting the bytes is what lets a
\ consumer reason about a quoted NAME - the checker's concrete type table is
\ written that way - without falling back to substring search over the source.
: TEST-LEXER-STRING-PAYLOAD ( -- )
   PAYLOAD-FIX$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 6 ASSERT=
   2 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=
   2 LINT-LEX:CONTENT s" : FORGED ;" ASSERT$
   3 LINT-LEX:CONTENT STR:LEN 0 ASSERT=
   4 LINT-LEX:TOKEN s" dup" ASSERT$
   4 LINT-LEX:CONTENT STR:LEN 0 ASSERT=
   0 LINT-LEX:CONTENT STR:LEN 0 ASSERT=
   ESC-OPENER$ LINT-LEX:SOURCE
   2 LINT-LEX:CONTENT STR:LEN 5 ASSERT=
   2 LINT-LEX:CONTENT STR:DATA c@ [char] a ASSERT=
   PLAIN-OPENER$ LINT-LEX:SOURCE
   2 LINT-LEX:CONTENT STR:LEN 2 ASSERT=
   UNTERM-FIX$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? ASSERT
   2 LINT-LEX:CONTENT STR:LEN 0 ASSERT= ;

\ src/core/checker.f names primitives `s"`, `c"`, `."`, `s\"`, `c\"`, `.\"`, `[']`
\ and `[char]`. The engine reads a row name with `parse-name`, so none of those
\ names opens a string; a word-at-a-time lexer that thinks otherwise eats real
\ source. Each row form gets all eight names, and the definition after the last
\ row proves the scanner handed the source back.
: EIGHT-BARE-ROWS$  ( -- str )
   ROW-RESET
   s" EPRIM: s" ROW+       ROW-Q            s"  PE-N EPRIM;" ROW+  ROW-NL
   s" EPRIM: c" ROW+       ROW-Q            s"  PE-N EPRIM;" ROW+  ROW-NL
   s" EPRIM: ." ROW+       ROW-Q            s"  PE-N EPRIM;" ROW+  ROW-NL
   s" EPRIM: s" ROW+  ROW-BS ROW-Q          s"  PE-N EPRIM;" ROW+  ROW-NL
   s" EPRIM: c" ROW+  ROW-BS ROW-Q          s"  PE-N EPRIM;" ROW+  ROW-NL
   s" EPRIM: ." ROW+  ROW-BS ROW-Q          s"  PE-N EPRIM;" ROW+  ROW-NL
   s" EPRIM: ['] PE-N EPRIM;" ROW+                               ROW-NL
   s" EPRIM: [char] PE-N EPRIM;" ROW+                            ROW-NL
   s" : AFTER dup ;" ROW+
   ROW$ ;

: EIGHT-PKG-ROWS$  ( -- str )
   ROW-RESET
   s" EPPRIM: PK s" ROW+       ROW-Q     s"  PE-N ECLOSE-PRIVATE" ROW+  ROW-NL
   s" EPPRIM: PK c" ROW+       ROW-Q     s"  PE-N ECLOSE-PRIVATE" ROW+  ROW-NL
   s" EPPRIM: PK ." ROW+       ROW-Q     s"  PE-N ECLOSE-PRIVATE" ROW+  ROW-NL
   s" EPPRIM: PK s" ROW+  ROW-BS ROW-Q   s"  PE-N ECLOSE-PRIVATE" ROW+  ROW-NL
   s" EPPRIM: PK c" ROW+  ROW-BS ROW-Q   s"  PE-N ECLOSE-PRIVATE" ROW+  ROW-NL
   s" EPPRIM: PK ." ROW+  ROW-BS ROW-Q   s"  PE-N ECLOSE-PRIVATE" ROW+  ROW-NL
   s" EPPRIM: PK ['] PE-N ECLOSE-PRIVATE" ROW+                    ROW-NL
   s" EPPRIM: PK [char] PE-N ECLOSE-PRIVATE" ROW+                 ROW-NL
   s" : AFTER drop ;" ROW+
   ROW$ ;

: TEST-ROW-QUOTE-NAMES ( -- )
   EIGHT-BARE-ROWS$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 12 ASSERT=
   REG-COUNT 8 ASSERT=
   0 1 21 ASSERT-BARE-ROW   1 2 21 ASSERT-BARE-ROW   2 3 21 ASSERT-BARE-ROW
   3 4 22 ASSERT-BARE-ROW   4 5 22 ASSERT-BARE-ROW   5 6 22 ASSERT-BARE-ROW
   6 7 22 ASSERT-BARE-ROW   7 8 25 ASSERT-BARE-ROW
   8 LINT-LEX:TOKEN s" :" ASSERT$      8 LINT-LEX:LINE@ 9 ASSERT=
   9 LINT-LEX:TOKEN s" AFTER" ASSERT$
   10 LINT-LEX:TOKEN s" dup" ASSERT$
   11 LINT-LEX:TOKEN s" ;" ASSERT$
   EIGHT-PKG-ROWS$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 12 ASSERT=
   REG-COUNT 8 ASSERT=
   0 1 33 ASSERT-PKG-ROW    1 2 33 ASSERT-PKG-ROW    2 3 33 ASSERT-PKG-ROW
   3 4 34 ASSERT-PKG-ROW    4 5 34 ASSERT-PKG-ROW    5 6 34 ASSERT-PKG-ROW
   6 7 34 ASSERT-PKG-ROW    7 8 37 ASSERT-PKG-ROW
   8 LINT-LEX:TOKEN s" :" ASSERT$      8 LINT-LEX:LINE@ 9 ASSERT=
   9 LINT-LEX:TOKEN s" AFTER" ASSERT$
   11 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ Openers and closers spelled in a top-level comment or string body never reach
\ the row scanner at all, so no registry token appears and the paren comment
\ stays one COMMENT token.
: FAKE-ROWS$  ( -- str )
   ROW-RESET
   ROW-BS  s"  EPRIM: FAKE EPRIM;" ROW+  ROW-NL
   s" ( EPPRIM: PK FAKE2 ECLOSE-PRIVATE )" ROW+  ROW-NL
   s" : HOLDER s" ROW+  ROW-Q  s"  EPRIM: FAKE3 EPRIM;" ROW+  ROW-Q
   s"  drop ;" ROW+
   ROW$ ;

: TEST-ROW-FAKE-IN-COMMENT-AND-STRING ( -- )
   FAKE-ROWS$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   REG-COUNT 0 ASSERT=
   LINT-LEX:COUNT 6 ASSERT=
   0 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT=
   0 LINT-LEX:LINE@ 2 ASSERT=
   1 LINT-LEX:TOKEN s" :" ASSERT$
   2 LINT-LEX:TOKEN s" HOLDER" ASSERT$
   4 LINT-LEX:TOKEN s" drop" ASSERT$
   5 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ Forth is case-insensitive, so the engine executes every spelling below.
: TEST-ROW-CASE-FOLD ( -- )
   ROW-RESET  s" eprim: foo PE-N EpRiM;" ROW+
   LEX-ROW  ASSERT-ONE-ROW
   ROW-RESET  s" EpPrIm: pk foo PE-N EcLoSe-PrIvAtE" ROW+
   LEX-ROW  ASSERT-ONE-ROW
   ROW-RESET  s" EPPRIM: PK FOO PE-N eclose-private" ROW+
   LEX-ROW  ASSERT-ONE-ROW ;

\ A header field names the primitive, so an opener or a closer standing there
\ means the header is missing. Every case reports the header field it rejects,
\ and the scan stops with no token from the row or after it.
: TEST-ROW-BAD-HEADER ( -- )
   ROW-RESET  s" EPRIM: EPRIM: PE-N EPRIM;" ROW+
   LEX-ROW  7 1 8 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" EPRIM: EPRIM;" ROW+
   LEX-ROW  7 1 8 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" EPPRIM: ECLOSE-PRIVATE FOO PE-N ECLOSE-PRIVATE" ROW+
   LEX-ROW  8 1 9 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" EPPRIM: PK ECLOSE-PRIVATE" ROW+
   LEX-ROW  11 1 12 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT= ;

\ The diagnostic names the token the rule rejects and the row's opener; the
\ tokens before the row survive while nothing at or after it is exposed.
: LATE-BAD-ROW$  ( -- str )
   ROW-RESET
   s" : FIRST dup ;" ROW+  ROW-NL
   s"    EPRIM: EPRIM;" ROW+  ROW-NL
   s" : NEVER-SEEN drop ;" ROW+
   ROW$ ;

: TEST-ROW-DIAGNOSTIC-SPAN ( -- )
   LATE-BAD-ROW$ LINT-LEX:SOURCE
   24 2 11 ASSERT-BAD-AT
   LINT-LEX:ROW-LINE@ 2 ASSERT=  LINT-LEX:ROW-COL@ 4 ASSERT=
   LINT-LEX:COUNT 4 ASSERT=
   0 LINT-LEX:TOKEN s" :" ASSERT$
   1 LINT-LEX:TOKEN s" FIRST" ASSERT$
   3 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ After one of these the engine consumes the next word as a parsed name and never
\ executes it, so `: EPRIM: ( -- )` declares the opener instead of opening a
\ row, which would swallow the source up to the closer.
: DEFINER-POSITION$  ( -- str )
   ROW-RESET
   s" : EPRIM: dup ;" ROW+  ROW-NL
   s" : EPPRIM: drop ;" ROW+  ROW-NL
   s" ' EPRIM: : A drop ;" ROW+  ROW-NL
   s" : C undefine EPRIM: ;" ROW+  ROW-NL
   s" : D ['] EPRIM: drop ;" ROW+  ROW-NL
   s" : LAST over ;" ROW+
   ROW$ ;

: TEST-ROW-DEFINER-POSITION ( -- )
   DEFINER-POSITION$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   REG-COUNT 0 ASSERT=
   LINT-LEX:COUNT 29 ASSERT=
   1 LINT-LEX:TOKEN s" EPRIM:" ASSERT$   1 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=
   2 LINT-LEX:TOKEN s" dup" ASSERT$
   5 LINT-LEX:TOKEN s" EPPRIM:" ASSERT$
   25 LINT-LEX:TOKEN s" :" ASSERT$
   26 LINT-LEX:TOKEN s" LAST" ASSERT$
   27 LINT-LEX:TOKEN s" over" ASSERT$
   28 LINT-LEX:TOKEN s" ;" ASSERT$ ;

: TEST-ROW-REUSE-AFTER-ERROR ( -- )
   ROW-RESET  s" EPRIM: FOO PE-N PE-IN" ROW+
   LEX-ROW  21 1 22 ASSERT-BAD-AT
   EIGHT-BARE-ROWS$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:ERROR-KIND@ 0 ASSERT=
   LINT-LEX:ERROR-BYTE@ 0 ASSERT=
   LINT-LEX:ERROR-LINE@ 0 ASSERT=
   LINT-LEX:ERROR-COL@ 0 ASSERT=
   LINT-LEX:COUNT 12 ASSERT=
   REG-COUNT 8 ASSERT=
   11 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ The engine's table (src/habu/prims.f) writes `EPRIM: name ... EPRIM;` and
\ `EPPRIM: pkg name ... ECLOSE-PRIVATE`, and its opener parses the name, so a
\ row named `create` or `:` is a row: neither name defines the closer after it.
\ The definition after the rows proves the scanner handed the source back.
: DEFINER-NAMED-ROWS$  ( -- str )
   ROW-RESET
   s" EPRIM: create EPRIM;" ROW+  ROW-NL
   s" EPRIM: : EPRIM;" ROW+  ROW-NL
   s" EPPRIM: PK create ECLOSE-PRIVATE" ROW+  ROW-NL
   s" EPPRIM: PK : PE-N PE-OUT ECLOSE-PRIVATE" ROW+  ROW-NL
   s" : AFTER dup ;" ROW+
   ROW$ ;

: TEST-ROW-DEFINER-NAMES ( -- )
   DEFINER-NAMED-ROWS$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 8 ASSERT=
   REG-COUNT 4 ASSERT=
   0 1 20 ASSERT-ROW-SPAN  0 LINT-LEX:TOKEN s" EPRIM: create EPRIM;" ASSERT$
   1 2 15 ASSERT-ROW-SPAN  1 LINT-LEX:TOKEN s" EPRIM: : EPRIM;" ASSERT$
   2 3 32 ASSERT-ROW-SPAN
   2 LINT-LEX:TOKEN s" EPPRIM: PK create ECLOSE-PRIVATE" ASSERT$
   3 4 39 ASSERT-ROW-SPAN
   3 LINT-LEX:TOKEN s" EPPRIM: PK : PE-N PE-OUT ECLOSE-PRIVATE" ASSERT$
   4 LINT-LEX:TOKEN s" :" ASSERT$      4 LINT-LEX:LINE@ 5 ASSERT=
   5 LINT-LEX:TOKEN s" AFTER" ASSERT$
   7 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ A row the source ends inside is malformed at the source's end.
: TEST-ROW-UNCLOSED ( -- )
   ROW-RESET  s" EPRIM: FOO PE-N" ROW+
   LEX-ROW  15 1 16 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" EPPRIM: PK FOO PE-N" ROW+
   LEX-ROW  19 1 20 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" EPRIM:" ROW+
   LEX-ROW  6 1 7 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" EPPRIM: PK" ROW+
   LEX-ROW  10 1 11 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT= ;

\ A row ends at the first closer of either kind, whichever row it opened, and
\ the closer after it is a word of the source again.
: TEST-ROW-EITHER-CLOSER ( -- )
   ROW-RESET  s" EPRIM: FOO PE-N ECLOSE-PRIVATE EPRIM;" ROW+
   LEX-ROW
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 2 ASSERT=
   0 1 30 ASSERT-ROW-SPAN
   0 LINT-LEX:TOKEN s" EPRIM: FOO PE-N ECLOSE-PRIVATE" ASSERT$
   1 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=  1 LINT-LEX:TOKEN s" EPRIM;" ASSERT$
   ROW-RESET  s" EPPRIM: PK FOO PE-N EPRIM; ECLOSE-PRIVATE" ROW+
   LEX-ROW
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 2 ASSERT=
   0 1 26 ASSERT-ROW-SPAN
   0 LINT-LEX:TOKEN s" EPPRIM: PK FOO PE-N EPRIM;" ASSERT$
   1 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=
   1 LINT-LEX:TOKEN s" ECLOSE-PRIVATE" ASSERT$ ;

\ A REF clause is `REF` and the token after it, so a closer there is its
\ operand and the row closes at the second closer.
: TEST-ROW-REF-OPERAND ( -- )
   ROW-RESET  s" EPRIM: FOO PE-N REF EPRIM; EPRIM;" ROW+
   LEX-ROW  ASSERT-ONE-ROW ;

\ A row body holds only atoms and REF clauses: a parsing word there makes the
\ row malformed.
: TEST-ROW-PARSER-IN-BODY ( -- )
   ROW-RESET  s" EPRIM: FOO postpone EPRIM; PE-N EPRIM;" ROW+
   LEX-ROW  11 1 12 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT= ;

: TEST-BIG-LEXER  ( -- )
   BIG-LEX$ LINT-LEX:SOURCE
   LINT-LEX:COUNT BIG-LEX-TOKENS ASSERT=
   0 LINT-LEX:TOKEN s" x" ASSERT$
   8192 LINT-LEX:TOKEN s" x" ASSERT$
   BIG-LEX-TOKENS 1- LINT-LEX:TOKEN s" x" ASSERT$ ;

: RUN  ( -- )
   1 TEST-N !
   INIT-FIXTURES
   TEST-LEXER
   TEST-LEXER-PAREN-NAME
   TEST-LEXER-PAREN-CALL
   TEST-LEXER-PRINT-PAREN
   TEST-LEXER-PRINT-NAME-POS
   TEST-LEXER-OPERAND-NAMES-NOTHING
   TEST-LEXER-PARSER-OPERAND
   TEST-LEXER-LOCAL-NAMES
   TEST-LEXER-LOCAL-SCOPE
   TEST-LEXER-LOCAL-RESCAN
   TEST-LEXER-ENGINE-DELIMS
   TEST-LEXER-NO-ERROR
   TEST-LEXER-ESC-QUOTE
   TEST-LEXER-STRING-PAYLOAD
   TEST-LEXER-UNTERM-QUOTE
   TEST-LEXER-REUSE-AFTER-ERROR
   TEST-LEXER-RELEASE
   TEST-ROW-QUOTE-NAMES
   TEST-ROW-FAKE-IN-COMMENT-AND-STRING
   TEST-ROW-CASE-FOLD
   TEST-ROW-BAD-HEADER
   TEST-ROW-DIAGNOSTIC-SPAN
   TEST-ROW-DEFINER-POSITION
   TEST-ROW-REUSE-AFTER-ERROR
   TEST-ROW-DEFINER-NAMES
   TEST-ROW-UNCLOSED
   TEST-ROW-EITHER-CLOSER
   TEST-ROW-REF-OPERAND
   TEST-ROW-PARSER-IN-BODY
   TEST-BIG-LEXER
   FREE-FIXTURES
   s" source-lex-test: ok (" type TEST-N @ 1- FMT:.INT s"  assertions)" type cr ;

RUN

;package
