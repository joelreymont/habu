\ text-foundation-test.f - focused tests for tools/lint/text.f text helpers.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/vector.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/lint/source-lex.f tools/lint/text-foundation-test.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

\ typed STR:STR:BUF-LEN@ CAD-NUM:TFT-BL>RAW boundary: read each fixture buffer's length as a byte-len
\ role, then project it back to the raw n the ( -- ptr u8 n ) accessors return.
package CAD-NUM
public
: TFT-BL>RAW ( CAD-NUM:byte-len -- n ) BYTE-LEN>N ;
;package

package LINT-TEXT-TEST
private

variable TEST-N
: ASSERT  ( bool -- )
   IF
      TEST-N @ 1+ TEST-N !
      exit
   THEN
   s" text-foundation-test failed at assertion " type TEST-N @ . cr
   s" text-foundation-test failed" 1 die ;
: ASSERT=  ( n n -- )  = ASSERT ;
: ASSERT$  ( ptr u8 n ptr u8 n -- )  LINT-STR= ASSERT ;

$100 constant FIX-CAP
create STR-FIX FIX-CAP allot     variable STR-FIX-LEN
create TRUST-FIX FIX-CAP allot   variable TRUST-LEN
create SRC-FIX FIX-CAP allot     variable SRC-LEN
create BT-FIX FIX-CAP allot      variable BT-LEN
create LEX-FIX FIX-CAP allot     variable LEX-LEN
create BAD-FIX FIX-CAP allot     variable BAD-LEN
create ESC-FIX FIX-CAP allot     variable ESC-LEN
create TOK-FIX FIX-CAP allot     variable TOK-LEN
9000 constant BIG-LEX-TOKENS
variable BIG-LEX-A
variable BIG-LEX-U

: BIG-LEX-A-FIELD ( -- ptr ptr u8 )
   BIG-LEX-A 0 ptr-field ;

: BIG-LEX-A@ ( -- ptr u8 )
   BIG-LEX-A-FIELD @ ;

: BIG-LEX-A! ( ptr u8 -- )
   BIG-LEX-A-FIELD ! ;

: BIG-LEX$ ( -- ptr u8 n ) BIG-LEX-A@ BIG-LEX-U @ ;

: BIG-LEX-PUT ( n -- ) {: k :}
   120 BIG-LEX-A@ k 2 * + c!
   32 BIG-LEX-A@ k 2 * 1+ + c! ;

: INIT-STR-FIX  ( -- )
   STR-FIX-LEN STR:BUF-RESET
   32 STR-FIX FIX-CAP STR:LENGTH STR-FIX-LEN STR:BUF-APPEND-C
   32 STR-FIX FIX-CAP STR:LENGTH STR-FIX-LEN STR:BUF-APPEND-C
   s" Alpha beta" STR:LENGTH STR-FIX FIX-CAP STR:LENGTH STR-FIX-LEN STR:BUF-APPEND
   10 STR-FIX FIX-CAP STR:LENGTH STR-FIX-LEN STR:BUF-APPEND-C
   s" Gamma" STR:LENGTH STR-FIX FIX-CAP STR:LENGTH STR-FIX-LEN STR:BUF-APPEND
   32 STR-FIX FIX-CAP STR:LENGTH STR-FIX-LEN STR:BUF-APPEND-C
   32 STR-FIX FIX-CAP STR:LENGTH STR-FIX-LEN STR:BUF-APPEND-C
   10 STR-FIX FIX-CAP STR:LENGTH STR-FIX-LEN STR:BUF-APPEND-C ;
: STR-FIX$  ( -- ptr u8 n )  STR-FIX STR-FIX-LEN STR:BUF-LEN@ CAD-NUM:TFT-BL>RAW ;

: INIT-TRUST-FIX  ( -- )
   TRUST-LEN STR:BUF-RESET
   s" prefix s" STR:LENGTH TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND
   DQUOTE TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND-C
   32 TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND-C
   s" SQ" STR:LENGTH TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND
   DQUOTE TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND-C
   32 TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND-C
   115 TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND-C
   DQUOTE TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND-C
   32 TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND-C
   s" ( n -- n )" STR:LENGTH TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND
   DQUOTE TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND-C
   s"  TRUST \\ comment" STR:LENGTH TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND
   10 TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND-C ;
: TRUST-FIX$  ( -- ptr u8 n )  TRUST-FIX TRUST-LEN STR:BUF-LEN@ CAD-NUM:TFT-BL>RAW ;

: TRUST-FIX-RESET  ( -- )
   TRUST-LEN STR:BUF-RESET ;

: TRUST-FIX+  ( ptr u8 n -- )
   STR:LENGTH TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND ;

: TRUST-FIX-C+  ( n -- )
   TRUST-FIX FIX-CAP STR:LENGTH TRUST-LEN STR:BUF-APPEND-C ;

: TRUST-STRING-FALSE$  ( -- ptr u8 n )
   TRUST-FIX-RESET
   s" ." TRUST-FIX+
   DQUOTE TRUST-FIX-C+
   s"  s" TRUST-FIX+
   DQUOTE TRUST-FIX-C+
   s"  FAKE" TRUST-FIX+
   DQUOTE TRUST-FIX-C+
   s"  s" TRUST-FIX+
   DQUOTE TRUST-FIX-C+
   s"  --" TRUST-FIX+
   DQUOTE TRUST-FIX-C+
   s"  TRUST" TRUST-FIX+
   DQUOTE TRUST-FIX-C+
   s"  ;" TRUST-FIX+
   TRUST-FIX$ ;

: INIT-SRC-FIX  ( -- )
   SRC-LEN STR:BUF-RESET
   s" : REPL-SRC s" STR:LENGTH SRC-FIX FIX-CAP STR:LENGTH SRC-LEN STR:BUF-APPEND
   DQUOTE SRC-FIX FIX-CAP STR:LENGTH SRC-LEN STR:BUF-APPEND-C
   32 SRC-FIX FIX-CAP STR:LENGTH SRC-LEN STR:BUF-APPEND-C
   s" src/habu/repl.f" STR:LENGTH SRC-FIX FIX-CAP STR:LENGTH SRC-LEN STR:BUF-APPEND
   DQUOTE SRC-FIX FIX-CAP STR:LENGTH SRC-LEN STR:BUF-APPEND-C
   s"  ;" STR:LENGTH SRC-FIX FIX-CAP STR:LENGTH SRC-LEN STR:BUF-APPEND
   10 SRC-FIX FIX-CAP STR:LENGTH SRC-LEN STR:BUF-APPEND-C ;
: SRC-FIX$  ( -- ptr u8 n )  SRC-FIX SRC-LEN STR:BUF-LEN@ CAD-NUM:TFT-BL>RAW ;

: INIT-BT-FIX  ( -- )
   BT-LEN STR:BUF-RESET
   s" See `tools/lint/source-lex.f` and `plain`." STR:LENGTH BT-FIX FIX-CAP STR:LENGTH BT-LEN STR:BUF-APPEND
   10 BT-FIX FIX-CAP STR:LENGTH BT-LEN STR:BUF-APPEND-C ;
: BT-FIX$  ( -- ptr u8 n )  BT-FIX BT-LEN STR:BUF-LEN@ CAD-NUM:TFT-BL>RAW ;

: INIT-LEX-FIX  ( -- )
   LEX-LEN STR:BUF-RESET
   s" : SQ ( n -- n ) s" STR:LENGTH LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND
   DQUOTE LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND-C
   s"  hi : ; ( x )" STR:LENGTH LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND
   DQUOTE LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND-C
   s"  dup " STR:LENGTH LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND
   92 LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND-C
   s"  skip die" STR:LENGTH LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND
   10 LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND-C
   99 LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND-C
   DQUOTE LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND-C
   s"  z" STR:LENGTH LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND
   DQUOTE LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND-C
   s"  ;" STR:LENGTH LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND
   10 LEX-FIX FIX-CAP STR:LENGTH LEX-LEN STR:BUF-APPEND-C ;
: LEX-FIX$  ( -- ptr u8 n )  LEX-FIX LEX-LEN STR:BUF-LEN@ CAD-NUM:TFT-BL>RAW ;

\ `: BAD s" nope` - the literal opened at byte 6 never closes. It lives in its
\ own buffer so the good fixture above is never clobbered and any test that
\ needs a malformed scan can establish one for itself.
: UNTERM-FIX$  ( -- ptr u8 n )
   BAD-LEN STR:BUF-RESET
   s" : BAD s" STR:LENGTH BAD-FIX FIX-CAP STR:LENGTH BAD-LEN STR:BUF-APPEND
   DQUOTE BAD-FIX FIX-CAP STR:LENGTH BAD-LEN STR:BUF-APPEND-C
   s"  nope" STR:LENGTH BAD-FIX FIX-CAP STR:LENGTH BAD-LEN STR:BUF-APPEND
   BAD-FIX BAD-LEN STR:BUF-LEN@ CAD-NUM:TFT-BL>RAW ;

: ESC-FIX-RESET  ( -- )
   ESC-LEN STR:BUF-RESET ;

: ESC-FIX+  ( ptr u8 n -- )
   STR:LENGTH ESC-FIX FIX-CAP STR:LENGTH ESC-LEN STR:BUF-APPEND ;

: ESC-FIX-C+  ( n -- )
   ESC-FIX FIX-CAP STR:LENGTH ESC-LEN STR:BUF-APPEND-C ;

: ESC-FIX$  ( -- ptr u8 n )  ESC-FIX ESC-LEN STR:BUF-LEN@ CAD-NUM:TFT-BL>RAW ;

\ `: E S\" a\" b" dup ;` - the S\" opener honours backslash escapes, so the
\ escaped quote at byte 10 must NOT close the literal. Building it byte-wise
\ keeps the fixture out of this file's own string syntax.
: ESC-OPENER$  ( -- ptr u8 n )
   ESC-FIX-RESET
   s" : E S" ESC-FIX+  92 ESC-FIX-C+  DQUOTE ESC-FIX-C+
   s"  a" ESC-FIX+     92 ESC-FIX-C+  DQUOTE ESC-FIX-C+
   s"  b" ESC-FIX+     DQUOTE ESC-FIX-C+
   s"  dup ;" ESC-FIX+
   ESC-FIX$ ;

\ `: P s" a\" c ;` - the plain s" opener does NOT honour backslash escapes, so
\ the same byte pair closes the literal at byte 9.
: PLAIN-OPENER$  ( -- ptr u8 n )
   ESC-FIX-RESET
   s" : P s" ESC-FIX+  DQUOTE ESC-FIX-C+
   s"  a" ESC-FIX+     92 ESC-FIX-C+  DQUOTE ESC-FIX-C+
   s"  c ;" ESC-FIX+
   ESC-FIX$ ;

\ `: Q s" : FORGED ;" s" " dup ;` - two literals, the first holding text that
\ would be a definition if it were ever tokenized and the second holding
\ nothing at all.
: PAYLOAD-FIX$  ( -- ptr u8 n )
   ESC-FIX-RESET
   s" : Q s" ESC-FIX+  DQUOTE ESC-FIX-C+
   s"  : FORGED ;" ESC-FIX+  DQUOTE ESC-FIX-C+
   s"  s" ESC-FIX+  DQUOTE ESC-FIX-C+  $20 ESC-FIX-C+  DQUOTE ESC-FIX-C+
   s"  dup ;" ESC-FIX+
   ESC-FIX$ ;

: INIT-TOK-FIX  ( -- )
   TOK-LEN STR:BUF-RESET
   s" : X ( n -- n ) dup " STR:LENGTH TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND
   92 TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND-C
   s"  skip" STR:LENGTH TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND
   10 TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND-C
   s" : Y ;" STR:LENGTH TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND ;
: TOK-FIX$  ( -- ptr u8 n )  TOK-FIX TOK-LEN STR:BUF-LEN@ CAD-NUM:TFT-BL>RAW ;

\ ---- primitive-axiom row fixtures ------------------------------------------
\ Rows are built byte-wise for the same reason the escaped-quote fixtures are:
\ a primitive can be NAMED `s"`, `s\"` or `."`, and writing those names as
\ literals here would end this file's own strings. ROW-Q appends a double quote,
\ ROW-BS a backslash and ROW-NL a newline, so every fixture below is exact bytes
\ rather than something the reader has to escape in their head.
$400 constant ROW-CAP
$0B constant ROW-VT-C
$28 constant ROW-LPAREN-C
create ROW-FIX ROW-CAP allot     variable ROW-LEN

: ROW-RESET  ( -- )  ROW-LEN STR:BUF-RESET ;
: ROW+  ( ptr u8 n -- )  STR:LENGTH ROW-FIX ROW-CAP STR:LENGTH ROW-LEN STR:BUF-APPEND ;
: ROW-C+  ( n -- )  ROW-FIX ROW-CAP STR:LENGTH ROW-LEN STR:BUF-APPEND-C ;
: ROW-Q  ( -- )  DQUOTE ROW-C+ ;
: ROW-BS  ( -- )  92 ROW-C+ ;
: ROW-NL  ( -- )  10 ROW-C+ ;
: ROW$  ( -- ptr u8 n )  ROW-FIX ROW-LEN STR:BUF-LEN@ CAD-NUM:TFT-BL>RAW ;

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

\ Index of the first REGISTRY token whose text starts with the given prefix, or
\ -1. Used to name one exact row inside a real source file.
: REG-FIND  ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 REG-I !
   begin REG-I @ LINT-LEX:COUNT < while
      REG-I @ LINT-LEX:KIND@ LINT-LEX:REGISTRY =
      REG-I @ LINT-LEX:TOKEN a u LINT-STARTS-WITH? and IF REG-I @ exit THEN
      REG-I @ 1+ REG-I !
   repeat  -1 ;

\ An exact length plus the opener and closer spelling pins the whole span: a row
\ that closed early or late cannot have the right byte count.
: ASSERT-ROW-SPAN  ( n n n -- ) {: k:n line:n len:n :}
   k LINT-LEX:KIND@ LINT-LEX:REGISTRY ASSERT=
   k LINT-LEX:LINE@ line ASSERT=
   k LINT-LEX:COL@ 1 ASSERT=
   k LINT-LEX:TOKEN nip len ASSERT= ;

: ASSERT-BARE-ROW  ( n n n -- ) {: k:n line:n len:n :}
   k line len ASSERT-ROW-SPAN
   k LINT-LEX:TOKEN s" PRIM:" LINT-STARTS-WITH? ASSERT
   k LINT-LEX:TOKEN s" PRIM;" LINT-ENDS-WITH? ASSERT ;

: ASSERT-PKG-ROW  ( n n n -- ) {: k:n line:n len:n :}
   k line len ASSERT-ROW-SPAN
   k LINT-LEX:TOKEN s" PPRIM:" LINT-STARTS-WITH? ASSERT
   k LINT-LEX:TOKEN s" PPRIM;" LINT-ENDS-WITH? ASSERT ;

: ASSERT-BAD-AT  ( n n n -- ) {: byte:n line:n col:n :}
   LINT-LEX:ERROR? ASSERT
   LINT-LEX:ERROR-KIND@ LINT-LEX:MALFORMED-REGISTRY ASSERT=
   LINT-LEX:ERROR-BYTE@ byte ASSERT=
   LINT-LEX:ERROR-LINE@ line ASSERT=
   LINT-LEX:ERROR-COL@ col ASSERT= ;

: ASSERT-ONE-ROW  ( -- )   \ the whole fixture is one row and nothing else
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
   INIT-STR-FIX
   INIT-TRUST-FIX
   INIT-SRC-FIX
   INIT-BT-FIX
   INIT-LEX-FIX
   INIT-TOK-FIX
   INIT-BIG-LEX ;

: TEST-STRINGS  ( -- )
   STR-FIX$ SPLIT-LINES  SN# @ 2 ASSERT=
   0 S@ LINT-TRIM s" Alpha beta" ASSERT$
   1 S@ LINT-TRIM s" Gamma" ASSERT$
   STR-FIX$ SPLIT-WHITESPACE  SN# @ 3 ASSERT=
   0 S@ s" Alpha" ASSERT$  1 S@ s" beta" ASSERT$  2 S@ s" Gamma" ASSERT$
   s" hello.f" s" .f" HAS-EXT? ASSERT
   s" hello.fs" s" .zig" HAS-EXT? 0= ASSERT
   s" Habu" s" Ha" LINT-STARTS-WITH? ASSERT
   s" Habu" s" bu" LINT-ENDS-WITH? ASSERT
   s" banana" 97 LINT-COUNT-CHAR 3 ASSERT=
   s" banana" 110 LINT-INDEX-OF MATCH option
     none OF -1 ENDOF
     some OF ENDOF
   ;MATCH 2 ASSERT=
   s" banana" 122 LINT-INDEX-OF MATCH option
     none OF -1 ENDOF
     some OF drop -2 ENDOF
   ;MATCH -1 ASSERT=
   s" banana split" s" spl" LINT-FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF ENDOF
   ;MATCH 7 ASSERT=
   s" banana split" s" zz" LINT-FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF drop -2 ENDOF
   ;MATCH -1 ASSERT=
   s" banana" s" " LINT-FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF ENDOF
   ;MATCH 0 ASSERT=
   s" banana split" s" spl" LINT-CONTAINS? ASSERT
   s" banana split" s" zz" LINT-CONTAINS? 0= ASSERT ;

: TEST-SCANNERS  ( -- )
   TRUST-FIX$ TRUST-SITE? ASSERT
   P1A@ P1U @ s" SQ" ASSERT$
   P2A@ P2U @ s" ( n -- n )" ASSERT$
   s" TRUSTED: TRAW ( a n -- ) catch ;" TRUST-SITE? ASSERT
   P1A@ P1U @ s" TRAW" ASSERT$
   P2A@ P2U @ s" a n --" ASSERT$
   s" \ TRUSTED: FAKE ( -- )" TRUST-SITE? 0= ASSERT
   s" ( TRUSTED: FAKE ( -- ) )" TRUST-SITE? 0= ASSERT
   TRUST-STRING-FALSE$ TRUST-SITE? 0= ASSERT
   SRC-FIX$ SRC-PATH-REF? ASSERT
   P1A@ P1U @ s" src/habu/repl.f" ASSERT$
   BT-FIX$ BACKTICK-PATH? ASSERT
   P1A@ P1U @ s" tools/lint/source-lex.f" ASSERT$
   s" `plain`" BACKTICK-PATH? 0= ASSERT ;

: TEST-SIGS  ( -- )
   s"  n -- n  " SIG-KIND SIG-TYPED ASSERT=
   s" private infer" SIG-KIND SIG-OPTOUT ASSERT=
   s" i64 )" SIG-KIND SIG-MISSING ASSERT=
   s" : BAD ( i64 ) dup ;" LINT-LEX:SOURCE
   2 LINT-LEX:CONTENT SIG-KIND SIG-MISSING ASSERT= ;

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
   3 LINT-LEX:TOKEN nip 2 ASSERT=  3 LINT-LEX:TOKEN drop c@ 115 ASSERT=  3 LINT-LEX:TOKEN drop 1+ c@ DQUOTE ASSERT=
   3 LINT-LEX:BYTE@ 16 ASSERT=  3 LINT-LEX:LINE@ 1 ASSERT=  3 LINT-LEX:COL@ 17 ASSERT=
   4 LINT-LEX:TOKEN s" dup" ASSERT$
   4 LINT-LEX:BYTE@ 33 ASSERT=  4 LINT-LEX:LINE@ 1 ASSERT=  4 LINT-LEX:COL@ 34 ASSERT=
   5 LINT-LEX:TOKEN nip 2 ASSERT=  5 LINT-LEX:TOKEN drop c@ 99 ASSERT=  5 LINT-LEX:TOKEN drop 1+ c@ DQUOTE ASSERT=
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
   0 LINT-LEX:CONTENT nip 0 ASSERT= ;

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
\ inside it must not reach a consumer - the exact case tools/error-code-lint.f
\ depends on. `.(X)` has no delimiter after the opener, so `parse-name` returns
\ one ordinary word instead, and an opener that never closes ends at end of input.
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
   1 LINT-LEX:CONTENT nip 0 ASSERT= ;

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

: TEST-ONE-ENGINE-DELIM ( n -- ) {: c:n :}
   ROW-RESET
   s" LEFT" ROW+  c ROW-C+
   ROW-LPAREN-C ROW-C+  c ROW-C+  s" hidden )" ROW+
   c ROW-C+  s" RIGHT" ROW+
   LEX-ROW
   LINT-LEX:ERROR? LINT-NOT ASSERT
   LINT-LEX:COUNT 3 ASSERT=
   0 LINT-LEX:TOKEN s" LEFT" ASSERT$
   1 LINT-LEX:KIND@ LINT-LEX:COMMENT ASSERT=
   1 LINT-LEX:CONTENT drop c@ c ASSERT=
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
   LINT-LEX:ERROR-KIND@ LINT-LEX:UNTERMINATED-QUOTE <> ASSERT
   LINT-LEX:ERROR-KIND@ 0 ASSERT=
   LINT-LEX:ERROR-BYTE@ 0 ASSERT=
   LINT-LEX:ERROR-LINE@ 0 ASSERT=
   LINT-LEX:ERROR-COL@ 0 ASSERT= ;

: TEST-LEXER-UNTERM-QUOTE ( -- )
   UNTERM-FIX$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? ASSERT
   LINT-LEX:ERROR-KIND@ LINT-LEX:UNTERMINATED-QUOTE ASSERT=
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

\ Escaped-quote spans: the S\" opener treats \" as literal text, the plain s"
\ opener does not. The token that follows each literal pins where it closed, so
\ neither can be satisfied by counting quotes.
: TEST-LEXER-ESC-QUOTE ( -- )
   ESC-OPENER$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 5 ASSERT=
   2 LINT-LEX:TOKEN nip 3 ASSERT=
   2 LINT-LEX:BYTE@ 4 ASSERT=  2 LINT-LEX:COL@ 5 ASSERT=
   3 LINT-LEX:TOKEN s" dup" ASSERT$
   3 LINT-LEX:BYTE@ 15 ASSERT=  3 LINT-LEX:LINE@ 1 ASSERT=  3 LINT-LEX:COL@ 16 ASSERT=
   4 LINT-LEX:TOKEN s" ;" ASSERT$
   4 LINT-LEX:BYTE@ 19 ASSERT=
   PLAIN-OPENER$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 5 ASSERT=
   2 LINT-LEX:TOKEN nip 2 ASSERT=
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
   3 LINT-LEX:CONTENT nip 0 ASSERT=
   4 LINT-LEX:TOKEN s" dup" ASSERT$
   4 LINT-LEX:CONTENT nip 0 ASSERT=
   0 LINT-LEX:CONTENT nip 0 ASSERT=
   ESC-OPENER$ LINT-LEX:SOURCE
   2 LINT-LEX:CONTENT nip 5 ASSERT=
   2 LINT-LEX:CONTENT drop c@ [char] a ASSERT=
   PLAIN-OPENER$ LINT-LEX:SOURCE
   2 LINT-LEX:CONTENT nip 2 ASSERT=
   UNTERM-FIX$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? ASSERT
   2 LINT-LEX:CONTENT nip 0 ASSERT= ;

\ ---- primitive-axiom rows ---------------------------------------------------
\ src/core/checker.f names primitives `s"`, `c"`, `."`, `s\"`, `c\"`, `.\"`, `[']`
\ and `[char]`. The engine reads a row name with `parse-name`, so none of those
\ names opens a string; a word-at-a-time lexer that thinks otherwise eats real
\ source. Each family gets all eight names, and the definition after the last row
\ proves the scanner handed the source back.
: EIGHT-BARE-ROWS$  ( -- ptr u8 n )
   ROW-RESET
   s" PRIM: s" ROW+       ROW-Q            s"  A PRIM;" ROW+  ROW-NL
   s" PRIM: c" ROW+       ROW-Q            s"  B PRIM;" ROW+  ROW-NL
   s" PRIM: ." ROW+       ROW-Q            s"  C PRIM;" ROW+  ROW-NL
   s" PRIM: s" ROW+  ROW-BS ROW-Q          s"  D PRIM;" ROW+  ROW-NL
   s" PRIM: c" ROW+  ROW-BS ROW-Q          s"  E PRIM;" ROW+  ROW-NL
   s" PRIM: ." ROW+  ROW-BS ROW-Q          s"  F PRIM;" ROW+  ROW-NL
   s" PRIM: ['] G PRIM;" ROW+                                 ROW-NL
   s" PRIM: [char] H PRIM;" ROW+                              ROW-NL
   s" : AFTER dup ;" ROW+
   ROW$ ;

: EIGHT-PKG-ROWS$  ( -- ptr u8 n )
   ROW-RESET
   s" PPRIM: PK s" ROW+       ROW-Q         s"  A PPRIM;" ROW+  ROW-NL
   s" PPRIM: PK c" ROW+       ROW-Q         s"  B PPRIM;" ROW+  ROW-NL
   s" PPRIM: PK ." ROW+       ROW-Q         s"  C PPRIM;" ROW+  ROW-NL
   s" PPRIM: PK s" ROW+  ROW-BS ROW-Q       s"  D PPRIM;" ROW+  ROW-NL
   s" PPRIM: PK c" ROW+  ROW-BS ROW-Q       s"  E PPRIM;" ROW+  ROW-NL
   s" PPRIM: PK ." ROW+  ROW-BS ROW-Q       s"  F PPRIM;" ROW+  ROW-NL
   s" PPRIM: PK ['] G PPRIM;" ROW+                              ROW-NL
   s" PPRIM: PK [char] H PPRIM;" ROW+                           ROW-NL
   s" : AFTER drop ;" ROW+
   ROW$ ;

: TEST-ROW-QUOTE-NAMES ( -- )
   EIGHT-BARE-ROWS$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 12 ASSERT=
   REG-COUNT 8 ASSERT=
   0 1 16 ASSERT-BARE-ROW   1 2 16 ASSERT-BARE-ROW   2 3 16 ASSERT-BARE-ROW
   3 4 17 ASSERT-BARE-ROW   4 5 17 ASSERT-BARE-ROW   5 6 17 ASSERT-BARE-ROW
   6 7 17 ASSERT-BARE-ROW   7 8 20 ASSERT-BARE-ROW
   8 LINT-LEX:TOKEN s" :" ASSERT$      8 LINT-LEX:LINE@ 9 ASSERT=
   9 LINT-LEX:TOKEN s" AFTER" ASSERT$
   10 LINT-LEX:TOKEN s" dup" ASSERT$
   11 LINT-LEX:TOKEN s" ;" ASSERT$
   EIGHT-PKG-ROWS$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 12 ASSERT=
   REG-COUNT 8 ASSERT=
   0 1 21 ASSERT-PKG-ROW    1 2 21 ASSERT-PKG-ROW    2 3 21 ASSERT-PKG-ROW
   3 4 22 ASSERT-PKG-ROW    4 5 22 ASSERT-PKG-ROW    5 6 22 ASSERT-PKG-ROW
   6 7 22 ASSERT-PKG-ROW    7 8 25 ASSERT-PKG-ROW
   8 LINT-LEX:TOKEN s" :" ASSERT$      8 LINT-LEX:LINE@ 9 ASSERT=
   9 LINT-LEX:TOKEN s" AFTER" ASSERT$
   11 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ The differential probe from the row contract. The engine accepts this row: `s"`
\ in the body is executed, so its literal swallows the `PRIM;` and the `create
\ LEAK` inside it, and the row closes at the LAST closer. A scanner that read the
\ body as flat raw fields would close at the embedded closer and then report
\ `create LEAK y"` as live source.
: STRING-BODY-ROW$  ( -- ptr u8 n )
   ROW-RESET
   s" PRIM: FOO s" ROW+  ROW-Q
   s"  q PRIM; create LEAK y" ROW+  ROW-Q
   s"  2drop PRIM;" ROW+  ROW-NL
   s" : REAL dup ;" ROW+
   ROW$ ;

: TEST-ROW-STRING-BODY ( -- )
   STRING-BODY-ROW$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 5 ASSERT=
   REG-COUNT 1 ASSERT=
   0 1 47 ASSERT-BARE-ROW
   1 LINT-LEX:TOKEN s" :" ASSERT$      1 LINT-LEX:LINE@ 2 ASSERT=
   2 LINT-LEX:TOKEN s" REAL" ASSERT$
   3 LINT-LEX:TOKEN s" dup" ASSERT$
   4 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ `[']` and `[char]` parse one raw operand, so a closer spelled in that operand is
\ the operand. Both rows must close at their SECOND closer.
: PARSED-OPERAND-ROWS$  ( -- ptr u8 n )
   ROW-RESET
   s" PRIM: FOO ['] PRIM; PE-N PRIM;" ROW+  ROW-NL
   s" PRIM: BAR [char] PPRIM; PE-N PRIM;" ROW+
   ROW$ ;

: TEST-ROW-PARSED-OPERAND ( -- )
   PARSED-OPERAND-ROWS$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 2 ASSERT=
   0 1 30 ASSERT-BARE-ROW
   1 2 34 ASSERT-BARE-ROW ;

\ Comments are inert everywhere, including inside a row body: a closer or an
\ opener written in one is text, not row structure. The first fixture hides both
\ a wrong-family closer and the row's own closer spelling in comments; the second
\ hides an opener, which would otherwise read as a nested row.
: COMMENTED-ROW$  ( -- ptr u8 n )
   ROW-RESET
   s" PRIM: FOO PE-N PE-IN " ROW+  ROW-BS  s"  PRIM; PPRIM; not closers" ROW+  ROW-NL
   s"    PE-N PE-OUT ( PPRIM; also inert ) PRIM;" ROW+  ROW-NL
   s" : TAIL dup ;" ROW+
   ROW$ ;

: OPENER-IN-COMMENT-ROW$  ( -- ptr u8 n )
   ROW-RESET
   s" PRIM: FOO ( PRIM: PPRIM: not nested ) PE-N PE-IN PRIM;" ROW+
   ROW$ ;

: TEST-ROW-COMMENTS-INERT ( -- )
   COMMENTED-ROW$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:COUNT 5 ASSERT=
   0 LINT-LEX:KIND@ LINT-LEX:REGISTRY ASSERT=
   0 LINT-LEX:LINE@ 1 ASSERT=
   0 LINT-LEX:TOKEN s" PRIM;" LINT-ENDS-WITH? ASSERT
   1 LINT-LEX:TOKEN s" :" ASSERT$      1 LINT-LEX:LINE@ 3 ASSERT=
   2 LINT-LEX:TOKEN s" TAIL" ASSERT$
   4 LINT-LEX:TOKEN s" ;" ASSERT$
   OPENER-IN-COMMENT-ROW$ LINT-LEX:SOURCE
   ASSERT-ONE-ROW ;

\ A parenthesized prefix attached to a closer is one row-body word.  Treating
\ only `(X)` as a comment would expose the suffix as a false row closer.
: TEST-ROW-ATTACHED-PAREN ( -- )
   ROW-RESET
   s" PRIM: FOO (X)PRIM;" ROW+
   LEX-ROW
   0 1 1 ASSERT-BAD-AT
   LINT-LEX:COUNT 0 ASSERT= ;

\ The first apparent closer is inside a comment whose opener is followed by a
\ vertical tab.  The row must extend to the later real closer.
: TEST-ROW-CONTROL-COMMENT ( -- )
   ROW-RESET
   s" PRIM: FOO " ROW+
   ROW-LPAREN-C ROW-C+  ROW-VT-C ROW-C+  s" PRIM; package FAKE )" ROW+
   ROW-VT-C ROW-C+  s" PE-N PRIM;" ROW+
   ROW$ nip {: rowu:n :}
   ROW-NL  s" : REAL dup ;" ROW+
   LEX-ROW
   LINT-LEX:ERROR? LINT-NOT ASSERT
   LINT-LEX:COUNT 5 ASSERT=
   REG-COUNT 1 ASSERT=
   0 LINT-LEX:TOKEN nip rowu ASSERT=
   1 LINT-LEX:TOKEN s" :" ASSERT$
   2 LINT-LEX:TOKEN s" REAL" ASSERT$
   4 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ A row body is interpreted, so a `.( ... )` there parses its own text just like
\ `s" ... "` does. The closer spelled inside the print body is that text, and the
\ row must run on to the later real closer. A print body that never closes means
\ the row can never close either, which is the malformed-row diagnostic.
: TEST-ROW-PRINT-BODY ( -- )
   ROW-RESET
   s" PRIM: FOO .( PRIM; package FAKE ) PE-N PRIM;" ROW+
   ROW$ nip {: rowu:n :}
   ROW-NL  s" : REAL dup ;" ROW+
   LEX-ROW
   LINT-LEX:ERROR? LINT-NOT ASSERT
   LINT-LEX:COUNT 5 ASSERT=
   REG-COUNT 1 ASSERT=
   0 LINT-LEX:TOKEN nip rowu ASSERT=
   1 LINT-LEX:TOKEN s" :" ASSERT$
   2 LINT-LEX:TOKEN s" REAL" ASSERT$
   4 LINT-LEX:TOKEN s" ;" ASSERT$
   ROW-RESET  s" PRIM: FOO .( PE-N PRIM;" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT= ;

\ Openers and closers spelled in a top-level comment or string body never reach
\ the row scanner at all, so no registry token appears and the paren comment
\ stays one COMMENT token.
: FAKE-ROWS$  ( -- ptr u8 n )
   ROW-RESET
   ROW-BS  s"  PRIM: FAKE PRIM;" ROW+  ROW-NL
   s" ( PRIM: FAKE2 PPRIM; )" ROW+  ROW-NL
   s" : HOLDER s" ROW+  ROW-Q  s"  PRIM: FAKE3 PRIM;" ROW+  ROW-Q  s"  drop ;" ROW+
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

\ `CLOSE-PRIVATE` closes a package row into the package private wordlist. A bare
\ row has no package wordlist, so there the same spelling is an ordinary effect
\ field or an ordinary name - the wrong-role cases below must NOT close early.
: TEST-ROW-PRIVATE-CLOSER ( -- )
   ROW-RESET  s" PPRIM: PK FOO PE-N PE-OUT CLOSE-PRIVATE" ROW+
   LEX-ROW  ASSERT-ONE-ROW
   ROW-RESET  s" PRIM: FOO PE-N PE-IN CLOSE-PRIVATE PE-N PE-OUT PRIM;" ROW+
   LEX-ROW  ASSERT-ONE-ROW
   ROW-RESET  s" PRIM: CLOSE-PRIVATE PE-N PE-OUT PRIM;" ROW+
   LEX-ROW  ASSERT-ONE-ROW
   ROW-RESET  s" PPRIM: PK CLOSE-PRIVATE PE-N PRIM;" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT
   LINT-LEX:COUNT 0 ASSERT= ;

\ Forth is case-insensitive, so the engine executes every spelling below.
: TEST-ROW-CASE-FOLD ( -- )
   ROW-RESET  s" prim: foo PE-N pRiM;" ROW+
   LEX-ROW  ASSERT-ONE-ROW
   ROW-RESET  s" PpRiM: pk foo PE-N pPrIm;" ROW+
   LEX-ROW  ASSERT-ONE-ROW
   ROW-RESET  s" PPRIM: PK FOO PE-N close-private" ROW+
   LEX-ROW  ASSERT-ONE-ROW ;

\ A header field names the primitive, so an opener or a closer of this row's
\ family standing there means the header is missing. Every case reports the
\ OPENER site, and the scan stops with no token from the row or after it.
: TEST-ROW-BAD-HEADER ( -- )
   ROW-RESET  s" PRIM: PRIM: FOO PRIM;" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" PRIM: PPRIM;" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" PRIM: PRIM;" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" PPRIM: PPRIM; FOO PE-N PPRIM;" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" PPRIM: PK PPRIM;" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" PRIM:" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" PPRIM: PK" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT= ;

\ A row that never closes, closes with the other family's closer, or nests an
\ opener is malformed at its opener - including when a parsed operand or a string
\ literal is what ran off the end.
: TEST-ROW-BAD-BODY ( -- )
   ROW-RESET  s" PRIM: FOO PE-N PE-IN" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT  LINT-LEX:COUNT 0 ASSERT=
   ROW-RESET  s" PRIM: FOO PE-N PPRIM; PRIM;" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT
   ROW-RESET  s" PPRIM: PK FOO PE-N PRIM; PPRIM;" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT
   ROW-RESET  s" PRIM: FOO PRIM: BAR PRIM; PRIM;" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT
   ROW-RESET  s" PRIM: FOO PPRIM: PK BAR PPRIM; PRIM;" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT
   ROW-RESET  s" PRIM: FOO [']" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT
   ROW-RESET  s" PRIM: FOO s" ROW+  ROW-Q  s"  unclosed" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT
   ROW-RESET  s" PRIM: FOO s" ROW+  ROW-BS ROW-Q  s"  unclosed" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT ;

\ The diagnostic names the opener, not the point of discovery, and the tokens
\ before the row survive while nothing at or after it is exposed.
: LATE-BAD-ROW$  ( -- ptr u8 n )
   ROW-RESET
   s" : FIRST dup ;" ROW+  ROW-NL
   s"    PRIM: PPRIM;" ROW+  ROW-NL
   s" : NEVER-SEEN drop ;" ROW+
   ROW$ ;

: TEST-ROW-DIAGNOSTIC-SPAN ( -- )
   LATE-BAD-ROW$ LINT-LEX:SOURCE
   17 2 4 ASSERT-BAD-AT
   LINT-LEX:COUNT 4 ASSERT=
   0 LINT-LEX:TOKEN s" :" ASSERT$
   1 LINT-LEX:TOKEN s" FIRST" ASSERT$
   3 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ After one of these the engine consumes the next word as a parsed name and never
\ executes it, so `: PRIM: ( -- ) parse-name PE-OPEN ;` in src/core/checker.f
\ declares the opener instead of opening a row. Getting this wrong hides real
\ source: a row opened at that definition closes at `: PRIM; ( -- )` seventeen
\ lines later and swallows three definitions.
: DEFINER-POSITION$  ( -- ptr u8 n )
   ROW-RESET
   s" : PRIM: dup ;" ROW+  ROW-NL
   s" : PPRIM: drop ;" ROW+  ROW-NL
   s" : A ' PRIM: drop ;" ROW+  ROW-NL
   s" : B postpone PPRIM: ;" ROW+  ROW-NL
   s" : C undefine PRIM: ;" ROW+  ROW-NL
   s" : D ['] PRIM: drop ;" ROW+  ROW-NL
   s" : LAST over ;" ROW+
   ROW$ ;

: TEST-ROW-DEFINER-POSITION ( -- )
   DEFINER-POSITION$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   REG-COUNT 0 ASSERT=
   LINT-LEX:COUNT 34 ASSERT=
   1 LINT-LEX:TOKEN s" PRIM:" ASSERT$   1 LINT-LEX:KIND@ LINT-LEX:WORD ASSERT=
   2 LINT-LEX:TOKEN s" dup" ASSERT$
   5 LINT-LEX:TOKEN s" PPRIM:" ASSERT$
   30 LINT-LEX:TOKEN s" :" ASSERT$
   31 LINT-LEX:TOKEN s" LAST" ASSERT$
   32 LINT-LEX:TOKEN s" over" ASSERT$
   33 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ SOURCE clears the diagnostic before every scan, so a good row source scanned
\ after a malformed one reports nothing and returns a full table.
: TEST-ROW-REUSE-AFTER-ERROR ( -- )
   ROW-RESET  s" PRIM: FOO PE-N PE-IN" ROW+
   LEX-ROW  0 1 1 ASSERT-BAD-AT
   EIGHT-BARE-ROWS$ LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:ERROR-KIND@ 0 ASSERT=
   LINT-LEX:ERROR-BYTE@ 0 ASSERT=
   LINT-LEX:ERROR-LINE@ 0 ASSERT=
   LINT-LEX:ERROR-COL@ 0 ASSERT=
   LINT-LEX:COUNT 12 ASSERT=
   REG-COUNT 8 ASSERT=
   11 LINT-LEX:TOKEN s" ;" ASSERT$ ;

\ End-to-end structural acceptance on the real axiom sources. Both must scan
\ without error; checker.f's quoted primitive name must remain one registry token
\ ending at its own closer rather than opening a string.
: TEST-REAL-REGISTRY-FILES ( -- )
   s" src/core/checker.f" LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:ERROR-KIND@ 0 ASSERT=
   \ Ratchet on the whole axiom registry of checker.f: 285 `PRIM:` rows plus 63
   \ `PPRIM:` rows. It was 345 before the sealed-owner WID registry was deleted,
   \ which took the four `owner-wid-preflight?` / `owner-wid-public?` /
   \ `owner-wid-private?` / `owner-wid?` axioms with it, and 341 before the bulk
   \ publication window (dot habu-publish-native-code-886e3ef9) added the three
   \ trusted-only axioms `code-publish`, `callmap-set` and `xref-retarget`, and
   \ 344 before the replay package-state window (dot habu-own-pkg-state-acf7086c)
   \ added `CHECKER-USING-PUSH` and `CHECKER-USING-POP`, the two rows a replayed
   \ source's own `using` and `;using` are driven through, and 346 before the
   \ no-emit publication hold (commit 94bf026b, 2026-08-06) added the two
   \ `CHECKER-TAPE` axioms `HOLD-ARM` and `HOLD-DISARM`, the rows that arm and
   \ release the held-publication window. That landing did not bump this row, so
   \ the ratchet went red exactly as designed; it was misread as a symptom of the
   \ concurrent-gate timing incident (dot habu-attr-the-candidate-4a2356c5) and
   \ merged anyway. The assertion number is the tell: a ratchet miss always stops
   \ at the same assertion, a contention flake stops at a timing one.
   \ The `PRIM:`/`PPRIM:` split above is also two higher than the old comment
   \ claimed - 283 plus 61 never summed to the 346 it sat next to. The lexer is
   \ the authority here and it counted 285 plus 63; the prose was stale, the
   \ constant was not.
   \ It was 348 before the string-literal tranche (dot
   \ habu-compile-str-literals-30a7121b) added the `CHECKER-TAPE` axiom
   \ `K-STRING`, the fourth token class the reader hands its observer - the one
   \ that carries a string literal's decoded body rather than its opener.
   \ It was 349 before the per-site address relocation record (dot
   \ habu-per-site-relocation-bb9b6d70) added the trusted-only axiom
   \ `addrmap-set`, the sibling of `callmap-set` for the second relocation map -
   \ the one that records where an address chain starts.
   \ It was 350 before the name-query split (dot
   \ habu-checker-defined-answers-1504bbde) replaced the single
   \ `CHECKER-DEFINED?` axiom with two: `CHECKER-DEFINED-HERE?`, the
   \ defining-scope question a duplicate guard asks, and `CHECKER-RESOLVES?`,
   \ the reference-scope question everything else was asking and getting the
   \ first one's answer. One row out, two in, so the count rose by one.
   \ It was 351 before the boot-prefix reader sweep (dot
   \ habu-turn-the-registry-4c064064) added twenty rows in one block: eight of
   \ the ten type-family registry readers src/compiler/native/family.f used to
   \ reach through TRUSTED: bridges, the eleven effect-store readers dict.f and
   \ reach.f reached the same way, and `CTL-DEAD?`, the one new checker word -
   \ the dead-flag question dict.f used to answer with its own copy of the mask.
   \ Twenty-one one-line trusted boundaries went out of the native chain for
   \ them, so the count rose by twenty while the tree's unchecked surface shrank.
   \ It rose by twenty and not twenty-two because two of those readers,
   \ TFAM-VAR-COUNT@ and TFAM-NAME$, already had rows in the public-signature
   \ metadata block; the sweep first landed duplicates for them, which this
   \ ratchet is why anyone would notice - a second row for a symbol is dead,
   \ since PRIM-FIRST-SCAN answers with the first slot.
   \ Rows for boot-prefix HABU words, not engine primitives, which the table has
   \ admitted since EXT-MARK-FREE-TAIL and CHECKER-DEFFAMILY.
   \ It was 371 before the definer-facing registrar split (dot
   \ habu-make-trust-refuse-cc8e19de) added `TRUST-DECL`, which needs the same
   \ axiom as `TRUST` for the same reason: the engine looks it up by name, so
   \ the seal-time internal-word marking pass has to leave it findable.
   \ It was 372 before the literal-value authority (dot
   \ habu-record-the-engine-79c570ed) added `num-parse`, the engine's own number
   \ reader over bytes a caller already holds. That one is an engine primitive
   \ rather than a boot-prefix word, and it is the row that let
   \ src/compiler/native/feed.f stop reading a literal's spelling back with a
   \ decoder of its own - a whole file of re-derived float arithmetic
   \ (src/compiler/native/real-lit.f) went out with it.
   \ It was 373 before the return-stack lowering (dot
   \ habu-thread-parked-values-3cabd3aa) added `EFFECT-RET-NEUTRAL?`, the row
   \ that lets checked Habu ask whether a word's declared effect leaves its
   \ caller's return stack alone. The reader itself already existed; what it did
   \ not have was an axiom, so src/compiler/native/dict.f - the first consumer the
   \ checker's own comment named - could not call it at all. One row in, and it
   \ is an engine-adjacent checker word rather than a primitive, on the same
   \ terms as its siblings `EFFECT-QUOT-SIMPLE?` and `CTL-DEAD?`.
   \ A lexer that swallowed a row into a neighbouring string would drop the
   \ count, not raise it.
   REG-COUNT 374 ASSERT=
   \ The `PRIM: s"` row is the one that broke the old lexer: its name is a live
   \ string opener, so the word path consumed source through the quote in the next
   \ row. Name that row and pin that it is one token ending at its own closer.
   ROW-RESET  s" PRIM: s" ROW+  ROW-Q
   ROW$ REG-FIND {: q:n :}
   q 0 >= ASSERT
   q LINT-LEX:KIND@ LINT-LEX:REGISTRY ASSERT=
   q LINT-LEX:TOKEN s" PRIM;" LINT-ENDS-WITH? ASSERT
   q LINT-LEX:TOKEN s" PE-PTR-U8" LINT-CONTAINS? ASSERT
   s" src/core/sumtype.f" LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT LINT-LEX:SOURCE
   LINT-LEX:ERROR? 0= ASSERT
   LINT-LEX:ERROR-KIND@ 0 ASSERT=
   \ The three surviving block openers of sumtype.f: NEWTYPE, SUMTYPE and
   \ PRODUCT. The global ENUM keyword is an ordinary checked ( -- ) definition
   \ over ENUM-DECL:ED-RUN now, so it carries no axiom of its own.
   REG-COUNT 3 ASSERT= ;

: TEST-TOKENIZER  ( -- )
   LINT-TRUE PARENS? !
   TOK-FIX$ TOKENIZE
   TN# @ 6 ASSERT=
   0 TOK s" :" ASSERT$
   0 TOK0? ASSERT
   1 TOK s" X" ASSERT$
   1 TOK0? 0= ASSERT
   2 TOK s" dup" ASSERT$
   2 TEOL? ASSERT
   3 TOK s" :" ASSERT$
   3 TOK0? ASSERT
   4 TOK s" Y" ASSERT$
   5 TOK s" ;" ASSERT$
   5 TEOL? ASSERT ;

: TEST-BIG-LEXER  ( -- )
   BIG-LEX$ LINT-LEX:SOURCE
   LINT-LEX:COUNT BIG-LEX-TOKENS ASSERT=
   0 LINT-LEX:TOKEN s" x" ASSERT$
   8192 LINT-LEX:TOKEN s" x" ASSERT$
   BIG-LEX-TOKENS 1- LINT-LEX:TOKEN s" x" ASSERT$ ;

: TEST-LINT-SOURCE ( -- )
   s" tools/lint/text.f" 2dup FILE-SIZE {: path:ptr pathu:n size:n :}
   path pathu LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT {: source:ptr sourceu:n :}
   sourceu size ASSERT=
   source sourceu s" package LINT-SOURCE" LINT-CONTAINS? ASSERT
   s" src/habu/habu2.f" 2dup FILE-SIZE {: large:ptr largeu:n largesize:n :}
   large largeu LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT {: largesource:ptr largesourceu:n :}
   largesourceu largesize ASSERT=
   largesource largesourceu s" LABEL@" LINT-CONTAINS? ASSERT ;

\ CMP-CI is an ORDER, and a caller only gets to replace a scan with a binary
\ search if it is a total one whose 0 answer is exactly LINT-STR=CI's true. The
\ three laws are checked directly: sign, antisymmetry, and agreement with the
\ equality the scan used - including the case where one name is a prefix of the
\ other, which is where a compare that only walked the shared bytes would call
\ two different names equal.
: CMP-SIGN ( n -- n )
   dup 0 < if drop -1 exit then
   0 > if 1 exit then
   0 ;

: BOOL>N ( bool -- n )
   IF 1 ELSE 0 THEN ;

: ASSERT-CMP ( ptr u8 n ptr u8 n n -- ) {: a:ptr u:n b:ptr v:n want:n :}
   a u b v LINT-ORDER:CMP-CI CMP-SIGN want ASSERT=
   b v a u LINT-ORDER:CMP-CI CMP-SIGN 0 want - ASSERT=          \ antisymmetric
   a u b v LINT-STR=CI BOOL>N  want 0= BOOL>N  ASSERT= ;        \ 0 iff LINT-STR=CI

: TEST-CMP-CI ( -- )
   s" abc" s" abd" -1 ASSERT-CMP
   s" abd" s" abc" 1 ASSERT-CMP
   s" abc" s" abc" 0 ASSERT-CMP
   s" ABC" s" abc" 0 ASSERT-CMP                                 \ folded, like the dictionary
   s" aBc" s" AbC" 0 ASSERT-CMP
   s" ab" s" abc" -1 ASSERT-CMP                                 \ prefix sorts first
   s" abc" s" ab" 1 ASSERT-CMP
   s" " s" " 0 ASSERT-CMP
   s" " s" a" -1 ASSERT-CMP
   s" RAW>NODE" s" RAW>SLOT" -1 ASSERT-CMP                      \ real mint names
   s" MINT-ROW" s" MINT-PATH" 1 ASSERT-CMP
   s" raw>node" s" RAW>NODE" 0 ASSERT-CMP ;

: TEST-CMP-CI-TRANSITIVE ( -- )                                 \ a<b and b<c imply a<c
   s" MINT-BYTE-LEN" s" MINT-CELL-OFF" -1 ASSERT-CMP
   s" MINT-CELL-OFF" s" MINT-INDEX" -1 ASSERT-CMP
   s" MINT-BYTE-LEN" s" MINT-INDEX" -1 ASSERT-CMP ;

: RUN  ( -- )
   1 TEST-N !
   TEST-CMP-CI
   TEST-CMP-CI-TRANSITIVE
   INIT-FIXTURES
   TEST-STRINGS
   TEST-SCANNERS
   TEST-SIGS
   TEST-LEXER
   TEST-LEXER-PAREN-NAME
   TEST-LEXER-PAREN-CALL
   TEST-LEXER-PRINT-PAREN
   TEST-LEXER-PRINT-NAME-POS
   TEST-LEXER-ENGINE-DELIMS
   TEST-LEXER-NO-ERROR
   TEST-LEXER-ESC-QUOTE
   TEST-LEXER-STRING-PAYLOAD
   TEST-LEXER-UNTERM-QUOTE
   TEST-LEXER-REUSE-AFTER-ERROR
   TEST-ROW-QUOTE-NAMES
   TEST-ROW-STRING-BODY
   TEST-ROW-PARSED-OPERAND
   TEST-ROW-COMMENTS-INERT
   TEST-ROW-ATTACHED-PAREN
   TEST-ROW-CONTROL-COMMENT
   TEST-ROW-PRINT-BODY
   TEST-ROW-FAKE-IN-COMMENT-AND-STRING
   TEST-ROW-PRIVATE-CLOSER
   TEST-ROW-CASE-FOLD
   TEST-ROW-BAD-HEADER
   TEST-ROW-BAD-BODY
   TEST-ROW-DIAGNOSTIC-SPAN
   TEST-ROW-DEFINER-POSITION
   TEST-ROW-REUSE-AFTER-ERROR
   TEST-REAL-REGISTRY-FILES
   TEST-TOKENIZER
   TEST-BIG-LEXER
   TEST-LINT-SOURCE
   s" text-foundation-test: ok (" type TEST-N @ 1- . s"  assertions)" type cr ;

RUN

;package
