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

: INIT-TOK-FIX  ( -- )
   TOK-LEN STR:BUF-RESET
   s" : X ( n -- n ) dup " STR:LENGTH TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND
   92 TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND-C
   s"  skip" STR:LENGTH TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND
   10 TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND-C
   s" : Y ;" STR:LENGTH TOK-FIX FIX-CAP STR:LENGTH TOK-LEN STR:BUF-APPEND ;
: TOK-FIX$  ( -- ptr u8 n )  TOK-FIX TOK-LEN STR:BUF-LEN@ CAD-NUM:TFT-BL>RAW ;

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

: RUN  ( -- )
   1 TEST-N !
   INIT-FIXTURES
   TEST-STRINGS
   TEST-SCANNERS
   TEST-SIGS
   TEST-LEXER
   TEST-LEXER-NO-ERROR
   TEST-LEXER-ESC-QUOTE
   TEST-LEXER-UNTERM-QUOTE
   TEST-LEXER-REUSE-AFTER-ERROR
   TEST-TOKENIZER
   TEST-BIG-LEXER
   TEST-LINT-SOURCE
   s" text-foundation-test: ok (" type TEST-N @ 1- . s"  assertions)" type cr ;

RUN

;package
