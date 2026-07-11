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
   STR-FIX-LEN BUF-RESET
   32 STR-FIX FIX-CAP STR-FIX-LEN BUF-APPEND-C
   32 STR-FIX FIX-CAP STR-FIX-LEN BUF-APPEND-C
   s" Alpha beta" STR-FIX FIX-CAP STR-FIX-LEN BUF-APPEND
   10 STR-FIX FIX-CAP STR-FIX-LEN BUF-APPEND-C
   s" Gamma" STR-FIX FIX-CAP STR-FIX-LEN BUF-APPEND
   32 STR-FIX FIX-CAP STR-FIX-LEN BUF-APPEND-C
   32 STR-FIX FIX-CAP STR-FIX-LEN BUF-APPEND-C
   10 STR-FIX FIX-CAP STR-FIX-LEN BUF-APPEND-C ;
: STR-FIX$  ( -- ptr u8 n )  STR-FIX STR-FIX-LEN BUF-LEN@ ;

: INIT-TRUST-FIX  ( -- )
   TRUST-LEN BUF-RESET
   s" prefix s" TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND
   DQUOTE TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND-C
   32 TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND-C
   s" SQ" TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND
   DQUOTE TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND-C
   32 TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND-C
   115 TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND-C
   DQUOTE TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND-C
   32 TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND-C
   s" ( n -- n )" TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND
   DQUOTE TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND-C
   s"  TRUST \\ comment" TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND
   10 TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND-C ;
: TRUST-FIX$  ( -- ptr u8 n )  TRUST-FIX TRUST-LEN BUF-LEN@ ;

: TRUST-FIX-RESET  ( -- )
   TRUST-LEN BUF-RESET ;

: TRUST-FIX+  ( ptr u8 n -- )
   TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND ;

: TRUST-FIX-C+  ( n -- )
   TRUST-FIX FIX-CAP TRUST-LEN BUF-APPEND-C ;

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
   SRC-LEN BUF-RESET
   s" : REPL-SRC s" SRC-FIX FIX-CAP SRC-LEN BUF-APPEND
   DQUOTE SRC-FIX FIX-CAP SRC-LEN BUF-APPEND-C
   32 SRC-FIX FIX-CAP SRC-LEN BUF-APPEND-C
   s" src/habu/repl.f" SRC-FIX FIX-CAP SRC-LEN BUF-APPEND
   DQUOTE SRC-FIX FIX-CAP SRC-LEN BUF-APPEND-C
   s"  ;" SRC-FIX FIX-CAP SRC-LEN BUF-APPEND
   10 SRC-FIX FIX-CAP SRC-LEN BUF-APPEND-C ;
: SRC-FIX$  ( -- ptr u8 n )  SRC-FIX SRC-LEN BUF-LEN@ ;

: INIT-BT-FIX  ( -- )
   BT-LEN BUF-RESET
   s" See `tools/lint/source-lex.f` and `plain`." BT-FIX FIX-CAP BT-LEN BUF-APPEND
   10 BT-FIX FIX-CAP BT-LEN BUF-APPEND-C ;
: BT-FIX$  ( -- ptr u8 n )  BT-FIX BT-LEN BUF-LEN@ ;

: INIT-LEX-FIX  ( -- )
   LEX-LEN BUF-RESET
   s" : SQ ( n -- n ) s" LEX-FIX FIX-CAP LEX-LEN BUF-APPEND
   DQUOTE LEX-FIX FIX-CAP LEX-LEN BUF-APPEND-C
   s"  hi : ; ( x )" LEX-FIX FIX-CAP LEX-LEN BUF-APPEND
   DQUOTE LEX-FIX FIX-CAP LEX-LEN BUF-APPEND-C
   s"  dup " LEX-FIX FIX-CAP LEX-LEN BUF-APPEND
   92 LEX-FIX FIX-CAP LEX-LEN BUF-APPEND-C
   s"  skip die" LEX-FIX FIX-CAP LEX-LEN BUF-APPEND
   10 LEX-FIX FIX-CAP LEX-LEN BUF-APPEND-C
   99 LEX-FIX FIX-CAP LEX-LEN BUF-APPEND-C
   DQUOTE LEX-FIX FIX-CAP LEX-LEN BUF-APPEND-C
   s"  z" LEX-FIX FIX-CAP LEX-LEN BUF-APPEND
   DQUOTE LEX-FIX FIX-CAP LEX-LEN BUF-APPEND-C
   s"  ;" LEX-FIX FIX-CAP LEX-LEN BUF-APPEND
   10 LEX-FIX FIX-CAP LEX-LEN BUF-APPEND-C ;
: LEX-FIX$  ( -- ptr u8 n )  LEX-FIX LEX-LEN BUF-LEN@ ;

: INIT-TOK-FIX  ( -- )
   TOK-LEN BUF-RESET
   s" : X ( n -- n ) dup " TOK-FIX FIX-CAP TOK-LEN BUF-APPEND
   92 TOK-FIX FIX-CAP TOK-LEN BUF-APPEND-C
   s"  skip" TOK-FIX FIX-CAP TOK-LEN BUF-APPEND
   10 TOK-FIX FIX-CAP TOK-LEN BUF-APPEND-C
   s" : Y ;" TOK-FIX FIX-CAP TOK-LEN BUF-APPEND ;
: TOK-FIX$  ( -- ptr u8 n )  TOK-FIX TOK-LEN BUF-LEN@ ;

: INIT-BIG-LEX  ( -- )
   BIG-LEX-TOKENS 2 * MEM-ALLOC-BYTES BIG-LEX-U ! BIG-LEX-A!
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
   s" : BAD ( i64 ) dup ;" LEX-SOURCE
   2 LCONTENT SIG-KIND SIG-MISSING ASSERT= ;

: TEST-LEXER  ( -- )
   LEX-FIX$ LEX-SOURCE
   LEX-UNTERM-QUOTE? 0= ASSERT
   L# @ 7 ASSERT=
   0 LK@ L-WORD ASSERT=    0 LEX-TOK s" :" ASSERT$
   0 LB@ 0 ASSERT=  0 LL@ 1 ASSERT=  0 LC@ 1 ASSERT=
   1 LK@ L-WORD ASSERT=    1 LEX-TOK s" SQ" ASSERT$
   1 LB@ 2 ASSERT=  1 LL@ 1 ASSERT=  1 LC@ 3 ASSERT=
   2 LK@ L-COMMENT ASSERT= 2 LCONTENT s"  n -- n " ASSERT$
   2 LB@ 5 ASSERT=  2 LL@ 1 ASSERT=  2 LC@ 6 ASSERT=
   3 LEX-TOK nip 2 ASSERT=  3 LEX-TOK drop c@ 115 ASSERT=  3 LEX-TOK drop 1+ c@ DQUOTE ASSERT=
   3 LB@ 16 ASSERT=  3 LL@ 1 ASSERT=  3 LC@ 17 ASSERT=
   4 LEX-TOK s" dup" ASSERT$
   4 LB@ 33 ASSERT=  4 LL@ 1 ASSERT=  4 LC@ 34 ASSERT=
   5 LEX-TOK nip 2 ASSERT=  5 LEX-TOK drop c@ 99 ASSERT=  5 LEX-TOK drop 1+ c@ DQUOTE ASSERT=
   5 LB@ 48 ASSERT=  5 LL@ 2 ASSERT=  5 LC@ 1 ASSERT=
   6 LEX-TOK s" ;" ASSERT$
   6 LB@ 54 ASSERT=  6 LL@ 2 ASSERT=  6 LC@ 7 ASSERT= ;

: TEST-LEXER-UNTERM-QUOTE ( -- )
   LEX-LEN BUF-RESET
   s" : BAD s" LEX-FIX FIX-CAP LEX-LEN BUF-APPEND
   DQUOTE LEX-FIX FIX-CAP LEX-LEN BUF-APPEND-C
   s"  nope" LEX-FIX FIX-CAP LEX-LEN BUF-APPEND
   LEX-FIX LEX-LEN BUF-LEN@ LEX-SOURCE
   LEX-UNTERM-QUOTE? ASSERT
   LEX-UNTERM-BYTE @ 6 ASSERT=
   LEX-UNTERM-LINE @ 1 ASSERT=
   LEX-UNTERM-COL @ 7 ASSERT= ;

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
   BIG-LEX$ LEX-SOURCE
   L# @ BIG-LEX-TOKENS ASSERT=
   0 LEX-TOK s" x" ASSERT$
   8192 LEX-TOK s" x" ASSERT$
   BIG-LEX-TOKENS 1- LEX-TOK s" x" ASSERT$ ;

: TEXT-FOUNDATION-TEST  ( -- )
   1 TEST-N !
   INIT-FIXTURES
   TEST-STRINGS
   TEST-SCANNERS
   TEST-SIGS
   TEST-LEXER
   TEST-LEXER-UNTERM-QUOTE
   TEST-TOKENIZER
   TEST-BIG-LEXER
   s" text-foundation-test: ok (" type TEST-N @ 1- . s"  assertions)" type cr ;

TEXT-FOUNDATION-TEST
