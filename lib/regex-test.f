\ regex-test.f - focused tests for bounded regex scanner tokens.
\ Run: cat lib/errors.f lib/string.f lib/test.f lib/regex.f lib/regex-test.f | bin/hb

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/regex.f

1 constant RXT-EX-FAIL
32 constant RXT-BUF-CAP
3 constant RXT-SMALL-CAP

create RXT-BUF RXT-BUF-CAP allot
create RXT-SMALL RXT-SMALL-CAP allot
variable RXT-RX-LEN

: RXT-BUF-PTR ( -- ptr u8 )
   RXT-BUF ;

: RXT-SMALL-PTR ( -- ptr u8 )
   RXT-SMALL ;

: RXT-COMPILE ( ptr u8 n -- n )
   >LEN RXT-BUF-PTR RXT-BUF-CAP >LEN RX-COMPILE LEN>N ;

: RXT-COMPILE! ( ptr u8 n -- )
   RXT-COMPILE RXT-RX-LEN ! ;

: RXT-B@ ( n -- n ) {: ix :}
   RXT-BUF ix + c@ ;

: RXT-MATCH? ( ptr u8 n -- bool )
   >LEN RXT-BUF-PTR RXT-RX-LEN @ >LEN RX-MATCH? ;

: RXT-FIND ( ptr u8 n -- n n bool )
   >LEN RXT-BUF-PTR RXT-RX-LEN @ >LEN RX-FIND {: off len found :}
   off OFF>N len LEN>N found ;

: RXT-COUNT ( ptr u8 n -- n )
   >LEN RXT-BUF-PTR RXT-RX-LEN @ >LEN RX-COUNT COUNT>N ;

: RXT-ASSERT-LIT ( n n -- ) {: off c :}
   off RXT-B@ RX-TOK-LITERAL T=
   off 1 + RXT-B@ c T= ;

: RXT-TEST-LITERALS ( -- )
   s" ab" RXT-COMPILE 4 T=
   0 97 RXT-ASSERT-LIT
   2 98 RXT-ASSERT-LIT ;

: RXT-TEST-META-TOKENS ( -- )
   s" .^$?*+" RXT-COMPILE 6 T=
   0 RXT-B@ RX-TOK-DOT T=
   1 RXT-B@ RX-TOK-BOL T=
   2 RXT-B@ RX-TOK-EOL T=
   3 RXT-B@ RX-TOK-QUESTION T=
   4 RXT-B@ RX-TOK-STAR T=
   5 RXT-B@ RX-TOK-PLUS T= ;

: RXT-TEST-TOKEN-TABLES ( -- )
   RX-C-DOT RX-ESCAPABLE? TTRUE
   97 RX-ESCAPABLE? TFALSE
   RX-C-LPAREN RX-UNSUPPORTED-META? TTRUE
   RX-C-DOT RX-UNSUPPORTED-META? TFALSE
   RX-C-PLUS RX-META-TOKEN MATCH option
     none OF 0 0= 0= ENDOF                          \ none -> fail (+ is a metachar)
     some OF RX-TOK-PLUS = ENDOF                     \ some(tok) -> the plus token
   ;MATCH TTRUE
   97 RX-META-TOKEN MATCH option
     none OF 0 0= ENDOF                             \ none -> pass (a is not a metachar)
     some OF drop 0 0= 0= ENDOF
   ;MATCH TTRUE ;

: RXT-TEST-SINGLE-TOKEN-EMIT ( -- )
   RX-TOK-DOT 0 >OFF RXT-BUF-PTR RXT-BUF-CAP >LEN 0 >OFF RX-EMIT-SINGLE-TOKEN
   OFF>N 1 T=
   OFF>N 1 T=
   0 RXT-B@ RX-TOK-DOT T= ;

: RXT-TEST-ESCAPES ( -- )
   s" \.\^\$\[\]\?\*\+\\" RXT-COMPILE 18 T=
   0 RX-C-DOT RXT-ASSERT-LIT
   2 RX-C-CARET RXT-ASSERT-LIT
   4 RX-C-DOLLAR RXT-ASSERT-LIT
   6 RX-C-LBRACKET RXT-ASSERT-LIT
   8 RX-C-RBRACKET RXT-ASSERT-LIT
   10 RX-C-QUESTION RXT-ASSERT-LIT
   12 RX-C-STAR RXT-ASSERT-LIT
   14 RX-C-PLUS RXT-ASSERT-LIT
   16 RX-C-BACKSLASH RXT-ASSERT-LIT ;

: RXT-TEST-CLASSES ( -- )
   s" [abc][^0-9]" RXT-COMPILE 10 T=
   0 RXT-B@ RX-TOK-CLASS T=
   1 RXT-B@ 3 T=
   2 RXT-B@ 97 T=
   3 RXT-B@ 98 T=
   4 RXT-B@ 99 T=
   5 RXT-B@ RX-TOK-NCLASS T=
   6 RXT-B@ 3 T=
   7 RXT-B@ 48 T=
   8 RXT-B@ RX-C-DASH T=
   9 RXT-B@ 57 T= ;

: RXT-TEST-CLASS-ESCAPE ( -- )
   s" [a\]b]" RXT-COMPILE 6 T=
   0 RXT-B@ RX-TOK-CLASS T=
   1 RXT-B@ 4 T=
   2 RXT-B@ 97 T=
   3 RXT-B@ RX-C-BACKSLASH T=
   4 RXT-B@ RX-C-RBRACKET T=
   5 RXT-B@ 98 T= ;

: RXT-DANGLING-ESCAPE ( -- )
   s" \" RXT-COMPILE drop ;

: RXT-BAD-ESCAPE ( -- )
   s" \a" RXT-COMPILE drop ;

: RXT-UNCLOSED-CLASS ( -- )
   s" [abc" RXT-COMPILE drop ;

: RXT-EMPTY-CLASS ( -- )
   s" []" RXT-COMPILE drop ;

: RXT-EMPTY-NEG-CLASS ( -- )
   s" [^]" RXT-COMPILE drop ;

: RXT-DANGLING-CLASS-ESCAPE ( -- )
   s" [a\]" RXT-COMPILE drop ;

: RXT-CAP-OVERFLOW ( -- )
   s" ab" >LEN RXT-SMALL-PTR RXT-SMALL-CAP >LEN RX-COMPILE drop ;

: RXT-NEG-CAP ( -- )
   s" a" >LEN RXT-BUF-PTR -1 >LEN RX-COMPILE drop ;

: RXT-BAD-QUANT ( -- )
   s" *a" RXT-COMPILE! s" aaa" RXT-MATCH? drop ;

: RXT-DOUBLE-QUANT ( -- )
   s" a**" RXT-COMPILE! s" aaa" RXT-MATCH? drop ;

: RXT-ANCHOR-QUANT ( -- )
   s" ^*" RXT-COMPILE! s" aaa" RXT-MATCH? drop ;

: RXT-ASSERT-NOT-FOUND ( n n bool -- )
   TFALSE
   0 T=
   0 T= ;

: RXT-ASSERT-FOUND ( n n bool n n -- ) {: want-off want-len :}
   TTRUE
   want-len T=
   want-off T= ;

: RXT-TEST-MATCH-LITERALS ( -- )
   s" abc" RXT-COMPILE!
   s" abc" RXT-MATCH? TTRUE
   s" ab" RXT-MATCH? TFALSE
   s" xabc" RXT-MATCH? TFALSE ;

: RXT-TEST-MATCH-META ( -- )
   s" a.c" RXT-COMPILE!
   s" abc" RXT-MATCH? TTRUE
   s" ac" RXT-MATCH? TFALSE
   s" ^a.c$" RXT-COMPILE!
   s" abc" RXT-MATCH? TTRUE
   s" abcx" RXT-MATCH? TFALSE ;

: RXT-TEST-MATCH-CLASSES ( -- )
   s" [a-c][^0-9]" RXT-COMPILE!
   s" bX" RXT-MATCH? TTRUE
   s" b5" RXT-MATCH? TFALSE
   s" dX" RXT-MATCH? TFALSE ;

: RXT-TEST-MATCH-ESCAPED ( -- )
   s" a\.\+\?" RXT-COMPILE!
   s" a.+?" RXT-MATCH? TTRUE
   s" ax+?" RXT-MATCH? TFALSE ;

: RXT-TEST-MATCH-REPEATS ( -- )
   s" ab?c" RXT-COMPILE!
   s" abc" RXT-MATCH? TTRUE
   s" ac" RXT-MATCH? TTRUE
   s" abb" RXT-MATCH? TFALSE
   s" ab*c" RXT-COMPILE!
   s" ac" RXT-MATCH? TTRUE
   s" abbbc" RXT-MATCH? TTRUE
   s" ab+c" RXT-COMPILE!
   s" abc" RXT-MATCH? TTRUE
   s" ac" RXT-MATCH? TFALSE
   s" a*a" RXT-COMPILE!
   s" aaa" RXT-MATCH? TTRUE ;

: RXT-TEST-FIND ( -- )
   s" a.c" RXT-COMPILE!
   s" zzaXczz" RXT-FIND 2 3 RXT-ASSERT-FOUND
   s" zzz" RXT-FIND RXT-ASSERT-NOT-FOUND
   s" ^abc" RXT-COMPILE!
   s" xabc" RXT-FIND RXT-ASSERT-NOT-FOUND
   s" abc" RXT-FIND 0 3 RXT-ASSERT-FOUND ;

: RXT-TEST-COUNT ( -- )
   s" a+" RXT-COMPILE!
   s" aaabaa" RXT-COUNT 2 T=
   s" [0-9]" RXT-COMPILE!
   s" a1b23" RXT-COUNT 3 T=
   s" a*" RXT-COMPILE!
   s" bbb" RXT-COUNT 4 T= ;

: RXT-TEST-THROWS ( -- )
   [: RXT-DANGLING-ESCAPE ;] catch E-RX-SYNTAX T=
   [: RXT-BAD-ESCAPE ;] catch E-RX-SYNTAX T=
   [: RXT-UNCLOSED-CLASS ;] catch E-RX-SYNTAX T=
   [: RXT-EMPTY-CLASS ;] catch E-RX-SYNTAX T=
   [: RXT-EMPTY-NEG-CLASS ;] catch E-RX-SYNTAX T=
   [: RXT-DANGLING-CLASS-ESCAPE ;] catch E-RX-SYNTAX T=
   [: RXT-CAP-OVERFLOW ;] catch E-RX-CAPACITY T=
   [: RXT-NEG-CAP ;] catch E-RX-CAPACITY T=
   [: RXT-BAD-QUANT ;] catch E-RX-SYNTAX T=
   [: RXT-DOUBLE-QUANT ;] catch E-RX-SYNTAX T=
   [: RXT-ANCHOR-QUANT ;] catch E-RX-SYNTAX T=
   [: RX-ACTIVE -1 >OFF RX-FLAG? drop ;] catch E-RX-CAPACITY T=
   [: RX-ACTIVE RX-STATE-CAP >OFF RX-FLAG? drop ;] catch E-RX-CAPACITY T= ;

: RXT-REPORT ( -- )
   T-FAILURES 0= if s" regex-test: ok" type cr exit then
   T-FAILURES . s" regex-test: failures" type cr
   s" regex-test: failures" RXT-EX-FAIL die ;

: RXT-MAIN ( -- )
   T-RESET
   RXT-TEST-LITERALS
   RXT-TEST-META-TOKENS
   RXT-TEST-TOKEN-TABLES
   RXT-TEST-SINGLE-TOKEN-EMIT
   RXT-TEST-ESCAPES
   RXT-TEST-CLASSES
   RXT-TEST-CLASS-ESCAPE
   RXT-TEST-MATCH-LITERALS
   RXT-TEST-MATCH-META
   RXT-TEST-MATCH-CLASSES
   RXT-TEST-MATCH-ESCAPED
   RXT-TEST-MATCH-REPEATS
   RXT-TEST-FIND
   RXT-TEST-COUNT
   RXT-TEST-THROWS
   RXT-REPORT ;

RXT-MAIN
