\ regex-test.f - focused tests for bounded regex scanner tokens.
\ Run: cat lib/errors.f lib/string.f lib/test.f lib/regex.f lib/regex-test.f | bin/hb

1 constant RXT-EX-FAIL
32 constant RXT-BUF-CAP
3 constant RXT-SMALL-CAP

create RXT-BUF RXT-BUF-CAP allot
create RXT-SMALL RXT-SMALL-CAP allot

: RXT-COMPILE ( ptr u8 n -- n )
   RXT-BUF RXT-BUF-CAP RX-COMPILE ;

: RXT-B@ ( n -- n ) {: ix :}
   RXT-BUF ix + c@ ;

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
   s" ab" RXT-SMALL RXT-SMALL-CAP RX-COMPILE drop ;

: RXT-NEG-CAP ( -- )
   s" a" RXT-BUF -1 RX-COMPILE drop ;

: RXT-TEST-THROWS ( -- )
   ['] RXT-DANGLING-ESCAPE catch E-RX-SYNTAX T=
   ['] RXT-BAD-ESCAPE catch E-RX-SYNTAX T=
   ['] RXT-UNCLOSED-CLASS catch E-RX-SYNTAX T=
   ['] RXT-EMPTY-CLASS catch E-RX-SYNTAX T=
   ['] RXT-EMPTY-NEG-CLASS catch E-RX-SYNTAX T=
   ['] RXT-DANGLING-CLASS-ESCAPE catch E-RX-SYNTAX T=
   ['] RXT-CAP-OVERFLOW catch E-RX-CAPACITY T=
   ['] RXT-NEG-CAP catch E-RX-CAPACITY T= ;

: RXT-REPORT ( -- )
   T-FAILURES 0= if s" regex-test: ok" type cr exit then
   T-FAILURES . s" regex-test: failures" type cr
   s" regex-test: failures" RXT-EX-FAIL die ;

: RXT-MAIN ( -- )
   T-RESET
   RXT-TEST-LITERALS
   RXT-TEST-META-TOKENS
   RXT-TEST-ESCAPES
   RXT-TEST-CLASSES
   RXT-TEST-CLASS-ESCAPE
   RXT-TEST-THROWS
   RXT-REPORT ;

RXT-MAIN
