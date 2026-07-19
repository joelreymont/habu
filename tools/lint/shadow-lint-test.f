\ shadow-lint-test.f - focused definer-classification fixtures.

require lib/test.f
require tools/lint/shadow-lint.f

package SHADOW-LINT-TOOL

\ Definer classification runs on the string-aware lexer: a LAYOUT-BUFFER token is
\ a definer and the token after it (DUP) is the defined name, a real prim.
: SLT-LAYOUT-BUFFER ( -- )
   s" 1 LAYOUT-BUFFER DUP family" LEX-SOURCE
   1 DEF-NAME-OFFSET 1 T=
   2 LEX-TOK s" DUP" LINT-STR= TTRUE
   2 LEX-TOK PRIM? TTRUE ;

\ PART A: a definer keyword (`variable`) and a prim name (`or`) inside a string
\ literal must not be read as a definition. The fixture is global scope, so only
\ the string-body skip can keep the finding count at zero.
: SLT-STRING-LITERAL ( -- )
   0 BAD !
   s" tools/lint/shadow-string-fixture.f" LINT-FILE
   BAD @ 0 T= ;

\ PART B negative: two packages each define a tail named like the prim `dup`.
\ Package-local tails are scoped and cannot clobber the global prim, so neither
\ is flagged.
: SLT-CROSS-PACKAGE ( -- )
   0 BAD !
   s" [cross-package]"
   s" package SLFA : DUP ; ;package  package SLFB : DUP ; ;package"
   LINT-SCAN
   BAD @ 0 T= ;

\ PART B positive: a global-scope definition named like the prim `dup` really
\ clobbers the global word and must still be flagged.
: SLT-GLOBAL-SHADOW ( -- )
   0 BAD !
   s" [global-shadow]" s" : DUP ( n -- n n ) ;" LINT-SCAN
   BAD @ 1 T= ;

create SLT-UB 2 allot

\ A bare `s"` opener with no closing quote: the lexer marks it unterminated.
: SLT-UNTERM$ ( -- ptr u8 n )
   115 SLT-UB c!                \ 's'
   DQUOTE 1 SLT-UB + c!         \ '"'
   SLT-UB 2 ;

\ Fail-closed: an unterminated string literal must halt the scan with the named
\ diagnostic code, never silently drop the rest of the source.
: SLT-UNTERM ( -- )
   [: s" [unterm-fixture]" SLT-UNTERM$ LINT-SCAN ;] catch E-SHADOW-UNTERM T= ;

: SLT-MAIN ( -- )
   T-RESET
   SLT-LAYOUT-BUFFER
   SLT-STRING-LITERAL
   SLT-CROSS-PACKAGE
   SLT-GLOBAL-SHADOW
   SLT-UNTERM
   T-REPORT
   s" shadow-lint-test: ok" type cr ;

SLT-MAIN

;package
