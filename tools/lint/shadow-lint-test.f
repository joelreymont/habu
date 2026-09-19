\ shadow-lint-test.f - focused definer-classification fixtures.

require lib/test.f
require tools/lint/shadow-lint.f

package SHADOW-LINT-TOOL

\ Definer classification runs on the string-aware lexer: a LAYOUT-BUFFER token is
\ a definer and the token after it (DUP) is the defined name, a real prim.
: SLT-LAYOUT-BUFFER ( -- )
   s" 1 LAYOUT-BUFFER DUP family" LINT-LEX:SOURCE
   1 DEF-NAME-OFFSET 1 T=
   2 LINT-LEX:TOKEN s" DUP" LINT-STR= TTRUE
   2 LINT-LEX:TOKEN PRIM? TTRUE ;

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

: SLT-DEFINER-BODY ( -- )
   0 BAD !
   s" [definer-body]" s" : MAKE-SLOT create here ptr-cell-mark 0 , ;" LINT-SCAN
   BAD @ 0 T=
   s" [after-definer]" s" : MAKE-SLOT create here ; variable DUP" LINT-SCAN
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

\ A `PRIM:` row with no closer is the lexer's other fail-closed defect. It must
\ throw the registry code, not the unterminated-string code: naming it as an open
\ quote would send the reader looking for a quote that is not there.
: SLT-MALFORMED-ROW ( -- )
   [: s" [registry-fixture]" s" PRIM: FOO PE-N PE-IN" LINT-SCAN ;] catch
   dup E-SHADOW-UNTERM <> TTRUE
   E-SHADOW-REGISTRY T= ;

create SLT-DIAG 256 allot

: SLT-ROW-CAPACITY ( -- )
   0 PN# ! 0 PEND !
   PMAX 0 ?do s" x" ADD-PRIM loop
   PN# @ PMAX T= PEND @ PMAX T=
   90 PNAMES PMAX + c!
   SLT-DIAG 256 LINT-OUT-BUFFER!
   [: s" overflow" ADD-PRIM ;] catch
   LINT-OUT-BUFFER-OFF E-SHADOW-CAPACITY T=
   LINT-OUT$ s" shadow-lint: prim rows capacity exceeded (count 512, ceiling 512)"
   LINT-STARTS-WITH? TTRUE
   PN# @ PMAX T= PEND @ PMAX T=
   PNAMES PMAX + c@ 90 T=
   PMAX 1- POFF @ 1 s" x" T$=
   PRIM-LEN PMAX 1- cells + @ 1 T= ;

: SLT-NAME-CAPACITY ( -- )
   0 PN# ! 0 PEND !
   PNAMES-CAP 0 ?do 65 PNAMES i + c! loop
   PNAMES PNAMES-CAP ADD-PRIM
   PN# @ 1 T= PEND @ PNAMES-CAP T=
   SLT-DIAG 256 LINT-OUT-BUFFER!
   [: s" x" ADD-PRIM ;] catch
   LINT-OUT-BUFFER-OFF E-SHADOW-CAPACITY T=
   LINT-OUT$ s" shadow-lint: name bytes capacity exceeded (count 8192, ceiling 8192)"
   LINT-STARTS-WITH? TTRUE
   PN# @ 1 T= PEND @ PNAMES-CAP T=
   0 POFF @ c@ 65 T= PRIM-LEN @ PNAMES-CAP T=
   \ A name longer than the remaining space is refused even before full.
   0 PN# ! 0 PEND ! s" x" ADD-PRIM
   90 PNAMES 1+ c!
   SLT-DIAG 256 LINT-OUT-BUFFER!
   [: PNAMES PNAMES-CAP ADD-PRIM ;] catch
   LINT-OUT-BUFFER-OFF E-SHADOW-CAPACITY T=
   PN# @ 1 T= PEND @ 1 T= PNAMES 1+ c@ 90 T=
   SLT-DIAG 256 LINT-OUT-BUFFER!
   [: PNAMES -1 ADD-PRIM ;] catch
   LINT-OUT-BUFFER-OFF E-SHADOW-CAPACITY T=
   PN# @ 1 T= PEND @ 1 T= ;

: SLT-MAIN ( -- )
   T-RESET
   SLT-LAYOUT-BUFFER
   SLT-STRING-LITERAL
   SLT-CROSS-PACKAGE
   SLT-GLOBAL-SHADOW
   SLT-DEFINER-BODY
   SLT-UNTERM
   SLT-MALFORMED-ROW
   SLT-ROW-CAPACITY
   SLT-NAME-CAPACITY
   s" src/habu/habu1.f" FB-LOAD TOKENIZE SCAN-PRIMS
   T-REPORT
   s" shadow-lint-test: ok" type cr ;

SLT-MAIN

;package
