\ ptx-emitter-lint-test.f - focused fixtures for the lexer-defect reporting in
\ tools/lint/ptx-emitter-lint.f.
\
\ The lint is fail-closed on any lexer diagnostic, because a scan that stopped
\ early can hide a duplicate emitter definition. It now has two defects to report:
\ an unterminated string literal and a malformed `PRIM:`/`PPRIM:` primitive-axiom
\ row. Each finding must name the defect the scan actually hit - a malformed row
\ reported as an open string sends the reader hunting for a quote that is not
\ there - so these fixtures pin the dispatch, not just the finding count.
\
\ Run: bin/hb --load tools/lint/ptx-emitter-lint-test.f

require lib/test.f
require tools/lint/ptx-emitter-lint.f

package PTX-EMITTER-LINT-TOOL

: PELT-STRING-DEFECT$ ( -- ptr u8 n )  s" UNTERMINATED string literal" ;
: PELT-ROW-DEFECT$ ( -- ptr u8 n )  s" MALFORMED primitive registry row" ;

create PELT-UB 2 allot

\ A bare `s"` with no closing quote.
: PELT-UNTERM$ ( -- ptr u8 n )
   115 PELT-UB c!                \ 's'
   DQUOTE 1 PELT-UB + c!         \ '"'
   PELT-UB 2 ;

: PELT-SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   0 PE-BAD !
   s" [pelt-fixture]" PE-FILE!
   a u PE-SCAN ;

\ A `PRIM:` row whose closer never arrives: one finding, named as a registry row.
: PELT-MALFORMED-ROW ( -- )
   s" PRIM: FOO PE-N PE-IN" PELT-SCAN
   s" malformed row is a finding" T-LABEL
   PE-BAD @ 1 T=
   s" malformed row names the row defect" T-LABEL
   PE-LEX-DEFECT$ PELT-ROW-DEFECT$ T$=
   s" malformed row is not called an open string" T-LABEL
   PE-LEX-DEFECT$ PELT-STRING-DEFECT$ T$<> ;

\ The pre-existing defect keeps its own wording.
: PELT-UNTERM-STRING ( -- )
   PELT-UNTERM$ PELT-SCAN
   s" open string is a finding" T-LABEL
   PE-BAD @ 1 T=
   s" open string names the string defect" T-LABEL
   PE-LEX-DEFECT$ PELT-STRING-DEFECT$ T$= ;

\ A well-formed row is not a defect at all: the scan completes and reports nothing.
: PELT-GOOD-ROW ( -- )
   s" PRIM: FOO PE-N PE-IN PRIM;" PELT-SCAN
   s" well-formed row is clean" T-LABEL
   PE-BAD @ 0 T= ;

: PELT-MAIN ( -- )
   T-RESET
   PELT-MALFORMED-ROW
   PELT-UNTERM-STRING
   PELT-GOOD-ROW
   0 PE-BAD !
   T-REPORT
   s" ptx-emitter-lint-test: ok" type cr ;

PELT-MAIN

;package
