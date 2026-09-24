\ clobber-lint-test.f - focused regression tests for clobber-lint.

require tools/lint/clobber-lint.f

package CLOBBER

: CLT-ASSERT ( bool -- )
   0= if s" clobber-lint-test failed" 1 die then ;

: CLT-FIXTURE-BAD# ( -- n )
   0 PARENS? !  0 CN# !  0 CEND !  0 EN# !  CLOBBER-CENSUS:RESET
   s" tools/lint/clobber-sys-x8-fixture.f" DECL-FILE
   s" tools/lint/clobber-sys-x8-fixture.f" PASS1-FILE
   CLOSE-CLOBBERS
   0 BAD !
   s" tools/lint/clobber-sys-x8-fixture.f" PASS2-FILE
   BAD @ ;

: CLT-SYS-CLOBBERS-X8 ( -- )
   CLT-FIXTURE-BAD# 11 = CLT-ASSERT ;

: CLT-QUALIFIED-LABELS ( -- )
   s" LPROT" START-L? CLT-ASSERT
   s" PROT:LOPEN" START-L? CLT-ASSERT
   s" SNAP-RELOC:LCALLS" START-L? CLT-ASSERT
   s" A:B:LTAIL" START-L? CLT-ASSERT
   s" LHEAD:OPEN" START-L? 0= CLT-ASSERT
   s" PROT:OPEN" START-L? 0= CLT-ASSERT
   s" PROT:" START-L? 0= CLT-ASSERT
   s" CP" START-L? 0= CLT-ASSERT ;

: CLT-LABEL-ACCESSORS ( -- )
   s" @" LABEL-ACCESS? 0= CLT-ASSERT
   s" LABEL@" LABEL-ACCESS? CLT-ASSERT
   s" OWNER:LABEL@" LABEL-ACCESS? CLT-ASSERT
   s" LABEL" LABEL-ACCESS? 0= CLT-ASSERT ;

: CLT-WRAP-BAD# ( -- n )
   0 PARENS? !  0 CN# !  0 CEND !  0 EN# !  CLOBBER-CENSUS:RESET
   s" tools/lint/clobber-wrap-fixture.f" DECL-FILE
   s" tools/lint/clobber-wrap-fixture.f" PASS1-FILE
   CLOSE-CLOBBERS
   0 BAD !
   s" tools/lint/clobber-wrap-fixture.f" PASS2-FILE
   BAD @ ;

: CLT-WRAPPED-CALLS ( -- )
   CLT-WRAP-BAD# 3 = CLT-ASSERT               \ two guards and a stale global-lookup input
   CLOBBER-CENSUS:COUNTS {: routines:n calls:n :}
   routines 2 = CLT-ASSERT
   calls 6 = CLT-ASSERT ;

: CLT-WRAP-UNMODELED ( -- )                   \ an unmodeled :CALL shape fails closed
   [: s" FFI-GUARD:CALL" 9 7 CLOBBER-WRAP:MASK drop ;] catch
      E-CLOBBER-WRAP-UNRESOLVED = CLT-ASSERT
   [: s" FFI-GUARD:CALL" 9 7 CLOBBER-WRAP:READS drop ;] catch
      E-CLOBBER-WRAP-UNRESOLVED = CLT-ASSERT
   [: s" FFI-GUARD:CALL" 9 7 CLOBBER-WRAP:RETURNS drop ;] catch
      E-CLOBBER-WRAP-UNRESOLVED = CLT-ASSERT ;

: CLT-LITERAL-BOUNDARIES ( -- )
   0 PARENS? ! 0 CN# ! 0 CEND ! 0 EN# ! CLOBBER-CENSUS:RESET
   s" tools/lint/clobber-literal-fixture.f" DECL-FILE
   s" tools/lint/clobber-literal-fixture.f" PASS1-FILE
   CLOSE-CLOBBERS 0 BAD !
   s" tools/lint/clobber-literal-fixture.f" PASS2-FILE
   BAD @ 1 = CLT-ASSERT
   CLOBBER-CENSUS:COUNTS {: routines:n calls:n :}
   routines 0 = CLT-ASSERT
   calls 2 = CLT-ASSERT ;

CLT-SYS-CLOBBERS-X8
CLT-LABEL-ACCESSORS
CLT-QUALIFIED-LABELS
CLT-WRAP-UNMODELED
CLT-WRAPPED-CALLS
CLT-LITERAL-BOUNDARIES
s" clobber-lint-test: ok" type NL
;package
