\ clobber-lint-test.f - focused regression tests for clobber-lint.
\ Load after tools/lint/clobber-lint.f.

require tools/lint/clobber-lint.f

: CLT-ASSERT ( bool -- )
   0= if s" clobber-lint-test failed" 1 die then ;

: CLT-FIXTURE-BAD# ( -- n )
   0 PARENS? !  0 CN# !  0 CEND !  0 EN# !  CLOBBER-CENSUS:RESET
   s" tools/lint/clobber-sys-x8-fixture.f" PASS1-FILE
   CLOSE-CLOBBERS
   0 BAD !
   s" tools/lint/clobber-sys-x8-fixture.f" PASS2-FILE
   BAD @ ;

: CLT-SYS-CLOBBERS-X8 ( -- )
   CLT-FIXTURE-BAD# 3 = CLT-ASSERT ;

: CLT-CURRENT-SYNTAX-CENSUS ( -- )
   CLOBBER-CENSUS:COUNTS {: routines:n calls:n :}
   routines 13 = CLT-ASSERT
   calls 6 = CLT-ASSERT ;

: CLT-LABEL-ACCESSORS ( -- )
   s" @" LABEL-ACCESS? 0= CLT-ASSERT
   s" LABEL@" LABEL-ACCESS? CLT-ASSERT
   s" OWNER:LABEL@" LABEL-ACCESS? CLT-ASSERT
   s" LABEL" LABEL-ACCESS? 0= CLT-ASSERT ;

: CLT-MACHINE-CONTRACTS ( -- )
   s" LP2CWAT" RETURNS-MASK 0 10 CL-ADD = CLT-ASSERT
   s" LCEMIT" PRESERVE-MASK 0 12 CL-ADD 13 CL-ADD = CLT-ASSERT
   s" LAOTWIDGATE" PRESERVE-MASK 0 11 CL-ADD = CLT-ASSERT
   s" LPROTWIDQ" PRESERVE-MASK 0 5 CL-ADD 6 CL-ADD 7 CL-ADD 14 CL-ADD = CLT-ASSERT
   s" LHIDXADD" PRESERVE-MASK
      0 2 CL-ADD 3 CL-ADD 4 CL-ADD 5 CL-ADD 6 CL-ADD 7 CL-ADD 8 CL-ADD
        14 CL-ADD 15 CL-ADD 16 CL-ADD 17 CL-ADD = CLT-ASSERT ;

CLT-SYS-CLOBBERS-X8
CLT-CURRENT-SYNTAX-CENSUS
CLT-LABEL-ACCESSORS
CLT-MACHINE-CONTRACTS
s" clobber-lint-test: ok" type NL
