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

: CLT-WRAP-BAD# ( -- n )
   0 PARENS? !  0 CN# !  0 CEND !  0 EN# !  CLOBBER-CENSUS:RESET
   s" tools/lint/clobber-wrap-fixture.f" PASS1-FILE
   CLOSE-CLOBBERS
   0 BAD !
   s" tools/lint/clobber-wrap-fixture.f" PASS2-FILE
   BAD @ ;

: CLT-WRAPPED-CALLS ( -- )
   CLT-WRAP-BAD# 2 = CLT-ASSERT               \ positive + transitive flag; negatives clean
   CLOBBER-CENSUS:COUNTS {: routines:n calls:n :}
   routines 2 = CLT-ASSERT
   calls 4 = CLT-ASSERT ;

: CLT-WRAP-CONTRACTS ( -- )
   s" PROT-GUARD:CALL" CLOBBER-WRAP:WRAP? CLT-ASSERT
   s" BL," CLOBBER-WRAP:WRAP? 0= CLT-ASSERT
   s" PROT-GUARD:CALL" 10 7 CLOBBER-WRAP:MASK
      0 11 CL-ADD 12 CL-ADD 13 CL-ADD = CLT-ASSERT   \ addr=x10 kept, len=x7 -> x11
   s" PROT-GUARD:CALL" 9 11 CLOBBER-WRAP:MASK
      0 10 CL-ADD 12 CL-ADD 13 CL-ADD = CLT-ASSERT   \ addr=x9 -> x10, len=x11 kept
   s" PROT-GUARD:CALL" 9 7 CLOBBER-WRAP:READS
      0 9 CL-ADD 7 CL-ADD = CLT-ASSERT
   s" PROT-GUARD:CALL" CLOBBER-WRAP:RETURNS
      0 10 CL-ADD 11 CL-ADD = CLT-ASSERT ;

: CLT-WRAP-UNMODELED ( -- )                   \ an unmodeled :CALL shape fails closed
   [: s" FFI-GUARD:CALL" 9 7 CLOBBER-WRAP:MASK drop ;] catch
      E-CLOBBER-WRAP-UNRESOLVED = CLT-ASSERT
   [: s" FFI-GUARD:CALL" 9 7 CLOBBER-WRAP:READS drop ;] catch
      E-CLOBBER-WRAP-UNRESOLVED = CLT-ASSERT
   [: s" FFI-GUARD:CALL" CLOBBER-WRAP:RETURNS drop ;] catch
      E-CLOBBER-WRAP-UNRESOLVED = CLT-ASSERT ;

CLT-SYS-CLOBBERS-X8
CLT-CURRENT-SYNTAX-CENSUS
CLT-LABEL-ACCESSORS
CLT-MACHINE-CONTRACTS
CLT-WRAP-CONTRACTS
CLT-WRAP-UNMODELED
CLT-WRAPPED-CALLS
s" clobber-lint-test: ok" type NL
