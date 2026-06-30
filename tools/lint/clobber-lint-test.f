\ clobber-lint-test.f - focused regression tests for clobber-lint.
\ Load after tools/lint/clobber-lint.f.

require tools/lint/clobber-lint.f

: CLT-ASSERT ( bool -- )
   0= if s" clobber-lint-test failed" 1 die then ;

: CLT-FIXTURE-BAD# ( -- n )
   0 PARENS? !  0 CN# !  0 CEND !  0 EN# !
   s" tools/lint/clobber-sys-x8-fixture.f" PASS1-FILE
   CLOSE-CLOBBERS
   0 BAD !
   s" tools/lint/clobber-sys-x8-fixture.f" PASS2-FILE
   BAD @ ;

: CLT-SYS-CLOBBERS-X8 ( -- )
   CLT-FIXTURE-BAD# 2 = CLT-ASSERT ;

CLT-SYS-CLOBBERS-X8
s" clobber-lint-test: ok" type NL
