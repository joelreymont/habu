\ argv-test.f -- focused tests for lib/argv.f.
\ Run mocks:  cat lib/argv.f lib/argv-test.f | bin/hb
\ Run script: cat lib/argv.f lib/argv-test.f > /tmp/hb-argv-test.f
\             bin/hb /tmp/hb-argv-test.f --json -o OUT -- file.f --literal

variable TEST-N
variable TEST-FAIL

: ASSERT ( f -- )
   TEST-N @ 1 + TEST-N !
   0= if
      s" argv-test: assertion " type TEST-N @ . s"  failed" type cr
      TEST-FAIL @ 1 + TEST-FAIL !
   then ;

: ASSERT= ( n n -- )  = ASSERT ;

: ASSERT$ ( ptr u8 n ptr u8 n -- )  STR= ASSERT ;

: ASSERT-RC ( n n -- )
   0 ARGV-QUIET!
   = ASSERT ;

: PARSE-RC ( -- n )  ['] ARGV-PARSE catch ;

: EXPECT-ONE-POS ( -- )  1 ARGV-EXPECT-POS-EXACT ;

: ONE-POS-RC ( -- n )
   ['] ARGV-PARSE catch dup 0 <> if exit then drop
   ['] EXPECT-ONE-POS catch ;

: NEED-OUT-RC ( -- n )
   ['] ARGV-PARSE catch dup 0 <> if exit then drop
   ['] ARGV-REQUIRE-OUT catch ;

: QUIET-MOCK ( -- )
   ARGV-MOCK-CLEAR
   -1 ARGV-QUIET! ;

: COMMON-ARGS ( -- )
   QUIET-MOCK
   s" --json" ARGV-MOCK+
   s" -o" ARGV-MOCK+
   s" OUT" ARGV-MOCK+
   s" file.f" ARGV-MOCK+ ;

: TEST-COMMON ( -- )
   COMMON-ARGS
   s" usage" ARGV-USAGE!
   s" STDOUT" ARGV-OUT-DEFAULT!
   ARGV-PARSE
   ARGV-JSON? ASSERT
   ARGV-OUT? ASSERT
   ARGV-POS# 1 ASSERT=
   0 ARGV-POS$ s" file.f" ASSERT$
   ARGV-OUT$ s" OUT" ASSERT$
   0 ARGV-POSZ dup ZLEN s" file.f" ASSERT$ ;

: TEST-DEFAULTS ( -- )
   QUIET-MOCK
   s" file.f" ARGV-MOCK+
   s" STDOUT" ARGV-OUT-DEFAULT!
   ARGV-PARSE
   ARGV-OUT? 0= ASSERT
   ARGV-OUT$ s" STDOUT" ASSERT$ ;

: TEST-DASHDASH ( -- )
   QUIET-MOCK
   s" --" ARGV-MOCK+
   s" --json" ARGV-MOCK+
   s" -o" ARGV-MOCK+
   ARGV-PARSE
   ARGV-JSON? 0= ASSERT
   ARGV-POS# 2 ASSERT=
   0 ARGV-POS$ s" --json" ASSERT$
   1 ARGV-POS$ s" -o" ASSERT$ ;

: TEST-UNKNOWN ( -- )
   QUIET-MOCK
   s" --wat" ARGV-MOCK+
   PARSE-RC ARGV-E-USAGE ASSERT-RC ;

: TEST-MISSING-OUT ( -- )
   QUIET-MOCK
   s" -o" ARGV-MOCK+
   PARSE-RC ARGV-E-USAGE ASSERT-RC ;

: TEST-POS-LOW ( -- )
   QUIET-MOCK
   ONE-POS-RC ARGV-E-USAGE ASSERT-RC ;

: TEST-POS-HIGH ( -- )
   QUIET-MOCK
   s" a.f" ARGV-MOCK+
   s" b.f" ARGV-MOCK+
   ONE-POS-RC ARGV-E-USAGE ASSERT-RC ;

: TEST-REQUIRE-OUT ( -- )
   QUIET-MOCK
   s" file.f" ARGV-MOCK+
   NEED-OUT-RC ARGV-E-USAGE ASSERT-RC ;

: TEST-MOCKS ( -- )
   TEST-COMMON
   TEST-DEFAULTS
   TEST-DASHDASH
   TEST-UNKNOWN
   TEST-MISSING-OUT
   TEST-POS-LOW
   TEST-POS-HIGH
   TEST-REQUIRE-OUT ;

: TEST-SCRIPT-ARGS ( -- )
   s" hb argv-test [options] file.f" ARGV-USAGE!
   s" STDOUT" ARGV-OUT-DEFAULT!
   ARGV-PARSE
   2 ARGV-EXPECT-POS-EXACT
   ARGV-JSON? ASSERT
   ARGV-OUT$ s" OUT" ASSERT$
   0 ARGV-POS$ s" file.f" ASSERT$
   1 ARGV-POS$ s" --literal" ASSERT$ ;

: REPORT ( -- )
   TEST-FAIL @ 0 = if
      s" argv-test: ok (" type TEST-N @ . s"  assertions)" type cr
   else
      s" argv-test: failures" 1 die
   then ;

: ARGV-TEST-MAIN ( -- )
   0 TEST-N !
   0 TEST-FAIL !
   SCRIPT-ARGC 0 > if TEST-SCRIPT-ARGS else TEST-MOCKS then
   REPORT ;

ARGV-TEST-MAIN
