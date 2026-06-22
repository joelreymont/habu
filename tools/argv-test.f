\ argv-test.f -- focused tests for tools/argv.f.
\ Run mocks:  cat tools/argv.f tools/argv-test.f | bin/hb
\ Run script: cat tools/argv.f tools/argv-test.f > /tmp/hb-argv-test.f
\             bin/hb /tmp/hb-argv-test.f --json --label NAME --strict-signatures --all-errors --strict-boundary -o OUT file.f

variable TEST-N
variable TEST-FAIL

: ASSERT ( f -- )
   TEST-N @ 1 + TEST-N !
   0= IF
      s" argv-test: assertion " type TEST-N @ . s"  failed" type cr
      TEST-FAIL @ 1 + TEST-FAIL !
   THEN ;

: ASSERT= ( n n -- )  = ASSERT ;

: ASSERT$ ( ptr u8 n ptr u8 n -- )  STR= ASSERT ;

: ASSERT-RC ( n n -- )
   0 ARGV-QUIET!
   = ASSERT ;

: PARSE-RC ( -- n )  [: ARGV-PARSE ;] catch ;

: EXPECT-ONE-POS ( -- )  1 ARGV-EXPECT-POS-EXACT ;

: ONE-POS-RC ( -- n )
   [: ARGV-PARSE ;] catch dup 0 <> IF EXIT THEN drop
   [: EXPECT-ONE-POS ;] catch ;

: NEED-OUT-RC ( -- n )
   [: ARGV-PARSE ;] catch dup 0 <> IF EXIT THEN drop
   [: ARGV-REQUIRE-OUT ;] catch ;

: NEED-LABEL-RC ( -- n )
   [: ARGV-PARSE ;] catch dup 0 <> IF EXIT THEN drop
   [: ARGV-REQUIRE-LABEL ;] catch ;

: QUIET-MOCK ( -- )
   ARGV-MOCK-CLEAR
   -1 ARGV-QUIET! ;

: COMMON-ARGS ( -- )
   QUIET-MOCK
   s" --json" ARGV-MOCK+
   s" --label" ARGV-MOCK+
   s" NAME" ARGV-MOCK+
   s" --strict-signatures" ARGV-MOCK+
   s" --all-errors" ARGV-MOCK+
   s" --strict-boundary" ARGV-MOCK+
   s" -o" ARGV-MOCK+
   s" OUT" ARGV-MOCK+
   s" file.f" ARGV-MOCK+ ;

: TEST-COMMON ( -- )
   COMMON-ARGS
   s" usage" ARGV-USAGE!
   s" DEFAULT" ARGV-LABEL-DEFAULT!
   s" STDOUT" ARGV-OUT-DEFAULT!
   ARGV-PARSE
   ARGV-JSON? ASSERT
   ARGV-STRICT-SIGNATURES? ASSERT
   ARGV-ALL-ERRORS? ASSERT
   ARGV-STRICT-BOUNDARY? ASSERT
   ARGV-LABEL? ASSERT
   ARGV-OUT? ASSERT
   ARGV-POS# 1 ASSERT=
   0 ARGV-POS$ s" file.f" ASSERT$
   ARGV-LABEL$ s" NAME" ASSERT$
   ARGV-OUT$ s" OUT" ASSERT$
   0 ARGV-POSZ dup ZLEN s" file.f" ASSERT$ ;

: TEST-DEFAULTS ( -- )
   QUIET-MOCK
   s" file.f" ARGV-MOCK+
   s" DEFAULT" ARGV-LABEL-DEFAULT!
   s" STDOUT" ARGV-OUT-DEFAULT!
   ARGV-PARSE
   ARGV-LABEL? 0= ASSERT
   ARGV-OUT? 0= ASSERT
   ARGV-LABEL$ s" DEFAULT" ASSERT$
   ARGV-OUT$ s" STDOUT" ASSERT$ ;

: TEST-DASHDASH ( -- )
   QUIET-MOCK
   s" --" ARGV-MOCK+
   s" --json" ARGV-MOCK+
   ARGV-PARSE
   ARGV-JSON? 0= ASSERT
   ARGV-POS# 1 ASSERT=
   0 ARGV-POS$ s" --json" ASSERT$ ;

: TEST-JSON-ERRORS ( -- )
   QUIET-MOCK
   s" --json-errors" ARGV-MOCK+
   s" file.f" ARGV-MOCK+
   ARGV-PARSE
   ARGV-JSON? ASSERT
   ARGV-POS# 1 ASSERT=
   0 ARGV-POS$ s" file.f" ASSERT$ ;

: TEST-UNKNOWN ( -- )
   QUIET-MOCK
   s" --wat" ARGV-MOCK+
   PARSE-RC ARGV-E-USAGE ASSERT-RC ;

: TEST-MISSING-LABEL ( -- )
   QUIET-MOCK
   s" --label" ARGV-MOCK+
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

: TEST-REQUIRE-LABEL ( -- )
   QUIET-MOCK
   s" file.f" ARGV-MOCK+
   NEED-LABEL-RC ARGV-E-USAGE ASSERT-RC ;

: TEST-MOCKS ( -- )
   TEST-COMMON
   TEST-DEFAULTS
   TEST-DASHDASH
   TEST-JSON-ERRORS
   TEST-UNKNOWN
   TEST-MISSING-LABEL
   TEST-MISSING-OUT
   TEST-POS-LOW
   TEST-POS-HIGH
   TEST-REQUIRE-OUT
   TEST-REQUIRE-LABEL ;

: TEST-SCRIPT-ARGS ( -- )
   s" hb argv-test [options] file.f" ARGV-USAGE!
   s" DEFAULT" ARGV-LABEL-DEFAULT!
   s" STDOUT" ARGV-OUT-DEFAULT!
   ARGV-PARSE
   1 ARGV-EXPECT-POS-EXACT
   ARGV-JSON? ASSERT
   ARGV-STRICT-SIGNATURES? ASSERT
   ARGV-ALL-ERRORS? ASSERT
   ARGV-STRICT-BOUNDARY? ASSERT
   ARGV-LABEL$ s" NAME" ASSERT$
   ARGV-OUT$ s" OUT" ASSERT$
   0 ARGV-POS$ s" file.f" ASSERT$ ;

: REPORT ( -- )
   TEST-FAIL @ 0 = IF
      s" argv-test: ok (" type TEST-N @ . s"  assertions)" type cr
   ELSE
      s" argv-test: failures" 1 die
   THEN ;

: ARGV-TEST-MAIN ( -- )
   0 TEST-N !
   0 TEST-FAIL !
   SCRIPT-ARGC 0 > IF TEST-SCRIPT-ARGS ELSE TEST-MOCKS THEN
   REPORT ;

ARGV-TEST-MAIN
