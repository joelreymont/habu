\ argv-test.f -- focused tests for lib/argv.f.
\ Run mocks:  cat lib/errors.f lib/string.f lib/argv.f lib/argv-test.f | bin/hb
\ Run script: cat lib/errors.f lib/string.f lib/argv.f lib/argv-test.f > /tmp/hb-argv-test.f
\             bin/hb /tmp/hb-argv-test.f --json --label NAME --strict-signatures --all-errors --strict-boundary -o OUT -- file.f --literal

require lib/errors.f
require lib/string.f
require lib/argv.f

\ White-box test: reopen the module's package so the fixtures reach argv's
\ private path buffer/capacity (ARGV-PATH-BUF, ARGV-PATH-CAP) and call the public
\ API by its bare package-local tails (PARSE, OUT$, E-USAGE, ...).
package ARGV

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
   0 QUIET!
   = ASSERT ;

: PARSE-RC ( -- n )  [: PARSE ;] catch ;

: EXPECT-ONE-POS ( -- )  1 EXPECT-POS-EXACT ;

: ONE-POS-RC ( -- n )
   [: PARSE ;] catch dup 0 <> if exit then drop
   [: EXPECT-ONE-POS ;] catch ;

: NEED-OUT-RC ( -- n )
   [: PARSE ;] catch dup 0 <> if exit then drop
   [: REQUIRE-OUT ;] catch ;

: NEED-LABEL-RC ( -- n )
   [: PARSE ;] catch dup 0 <> if exit then drop
   [: REQUIRE-LABEL ;] catch ;

: QUIET-MOCK ( -- )
   MOCK-CLEAR
   -1 QUIET! ;

: COMMON-ARGS ( -- )
   QUIET-MOCK
   s" --json" MOCK+
   s" --label" MOCK+
   s" NAME" MOCK+
   s" --strict-signatures" MOCK+
   s" --all-errors" MOCK+
   s" --strict-boundary" MOCK+
   s" -o" MOCK+
   s" OUT" MOCK+
   s" file.f" MOCK+ ;

: TEST-COMMON ( -- )
   COMMON-ARGS
   s" usage" USAGE!
   s" DEFAULT" LABEL-DEFAULT!
   s" STDOUT" OUT-DEFAULT!
   PARSE
   JSON? ASSERT
   STRICT-SIGNATURES? ASSERT
   ALL-ERRORS? ASSERT
   STRICT-BOUNDARY? ASSERT
   LABEL? ASSERT
   OUT? ASSERT
   POS# 1 ASSERT=
   0 POS$ s" file.f" ASSERT$
   LABEL$ s" NAME" ASSERT$
   OUT$ s" OUT" ASSERT$
   0 POSZ dup ZLEN s" file.f" ASSERT$ ;

: TEST-DEFAULTS ( -- )
   QUIET-MOCK
   s" file.f" MOCK+
   s" DEFAULT" LABEL-DEFAULT!
   s" STDOUT" OUT-DEFAULT!
   PARSE
   LABEL? 0= ASSERT
   OUT? 0= ASSERT
   LABEL$ s" DEFAULT" ASSERT$
   OUT$ s" STDOUT" ASSERT$ ;

: TEST-DASHDASH ( -- )
   QUIET-MOCK
   s" --" MOCK+
   s" --json" MOCK+
   s" -o" MOCK+
   PARSE
   JSON? 0= ASSERT
   POS# 2 ASSERT=
   0 POS$ s" --json" ASSERT$
   1 POS$ s" -o" ASSERT$ ;

: TEST-JSON-ERRORS ( -- )
   QUIET-MOCK
   s" --json-errors" MOCK+
   s" file.f" MOCK+
   PARSE
   JSON? ASSERT
   POS# 1 ASSERT=
   0 POS$ s" file.f" ASSERT$ ;

: TEST-STRING-ORDER ( -- )
   QUIET-MOCK
   s" first.f" MOCK+
   s" --label" MOCK+
   s" LABEL" MOCK+
   s" second.f" MOCK+
   s" -o" MOCK+
   s" OUT" MOCK+
   PARSE
   LABEL$ s" LABEL" ASSERT$
   OUT$ s" OUT" ASSERT$
   POS# 2 ASSERT=
   0 POS$ s" first.f" ASSERT$
   1 POS$ s" second.f" ASSERT$ ;

: TEST-UNKNOWN ( -- )
   QUIET-MOCK
   s" --wat" MOCK+
   PARSE-RC E-USAGE ASSERT-RC ;

: TEST-MISSING-LABEL ( -- )
   QUIET-MOCK
   s" --label" MOCK+
   PARSE-RC E-USAGE ASSERT-RC ;

: TEST-MISSING-OUT ( -- )
   QUIET-MOCK
   s" -o" MOCK+
   PARSE-RC E-USAGE ASSERT-RC ;

: TEST-POS-LOW ( -- )
   QUIET-MOCK
   ONE-POS-RC E-USAGE ASSERT-RC ;

: TEST-POS-HIGH ( -- )
   QUIET-MOCK
   s" a.f" MOCK+
   s" b.f" MOCK+
   ONE-POS-RC E-USAGE ASSERT-RC ;

: TEST-REQUIRE-OUT ( -- )
   QUIET-MOCK
   s" file.f" MOCK+
   NEED-OUT-RC E-USAGE ASSERT-RC ;

: TEST-REQUIRE-LABEL ( -- )
   QUIET-MOCK
   s" file.f" MOCK+
   NEED-LABEL-RC E-USAGE ASSERT-RC ;

: ZCOPY-NEG-CASE ( -- )
   s" x" drop -1 ARGV-PATH-BUF ARGV-PATH-CAP ZCOPY drop ;

: ZCOPY-FULL-CAP-CASE ( -- )
   ARGV-PATH-BUF ARGV-PATH-CAP ARGV-PATH-BUF ARGV-PATH-CAP ZCOPY drop ;

: TEST-ZCOPY-NEG ( -- )
   [: ZCOPY-NEG-CASE ;] catch E-INTERNAL ASSERT-RC ;

: TEST-ZCOPY-FULL-CAP ( -- )
   [: ZCOPY-FULL-CAP-CASE ;] catch E-INTERNAL ASSERT-RC ;

: TEST-MOCKS ( -- )
   TEST-COMMON
   TEST-DEFAULTS
   TEST-DASHDASH
   TEST-JSON-ERRORS
   TEST-STRING-ORDER
   TEST-UNKNOWN
   TEST-MISSING-LABEL
   TEST-MISSING-OUT
   TEST-POS-LOW
   TEST-POS-HIGH
   TEST-REQUIRE-OUT
   TEST-REQUIRE-LABEL
   TEST-ZCOPY-NEG
   TEST-ZCOPY-FULL-CAP ;

: TEST-SCRIPT-ARGS ( -- )
   s" hb argv-test [options] file.f" USAGE!
   s" DEFAULT" LABEL-DEFAULT!
   s" STDOUT" OUT-DEFAULT!
   PARSE
   2 EXPECT-POS-EXACT
   JSON? ASSERT
   STRICT-SIGNATURES? ASSERT
   ALL-ERRORS? ASSERT
   STRICT-BOUNDARY? ASSERT
   LABEL$ s" NAME" ASSERT$
   OUT$ s" OUT" ASSERT$
   0 POS$ s" file.f" ASSERT$
   1 POS$ s" --literal" ASSERT$ ;

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

;package
