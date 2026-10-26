\ argv-test.f -- focused tests for src/lib/argv.f.
\ Run mocks:  cat src/lib/errors.f src/lib/string.f src/lib/argv.f src/lib/argv-test.f | bin/hb
\ Run script: cat src/lib/errors.f src/lib/string.f src/lib/argv.f src/lib/argv-test.f > /tmp/hb-argv-test.f
\             bin/hb /tmp/hb-argv-test.f --json --label NAME --all-errors --strict-boundary -o OUT -- file.f --literal

require src/lib/errors.f
require src/lib/string.f
require src/lib/argv.f
require src/lib/fmt.f                   \ FMT:.INT - report numbers inline

\ White-box test: reopen the module's package so the fixtures reach argv's
\ private path buffer/capacity (ARGV-PATH-BUF, ARGV-PATH-CAP) and call the public
\ API by its bare package-local tails (PARSE, OUT$, E-USAGE, ...).
package ARGV

variable TEST-N
variable TEST-FAIL

: ASSERT ( f -- )
   TEST-N @ 1 + TEST-N !
   0= if
      s" argv-test: assertion " type TEST-N @ FMT:.INT s"  failed" type cr
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

: EXPECT-TOKEN-BOUNDS ( -- )
   -1 QUIET!
   [: -1 TOK$ 2drop ;] catch E-USAGE ASSERT=
   [: COUNT TOK$ 2drop ;] catch E-USAGE ASSERT=
   0 QUIET! ;

: TEST-TOKEN-BOUNDS ( -- )
   QUIET-MOCK PARSE
   POS# 0 ASSERT=
   [: 0 POS$ 2drop ;] catch E-USAGE ASSERT=
   EXPECT-TOKEN-BOUNDS
   s" one" MOCK+
   0 TOK$ s" one" ASSERT$
   EXPECT-TOKEN-BOUNDS ;

: TEST-CONFIGURED-VALUES ( -- )
   RESET
   s" first" LABEL-DEFAULT!
   s" first" OUT-DEFAULT!
   LABEL$ s" first" ASSERT$
   OUT$ s" first" ASSERT$
   s" chosen" LABEL!
   s" chosen" OUT!
   s" second" LABEL-DEFAULT!
   s" second" OUT-DEFAULT!
   LABEL? ASSERT OUT? ASSERT
   LABEL$ s" chosen" ASSERT$
   OUT$ s" chosen" ASSERT$
   s" replaced" LABEL!
   s" replaced" OUT!
   LABEL$ s" replaced" ASSERT$
   OUT$ s" replaced" ASSERT$
   RESET
   LABEL? 0= ASSERT OUT? 0= ASSERT
   LABEL$ s" second" ASSERT$
   OUT$ s" second" ASSERT$ ;

: TEST-REPEATED-OPTIONS ( -- )
   QUIET-MOCK
   s" --label" MOCK+ s" first" MOCK+
   s" --label" MOCK+ s" last" MOCK+
   s" -o" MOCK+ s" first" MOCK+
   s" -o" MOCK+ s" last" MOCK+
   PARSE
   LABEL$ s" last" ASSERT$
   OUT$ s" last" ASSERT$
   LABEL? ASSERT OUT? ASSERT
   POS# 0 ASSERT=
   0 QUIET! ;

: ASSERT-PRIOR-PARSE ( -- )
   JSON? ASSERT
   ALL-ERRORS? ASSERT
   STRICT-BOUNDARY? ASSERT
   POS# 1 ASSERT=
   0 POS$ s" file.f" ASSERT$
   LABEL? ASSERT OUT? ASSERT
   LABEL$ s" NAME" ASSERT$
   OUT$ s" OUT" ASSERT$ ;

: TEST-PARSE-ROLLBACK ( -- )
   COMMON-ARGS PARSE
   QUIET-MOCK
   s" changed.f" MOCK+
   s" --label" MOCK+ s" changed" MOCK+
   s" --unknown" MOCK+
   PARSE-RC E-USAGE ASSERT=
   ASSERT-PRIOR-PARSE
   QUIET-MOCK
   s" changed.f" MOCK+
   s" -o" MOCK+
   PARSE-RC E-USAGE ASSERT=
   ASSERT-PRIOR-PARSE
   0 QUIET! ;

\ The mock list and positional table grow, so a list well past any one
\ allocation's first size parses whole, through the mock path and after `--`.
200 constant MANY-ARGS

: TEST-APPEND-BOUNDS ( -- )
   COMMON-ARGS PARSE
   QUIET-MOCK
   s" first" MOCK+
   MANY-ARGS 2 - 0 do s" middle" MOCK+ loop
   s" last" MOCK+
   COUNT MANY-ARGS ASSERT=
   ASSERT-PRIOR-PARSE
   0 TOK$ s" first" ASSERT$
   MANY-ARGS 1- TOK$ s" last" ASSERT$
   PARSE
   POS# MANY-ARGS ASSERT=
   0 POS$ s" first" ASSERT$
   MANY-ARGS 1- POS$ s" last" ASSERT$
   [: -1 POS$ 2drop ;] catch E-USAGE ASSERT=
   [: MANY-ARGS POS$ 2drop ;] catch E-USAGE ASSERT=
   QUIET-MOCK
   s" --json" MOCK+ s" --" MOCK+
   MANY-ARGS 0 do s" -rest" MOCK+ loop
   PARSE
   POS# MANY-ARGS ASSERT=
   MANY-ARGS 1- POS$ s" -rest" ASSERT$
   JSON? ASSERT
   EXPECT-TOKEN-BOUNDS
   QUIET-MOCK
   s" kept" MOCK+
   [: s" bad" drop -1 MOCK+ ;] catch E-INTERNAL ASSERT=
   COUNT 1 ASSERT=
   0 TOK$ s" kept" ASSERT$
   POS# MANY-ARGS ASSERT=
   MANY-ARGS 1- POS$ s" -rest" ASSERT$
   PARSE
   POS# 1 ASSERT=
   0 POS$ s" kept" ASSERT$
   0 QUIET! ;

: TEST-SPAN-REFUSALS ( -- )
   s" ARGV-SPAN-OK ( ptr u8 len -- span ) SPAN-MAKE"
      CHECK-QUIET-CANDIDATE! -1 ASSERT=
   s" ARGV-SPAN-SWAP ( len ptr u8 -- span ) SPAN-MAKE"
      CHECK-QUIET-CANDIDATE! 0 ASSERT=
   s" ARGV-SPAN-SCALAR ( n -- ) 0 ARGV-MOCK !"
      CHECK-QUIET-CANDIDATE! 0 ASSERT=
   s" ARGV-CONFIG-SCALAR ( span -- ) ARGV-LABEL !"
      CHECK-QUIET-CANDIDATE! 0 ASSERT= ;

\ FAIL writes its message, "usage: " and the usage text to stderr as they are,
\ so a long usage text is a usage refusal like a short one; a negative message
\ length is refused before anything is written.
4096 constant MSG-LEN
create MSG-TEXT MSG-LEN allot

: TEST-MSG-BOUNDS ( -- )
   MSG-LEN 0 do 117 MSG-TEXT i + c! loop
   MSG-TEXT MSG-LEN USAGE!
   -1 QUIET!
   [: s" x" FAIL ;] catch E-USAGE ASSERT-RC
   -1 QUIET!
   [: MSG-TEXT MSG-LEN FAIL ;] catch E-USAGE ASSERT-RC
   -1 QUIET!
   [: MSG-TEXT -1 FAIL ;] catch E-INTERNAL ASSERT-RC
   s" usage" USAGE! ;

: TEST-MOCKS ( -- )
   TEST-COMMON
   TEST-DEFAULTS
   TEST-DASHDASH
   TEST-JSON-ERRORS
   TEST-STRING-ORDER
   TEST-UNKNOWN
   TEST-MISSING-LABEL
   TEST-MISSING-OUT
   TEST-MSG-BOUNDS
   TEST-POS-LOW
   TEST-POS-HIGH
   TEST-REQUIRE-OUT
   TEST-REQUIRE-LABEL
   TEST-ZCOPY-NEG
   TEST-ZCOPY-FULL-CAP
   TEST-TOKEN-BOUNDS
   TEST-CONFIGURED-VALUES
   TEST-REPEATED-OPTIONS
   TEST-PARSE-ROLLBACK
   TEST-APPEND-BOUNDS
   TEST-SPAN-REFUSALS ;

: TEST-SCRIPT-ARGS ( -- )
   s" hb argv-test [options] file.f" USAGE!
   s" DEFAULT" LABEL-DEFAULT!
   s" STDOUT" OUT-DEFAULT!
   PARSE
   2 EXPECT-POS-EXACT
   JSON? ASSERT
   ALL-ERRORS? ASSERT
   STRICT-BOUNDARY? ASSERT
   LABEL$ s" NAME" ASSERT$
   OUT$ s" OUT" ASSERT$
   0 POS$ s" file.f" ASSERT$
   1 POS$ s" --literal" ASSERT$
   EXPECT-TOKEN-BOUNDS ;

: REPORT ( -- )
   TEST-FAIL @ 0 = if
      s" argv-test: ok (" type TEST-N @ FMT:.INT s"  assertions)" type cr
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
