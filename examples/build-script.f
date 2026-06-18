\ build-script.f - checked stdlib build-style script usage example.
\ Run: tools/examples-test.sh

: BS-CONFIGURE ( -- )
   s" hb build-script.f [--json] -o OUT SOURCE" ARGV-USAGE!
   s" build/app.hb" ARGV-OUT-DEFAULT! ;

: BS-VALIDATE-ARGS ( -- )
   ARGV-PARSE
   1 ARGV-EXPECT-POS-EXACT
   ARGV-REQUIRE-OUT ;

: BS-SOURCE-FILE? ( -- bool )
   0 ARGV-POS$ FILE? ;

: BS-OUT-HAS-SUFFIX? ( -- bool )
   ARGV-OUT$ s" .hb" ENDS-WITH? ;

: BS-OUT-NAME-OK? ( -- bool )
   ARGV-OUT$ BASENAME s" app.hb" STR= ;

: BS-MAIN ( -- )
   T-RESET
   BS-CONFIGURE
   BS-VALIDATE-ARGS
   ARGV-JSON? TTRUE
   BS-SOURCE-FILE? TTRUE
   BS-OUT-HAS-SUFFIX? TTRUE
   BS-OUT-NAME-OK? TTRUE
   T-REPORT ;

BS-MAIN
