\ manifest-lint-test.f - checked fixtures for the engine manifest lint.
\ HOSTILE holds near misses of a require edge (a comment, a path inside a
\ generated source string, a word starting with `require`, tokens out of order):
\ counting one would pass a manifest row that should fail.
\ Fixtures compose `"` from a named byte, so no fixture line reads as a require row.
\ Run: bin/hb --load tools/manifest-lint-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require tools/lint/text.f
require tools/lint/lib.f
require tools/manifest-lint-core.f

package MANIFEST-LINT-TEST
private

$400 constant MLT-CAP
create MLT-BUF MLT-CAP allot
variable MLT-U

34 constant MLT-QUOTE
10 constant MLT-LF

: MLT-RESET ( -- ) 0 MLT-U ! ;

: MLT-C+ ( n -- ) {: c:n :}
   MLT-U @ MLT-CAP >= if s" manifest-lint-test: fixture overflow" 70 die then
   c MLT-BUF MLT-U @ + c!
   MLT-U @ 1+ MLT-U ! ;

: MLT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ MLT-C+
      1+
   repeat drop ;

: MLT-Q ( -- ) MLT-QUOTE MLT-C+ ;
: MLT-NL ( -- ) MLT-LF MLT-C+ ;
: MLT-SP ( -- ) 32 MLT-C+ ;
: MLT-INDENT ( -- ) MLT-SP MLT-SP MLT-SP ;

\ `s" <path>"`, byte for byte, without writing a quote inside a literal here.
: MLT-QUOTED+ ( ptr u8 n -- ) {: a:ptr u:n :}
   s" s" MLT+ MLT-Q MLT-SP
   a u MLT+
   MLT-Q ;

: MLT$ ( -- ptr u8 n ) MLT-BUF MLT-U @ ;

: MLT-EDGES ( -- n ) MLT$ MANIFEST-LINT:EDGES-IN ;

\ ---- the two spellings the tree really uses ------------------------------------

: PLAIN-REQUIRE ( -- )
   MLT-RESET
   s" require src/lib/one.f" MLT+
   MLT-EDGES 1 T= ;

: INDENTED-REQUIRE ( -- )
   MLT-RESET
   s"    require src/lib/one.f" MLT+
   MLT-EDGES 1 T= ;

: QUOTED-REQUIRED ( -- )                       \ s" src/lib/two.f" required
   MLT-RESET
   s" src/lib/two.f" MLT-QUOTED+  s"  required" MLT+
   MLT-EDGES 1 T= ;

: INDENTED-QUOTED-REQUIRED ( -- )              \ the shape inside LOAD-REPL-TERM
   MLT-RESET
   MLT-INDENT  s" src/os/linux/repl-term.f" MLT-QUOTED+  s"  required" MLT+
   MLT-EDGES 1 T= ;

: BOTH-SPELLINGS ( -- )
   MLT-RESET
   s" require src/lib/one.f" MLT+ MLT-NL
   s" src/lib/two.f" MLT-QUOTED+  s"  required" MLT+
   MLT-EDGES 2 T= ;

: DUPLICATE-IS-ONE-PATH ( -- )                 \ the table interns, it does not count lines
   MLT-RESET
   s" require src/lib/one.f" MLT+ MLT-NL
   s" require src/lib/one.f" MLT+ MLT-NL
   s" require src/lib/one.f" MLT+
   MLT-EDGES 1 T= ;

: LAST-LINE-WITHOUT-LF ( -- )                  \ a file that does not end in a newline
   MLT-RESET
   s" require src/lib/one.f" MLT+ MLT-NL
   s" require src/lib/two.f" MLT+
   MLT-EDGES 2 T= ;

\ ---- the near misses -----------------------------------------------------------

: COMMENTED-REQUIRE ( -- )
   MLT-RESET
   s" \ require src/lib/one.f" MLT+
   MLT-EDGES 0 T= ;

: COMMENTED-QUOTED ( -- )
   MLT-RESET
   s" \ " MLT+  s" src/lib/two.f" MLT-QUOTED+  s"  required" MLT+
   MLT-EDGES 0 T= ;

: REQUIRE-INSIDE-A-SOURCE-STRING ( -- )        \ s" 0 set-tier require src/lib/x.f" GE-SRC-LINE
   MLT-RESET
   s" 0 set-tier require src/lib/one.f" MLT-QUOTED+  s"  GE-SRC-LINE" MLT+
   MLT-EDGES 0 T= ;

: QUOTED-WITHOUT-REQUIRED ( -- )               \ s" src/lib/two.f" provided
   MLT-RESET
   s" src/lib/two.f" MLT-QUOTED+  s"  provided" MLT+
   MLT-EDGES 0 T= ;

: LONGER-WORD-IS-NOT-REQUIRE ( -- )
   MLT-RESET
   s" requires src/lib/one.f" MLT+
   MLT-EDGES 0 T= ;

: REQUIRE-NOT-LEADING ( -- )
   MLT-RESET
   s" src/lib/one.f require" MLT+
   MLT-EDGES 0 T= ;

: BARE-REQUIRE-HAS-NO-PATH ( -- )
   MLT-RESET
   s" require" MLT+
   MLT-EDGES 0 T= ;

: EMPTY-TEXT ( -- )
   MLT-RESET
   MLT-EDGES 0 T= ;

: HOSTILE ( -- )
   COMMENTED-REQUIRE
   COMMENTED-QUOTED
   REQUIRE-INSIDE-A-SOURCE-STRING
   QUOTED-WITHOUT-REQUIRED
   LONGER-WORD-IS-NOT-REQUIRE
   REQUIRE-NOT-LEADING
   BARE-REQUIRE-HAS-NO-PATH
   EMPTY-TEXT ;

: SHAPES ( -- )
   PLAIN-REQUIRE
   INDENTED-REQUIRE
   QUOTED-REQUIRED
   INDENTED-QUOTED-REQUIRED
   BOTH-SPELLINGS
   DUPLICATE-IS-ONE-PATH
   LAST-LINE-WITHOUT-LF ;

\ ---- the live tree -------------------------------------------------------------
\ A row added to src/habu/native-runtime.f that nothing requires fails here.

: LIVE ( -- )
   [: MANIFEST-LINT:STRICT ;] catch 0 T= ;

\ Without this case LIVE proves only that the lint is quiet.
: STRAY-IS-FOUND ( -- )
   s" tools/manifest-lint-stray-fixture.f" MANIFEST-LINT:RUN-MANIFEST
   MANIFEST-LINT:FINDINGS 1 T= ;

: MAIN ( -- )
   T-RESET
   SHAPES
   HOSTILE
   STRAY-IS-FOUND
   LIVE
   T-REPORT ;

MAIN

;package
