\ json-pointer-test.f - focused tests for JSON:POINTER? and JSON:POINTER-SPAN.
\ Run: bin/hb --load src/lib/json-pointer-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/json.f
require src/lib/json/pointer.f
require src/lib/json/read.f

package JSON-POINTER-TEST
private

\ RFC 6901 section 5's document.
: DOC$ ( -- str )
   s\" {\"foo\":[\"bar\",\"baz\"],\"\":0,\"a/b\":1,\"c%d\":2,\"e^f\":3,\"g|h\":4,\"i\\\\j\":5,\"k\\\"l\":6,\" \":7,\"m~n\":8}" ;

\ A slash, then a byte no UTF-8 text holds.
create BAD-UTF8 47 c, 255 c,
: BAD-UTF8$ ( -- str ) BAD-UTF8 2 >str ;

\ The bytes the pointer names in DOC$.
: PLACE$ ( str -- str )
   {: s:str :}
   DOC$ s JSON:POINTER-SPAN TTRUE {: at:n len:n :}
   DOC$ STR:DATA at + len >str ;

: NO-PLACE ( str -- )
   {: s:str :}
   DOC$ s JSON:POINTER-SPAN TFALSE 0 T= 0 T= ;

\ Section 5's twelve pointers and the values it gives for them.
: TEST-RFC ( -- )
   s" " PLACE$ DOC$ T$=
   s" /foo" PLACE$ s\" [\"bar\",\"baz\"]" T$=
   s" /foo/0" PLACE$ s\" \"bar\"" T$=
   s" /" PLACE$ s" 0" T$=
   s" /a~1b" PLACE$ s" 1" T$=
   s" /c%d" PLACE$ s" 2" T$=
   s" /e^f" PLACE$ s" 3" T$=
   s" /g|h" PLACE$ s" 4" T$=
   s\" /i\\j" PLACE$ s" 5" T$=
   s\" /k\"l" PLACE$ s" 6" T$=
   s" / " PLACE$ s" 7" T$=
   s" /m~0n" PLACE$ s" 8" T$= ;

: TEST-NO-PLACE ( -- )
   s" /foo/2" NO-PLACE
   s" /foo/-" NO-PLACE
   s" /foo/01" NO-PLACE
   s" /x" NO-PLACE
   s" /foo/0/x" NO-PLACE ;

: TEST-POINTER? ( -- )
   s" a" JSON:POINTER? TFALSE
   s" /~" JSON:POINTER? TFALSE
   s" /~2" JSON:POINTER? TFALSE
   BAD-UTF8$ JSON:POINTER? TFALSE
   s" " JSON:POINTER? TTRUE
   s" /m~0n" JSON:POINTER? TTRUE ;

\ The text is read whole, so a malformed one throws though the place is found.
: TEST-THROWS ( -- )
   [: DOC$ s" /~2" JSON:POINTER-SPAN drop 2drop ;] E-JSON-POINTER TTHROWSQ
   [: s\" {\"a\":1," s" /a" JSON:POINTER-SPAN drop 2drop ;]
   E-JSON-READ-EOF TTHROWSQ ;

public

: RUN ( -- )
   TEST-RFC
   TEST-NO-PLACE
   TEST-POINTER?
   TEST-THROWS ;

;package

T-RESET
JSON-POINTER-TEST:RUN
T-REPORT
s" json-pointer-test: ok" type cr
