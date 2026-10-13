\ mapped-test.f - the mapped-span witness must answer both ways.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f
\ src/lib/test/mapped.f src/lib/test/mapped-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/test/mapped.f

package MAPPED-TEST

$10000 constant MPT-SPAN

create MPT-STATIC MPT-SPAN allot

\ Two spans, released one at a time: a witness that answered a constant, or that
\ went blind once any release happened, fails one of these six assertions.
: MPT-CASES ( -- )
   MPT-STATIC MAPPED:LIVE? TTRUE
   MPT-SPAN MEM-ALLOC-64K-SPAN {: a:ptr acap:n :}
   MPT-SPAN MEM-ALLOC-64K-SPAN {: b:ptr bcap:n :}
   a MAPPED:LIVE? TTRUE
   b MAPPED:LIVE? TTRUE
   a acap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   a MAPPED:LIVE? TFALSE
   b MAPPED:LIVE? TTRUE
   b bcap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   b MAPPED:LIVE? TFALSE
   MPT-STATIC MAPPED:LIVE? TTRUE ;

: MPT-MAIN ( -- )
   T-RESET
   MPT-CASES
   T-REPORT
   s" mapped-test: ok" type cr ;

MPT-MAIN

;package
