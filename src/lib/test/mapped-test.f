split-str
\ mapped-test.f - the mapped-span witness must answer both ways.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f
\ src/lib/test/mapped.f src/lib/test/mapped-test.f

require src/lib/error.f
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
   MPT-SPAN MEM-ALLOC-64K-SPAN {: a:^ acap:n :}
   MPT-SPAN MEM-ALLOC-64K-SPAN {: b:^ bcap:n :}
   a MAPPED:LIVE? TTRUE
   b MAPPED:LIVE? TTRUE
   a acap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   a MAPPED:LIVE? TFALSE
   b MAPPED:LIVE? TTRUE
   b bcap MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES
   b MAPPED:LIVE? TFALSE
   MPT-STATIC MAPPED:LIVE? TTRUE ;

1 TYPED-BUFFER MPT-KEPT ^u8

: MPT-PAGE ( -- NUM:alloc-byte-len ) 4096 MEM:BYTES-ALLOC-LEN ;

\ A body that keeps one page is caught; one that frees the page it maps is not.
: MPT-RELEASES ( -- )
   [: MPT-PAGE MEM:ALLOC-BYTES drop 0 MPT-KEPT ! ;] MAPPED:RELEASED? TFALSE
   0 MPT-KEPT @ MPT-PAGE MEM:RELEASE-BYTES
   [: MPT-PAGE MEM:ALLOC-BYTES MEM:RELEASE-BYTES ;] MAPPED:RELEASED? TTRUE ;

: MPT-MAIN ( -- )
   T-RESET
   MPT-CASES
   MPT-RELEASES
   T-REPORT
   s" mapped-test: ok" type cr ;

MPT-MAIN

;package
