\ os-memory-test.f - process page-size query and image-lifecycle proof.
require lib/test.f
require lib/os-memory.f
require lib/image-lifecycle.f
require lib/memory.f

package OS-MEMORY-TEST

: POWER-OF-TWO? ( n -- bool )
   dup 0 > swap dup 1- and 0= and ;

: POSITIVE ( -- )
   OS-MEMORY:PAGE-SIZE dup 0 > TTRUE
   POWER-OF-TWO? TTRUE ;

: PAGE-ALLOCATION ( -- )
   OS-MEMORY:PAGE-SIZE MEM-ALLOC-BYTES {: a:ptr au:n :}
   au OS-MEMORY:PAGE-SIZE T=
   a au MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: RECAPTURE ( -- )
   OS-MEMORY:PAGE-SIZE 0 > TTRUE
   IMAGE-LIFECYCLE:PREPARE
   OS-MEMORY:PAGE-SIZE 0 > TTRUE
   IMAGE-LIFECYCLE:PREPARE
   OS-MEMORY:PAGE-SIZE 0 > TTRUE ;

T-RESET
POSITIVE
PAGE-ALLOCATION
RECAPTURE
T-REPORT

;package
