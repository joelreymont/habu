\ guard-page.f - test-only caller bytes that end where readable memory ends.
\
\ A word that must bound a length before it reads the caller's bytes is proved
\ to by bytes followed by an inaccessible page: claimed one past what the tail
\ holds, a read that runs before the bound faults, and a bound that runs first
\ refuses. The page is one MEM-ALLOC-GUARDED keeps between two inaccessible
\ ones, mapped once per process and refilled on every call.

require lib/memory.f
require lib/test/assert.f                 \ T-EX-FAIL

package GUARD-PAGE
private

PTR-VARIABLE PAGE-A

: PAGE@ ( -- ptr u8 )
   PAGE-A @ ;

: PAGE ( -- ptr u8 )
   PAGE@ 0= if
      STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop PAGE-A !
   then
   PAGE@ ;

public

\ The last u bytes before the inaccessible page, each set to c. The page holds
\ 0 to PAGE-BYTES of them; more would write into the guard below it.
: TAIL ( n n -- ptr u8 ) {: u:n c:n :}
   u 0 < u STACK-ABI:PAGE-BYTES > or if
      s" guard-page: a tail is 0 to PAGE-BYTES bytes" T-EX-FAIL die
   then
   PAGE STACK-ABI:PAGE-BYTES u - + {: a:ptr :}
   u 0 ?do c a i + c! loop
   a ;

;package
