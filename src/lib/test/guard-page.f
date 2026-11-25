\ guard-page.f - test-only caller bytes that end where readable memory ends.
\ A word that must bound a length before it reads is proved to by bytes followed
\ by an inaccessible page: claimed one past the tail, a read before the bound
\ faults and a bound that runs first rejects. The page sits between two
\ inaccessible ones (MEM:ALLOC-GUARDED), mapped once and refilled on every call.
\ STORAGE CLASS. PROCESS-WIDE: one task uses the guarded page at a time.

require src/lib/memory.f
require src/core/exit-status.f

package GUARD-PAGE
private

PTR-VARIABLE PAGE-A

: PAGE@ ( -- ^u8 )
   PAGE-A @ ;

: PAGE ( -- ^u8 )
   PAGE@ 0= if
      STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED drop PAGE-A !
   then
   PAGE@ ;

public

\ The last u bytes before the inaccessible page, each set to c. The page holds
\ 0 to PAGE-BYTES of them; more would write into the guard below it.
: TAIL ( n n -- ^u8 ) {: u:n c:n :}
   u 0 < u STACK-ABI:PAGE-BYTES > or if
      s" guard-page: a tail is 0 to PAGE-BYTES bytes" EXIT-STATUS:FAILURE die
   then
   PAGE STACK-ABI:PAGE-BYTES u - + {: a:^ :}
   u 0 ?do c a i + c! loop
   a ;

;package
