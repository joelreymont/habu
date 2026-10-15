\ Pause after growth to the inline boundary, then deny every further mapping
\ (Linux only; the parent row skips elsewhere). The allocation that then fails
\ is the engine's.
require src/lib/ffi-abi.f
require src/habu/address-cells.f
package ADDRESS-CELL-OOM-CHILD
using ADDRESS-CELLS
PROCESS-SYMBOLS
FUNCTION: SET-LIMIT setrlimit ( n ptr u8 -- i32 ) ;FUNCTION
9 constant RLIMIT-AS                 \ Linux RLIMIT_AS
create SLOTS BOOT-CAP 1+ cells allot
create LIMIT 16 allot                \ struct rlimit: rlim_cur, rlim_max

\ RLIMIT_AS {0, 0}: the kernel refuses every mapping that would grow the
\ address space past the limit, which is now every mapping. The call's first
\ resolution (FFI:FN-LOOKUP) runs the loader before the syscall, so nothing
\ maps between the cap and the allocation under test.
: LIMIT-AS! ( -- )
   0 LIMIT ! 0 LIMIT cell+ !
   RLIMIT-AS LIMIT SET-LIMIT 0 <> if
      s" address-cell-oom: setrlimit failed" 76 die then ;

: RUN ( -- )
   CURRENT? 0= if s" address-cell-oom: wrong host ABI" 76 die then
   BOOT-CAP LIVE-SPAN nip - {: remaining:n :}
   remaining 0 <= if s" address-cell-oom: no inline room" 76 die then
   remaining 0 ?do SLOTS i cells + ptr-cell-mark loop
   s" address-cell-oom: ready" type cr
   LIMIT-AS!
   \ Existing exact duplicates need no row or index allocation at capacity.
   SLOTS ptr-cell-mark
   SLOTS remaining 1- cells + ptr-cell-mark
   LIVE-SPAN nip BOOT-CAP <> if
      s" address-cell-oom: duplicate changed count" 76 die then
   s" address-cell-oom: duplicate-ok" type cr
   SLOTS remaining cells + ptr-cell-mark
   s" address-cell-oom: unexpected allocation success" 76 die ;
RUN
;using
;package
