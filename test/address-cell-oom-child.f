\ Fill the inline table, then deny every further mapping (Linux only; the
\ parent test skips elsewhere). The mark that must grow the table throws
\ E-XTCELL-GROW: caught, the table still takes marks; uncaught, it ends the child.
require src/core/exit-status.f
require src/lib/ffi-abi.f
require src/habu/address-cells.f
package ADDRESS-CELL-OOM-CHILD
PROCESS-SYMBOLS
FUNCTION: SET-LIMIT setrlimit ( n ^u8 -- i32 ) ;FUNCTION
9 constant RLIMIT-AS                 \ Linux RLIMIT_AS
create SLOTS ADDRESS-CELLS:BOOT-CAP 1+ cells allot
create LIMIT 16 allot                \ struct rlimit: rlim_cur, rlim_max

\ RLIMIT_AS {0, 0}: every mapping fails. The call's first
\ resolution runs the loader before the syscall, so nothing maps between the cap
\ and the allocation under test.
: LIMIT-AS! ( -- )
   0 LIMIT ! 0 LIMIT cell+ !
   RLIMIT-AS LIMIT SET-LIMIT 0 <> if
      s" address-cell-oom: setrlimit failed" EXIT-STATUS:SYSTEM die then ;

: RUN ( -- )
   ADDRESS-CELLS:BOOT-CAP ADDRESS-CELLS:LIVE-SPAN nip - {: remaining:n :}
   remaining 0 <= if s" address-cell-oom: no inline room" EXIT-STATUS:FAILURE die then
   remaining 0 ?do SLOTS i cells + ptr-cell-mark loop
   s" address-cell-oom: ready" type cr
   LIMIT-AS!
   \ Existing exact duplicates need no row or index allocation at capacity.
   SLOTS ptr-cell-mark
   SLOTS remaining 1- cells + ptr-cell-mark
   ADDRESS-CELLS:LIVE-SPAN nip ADDRESS-CELLS:BOOT-CAP <> if
      s" address-cell-oom: duplicate changed count" EXIT-STATUS:FAILURE die then
   s" address-cell-oom: duplicate-ok" type cr
   [: SLOTS remaining cells + ptr-cell-mark ;] catch
   s" address-cell-oom: caught " type .
   \ A duplicate takes the table's lock: a lock the throw left held hangs here.
   SLOTS ptr-cell-mark
   s" address-cell-oom: after-catch-ok" type cr
   SLOTS remaining cells + ptr-cell-mark
   s" address-cell-oom: unexpected allocation success" EXIT-STATUS:FAILURE die ;
RUN
;package
