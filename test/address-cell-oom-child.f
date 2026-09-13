\ Pause exactly before the first registry mmap. The Linux resource-limit test
\ constrains this child's address space after every source dependency has loaded.
require lib/errors.f
require src/habu/address-cells.f
package ADDRESS-CELL-OOM-CHILD
create SLOTS ADDRESS-CELLS:BOOT-CAP 1+ cells allot
create ACK 1 allot
: RUN ( -- )
   ADDRESS-CELLS:CURRENT? 0= if s" address-cell-oom: wrong host ABI" 76 die then
   ADDRESS-CELLS:BOOT-CAP ADDRESS-CELLS:LIVE-SPAN nip - {: remaining:n :}
   remaining 0 <= if s" address-cell-oom: no inline room" 76 die then
   remaining 0 ?do SLOTS i cells + ptr-cell-mark loop
   s" address-cell-oom: ready" type cr
   0 ACK 1 read 1 <> if s" address-cell-oom: missing limit handoff" 76 die then
   SLOTS remaining cells + ptr-cell-mark
   s" address-cell-oom: unexpected allocation success" 76 die ;
RUN
;package
