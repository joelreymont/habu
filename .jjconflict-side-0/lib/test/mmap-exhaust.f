\ mmap-exhaust.f - exhaust anonymous VM mappings in a fork child.

require lib/memory.f

package MMAP-TEST
private

$5002 constant NORESERVE-MAP
1 44 lshift constant MAP-START

: MAP-ONE ( n -- bool )
   0 swap MEM-PROT-RW NORESERVE-MAP MEM-ANON-FD MEM-OFF-ZERO mmap
   0 >= ;

: FILL-MAPS ( n -- )
   begin dup MAP-ONE while repeat drop ;

public

: EXHAUST-CHILD ( n -- ) {: floor:n :}
   MAP-START
   begin dup floor > while
      dup FILL-MAPS 2 /
   repeat
   drop
   floor FILL-MAPS ;

: EXHAUSTED? ( n -- bool )
   MAP-ONE 0= ;

;package
