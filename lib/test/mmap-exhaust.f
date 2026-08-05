\ mmap-exhaust.f - exhaust mappings and census process VM pages.

require lib/memory.f
require lib/fs.f

package MMAP-TEST
private

$5002 constant NORESERVE-MAP
1 44 lshift constant MAP-START
$80 constant STATM-CAP

create STATM-BUF STATM-CAP allot

: MAP-ONE ( n -- bool )
   0 swap MEM-PROT-RW NORESERVE-MAP MEM-ANON-FD MEM-OFF-ZERO mmap
   0 >= ;

: FILL-MAPS ( n -- )
   begin dup MAP-ONE while repeat drop ;

: STATM-TOK-U ( ptr u8 n n -- n ) {: a:ptr u:n i:n :}
   i u = if i exit then
   a i + c@ STR-SPACE <= if i exit then
   a u i 1+ recurse ;

: STATM-N ( ptr u8 n n -- n ) {: a:ptr u:n acc:n :}
   u 0= if acc exit then
   a c@ {: c:n :}
   c STR-DIGIT? 0= if E-FS-IO throw then
   a 1+ u 1- acc 10 * c STR-ZERO - + recurse ;

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

: VM-PAGES ( -- n )
   s" /proc/self/statm" STATM-BUF STATM-CAP READ-ALL {: u:n :}
   STATM-BUF u 0 STATM-TOK-U {: tokenu:n :}
   tokenu 0= if E-FS-IO throw then
   STATM-BUF tokenu 0 STATM-N ;

;package
