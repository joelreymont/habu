\ Real kernel mappings distinguish an inaccessible reservation from memory
\ temporarily protected against access. Neither classification uses addresses.
require lib/test.f
require lib/memory.f
require lib/os-memory.f
require src/habu/proc-maps.f

\ Extending a region after the first Mach query makes the second reply start
\ before its cursor. Exercise that kernel response and the mapped lookup.
package PROC-MAPS
private

: TEST-ADDR ( ptr u8 -- n ) NULL-PTR BYTE-VIEW - ;

: TEST-MACH-QUERY ( n -- n n )
   MACH-ADDR LE:U64!
   0 MACH-DEPTH LE:U32!
   MACH-INFO-COUNT MACH-COUNT LE:U32!
   MACH-SELF MACH-ADDR MACH-SIZE MACH-DEPTH MACH-INFO MACH-COUNT MACH-REGION 0 T=
   MACH-INFO MACH-SUBMAP-OFF + LE:U32@ 0 T=
   MACH-ADDR LE:U64@ dup MACH-SIZE LE:U64@ + ;

public

: TEST-MACH-EXTENSION ( -- )
   OS-MEMORY:PAGE-SIZE {: page:n :}
   MEM-64K page + MEM-ALLOC-BYTES {: span:ptr spanlen:n :}
   span MEM-64K + {: edge:ptr :}
   edge page munmap 0 T=
   span TEST-ADDR TEST-MACH-QUERY {: first-lo:n first-hi:n :}
   first-hi edge TEST-ADDR T=
   edge TEST-ADDR page MEM-PROT-RW MEM-MAP-PRIVATE-ANON-FIXED
      MEM-ANON-FD MEM-OFF-ZERO mmap edge TEST-ADDR T=
   edge TEST-ADDR TEST-MACH-QUERY {: lo:n hi:n :}
   lo first-lo T=
   hi edge page + TEST-ADDR T=
   edge TEST-ADDR lo hi MACH-ROW-LO edge TEST-ADDR T=
   RELOAD
   span TEST-ADDR MAPPED? TTRUE
   edge TEST-ADDR MAPPED? TTRUE
   edge page 1- + TEST-ADDR MAPPED? TTRUE
   span spanlen munmap 0 T= ;

;package

package PROC-MAPS-TEST

: ADDRESS ( ptr u8 -- n ) NULL-PTR BYTE-VIEW - ;

\ A fragmented live map spans many /proc/self/maps reads and forces all four
\ area tables to grow. The result must still classify its live mappings.
4096 constant LINUX-PAGES
4 constant LINUX-STRIDE

: LINUX-GROWTH ( -- )
   OS-MEMORY:PAGE-SIZE {: page:n :}
   LINUX-PAGES page * MEM-ALLOC-BYTES drop {: span:ptr :}
   LINUX-PAGES LINUX-STRIDE / 0 ?do
      span i LINUX-STRIDE * 1+ page * + {: gap:ptr :}
      gap page LINUX-STRIDE 1- * munmap 0 T=
   loop
   PROC-MAPS:RELOAD
   PROC-MAPS:EXTENTS LINUX-PAGES LINUX-STRIDE / >= TTRUE
   LINUX-PAGES LINUX-STRIDE / 0 ?do
      span i LINUX-STRIDE * page * + ADDRESS PROC-MAPS:MAPPED? TTRUE
   loop
   LINUX-PAGES LINUX-STRIDE / 0 ?do
      span i LINUX-STRIDE * page * + page munmap 0 T=
   loop ;

PROCESS-SYMBOLS
FUNCTION: MACH-SELF task_self_trap ( -- u32 ) ;FUNCTION
FUNCTION: MACH-PROTECT mach_vm_protect ( n n n n n -- i32 ) ;FUNCTION

\ mach_vm_address_t is the integer representation of this allocation's pointer:
\ its distance from the null address.
: MACOS-REGIONS ( -- )
   MEM-ALLOC-64K {: live:ptr liveu:n :}
   MEM-ALLOC-64K {: blocked:ptr blocku:n :}
   MEM-ALLOC-64K {: reserved:ptr reservu:n :}
   MACH-SELF {: task:n :}
   task blocked ADDRESS blocku 0 0 MACH-PROTECT 0 T=
   task reserved ADDRESS reservu 1 0 MACH-PROTECT 0 T=
   PROC-MAPS:RELOAD
   live ADDRESS PROC-MAPS:MAPPED? TTRUE
   live liveu 1- + ADDRESS PROC-MAPS:MAPPED? TTRUE
   blocked ADDRESS PROC-MAPS:MAPPED? TTRUE
   reserved ADDRESS PROC-MAPS:MAPPED? TFALSE
   reserved reservu 1- + ADDRESS PROC-MAPS:MAPPED? TFALSE
   \ PROT_NONE alone is reversible: the pointer remains a process allocation.
   task blocked ADDRESS blocku 0 MEM-PROT-RW MACH-PROTECT 0 T=
   42 blocked c! blocked c@ 42 T=
   live liveu munmap 0 T=
   blocked blocku munmap 0 T=
   reserved reservu munmap 0 T= ;

: RUN ( -- )
   T-RESET
   HB-TARGET-LINUX-KERNEL? if LINUX-GROWTH then
   HB-TARGET-MACOS? if
      PROC-MAPS:TEST-MACH-EXTENSION
      MACOS-REGIONS
   then
   T-REPORT ;

RUN
;package
