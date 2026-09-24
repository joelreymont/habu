\ Real kernel mappings distinguish an inaccessible reservation from memory
\ temporarily protected against access. Neither classification uses addresses.
require lib/test.f
require lib/memory.f
require src/habu/proc-maps.f

package PROC-MAPS-TEST

PROCESS-SYMBOLS
FUNCTION: MACH-SELF task_self_trap ( -- n ) ;FUNCTION
FUNCTION: MACH-PROTECT mach_vm_protect ( n n n n n -- n ) ;FUNCTION

\ mach_vm_address_t is the integer representation of this allocation's pointer.
TRUSTED: ADDRESS ( ptr u8 -- n ) ;

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
   HB-TARGET-MACOS? if MACOS-REGIONS then
   T-REPORT ;

RUN
;package
