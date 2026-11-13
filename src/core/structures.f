\ structures.f - checked structure and field defining words. Each field name is
\ a child word sumtype.f's TDECL-CELL-CHILD, TDECL-PTR-CHILD or TDECL-BYTE-CHILD
\ makes: a fixed body that adds the field's offset, and a typed effect. A field
\ word reads no storage and names no base, so it means the same in every thread
\ and in every package. BEGIN-STRUCTURE stays create/does> (see its note).

$4C constant STRUCT-RC

variable STRUCT-ACTIVE

: STRUCT-BYTE+ ( ptr a n -- ptr u8 )
   swap BYTE-VIEW swap + ;

: STRUCT-REQUIRE-CLOSED ( -- )
   STRUCT-ACTIVE @ if s" structure: nested begin" STRUCT-RC die then ;

: STRUCT-REQUIRE-OPEN ( -- )
   STRUCT-ACTIVE @ 0= if s" structure: no active structure" STRUCT-RC die then ;

\ The field's name, which the live input holds next.
: STRUCT-NAME ( -- ptr u8 n )
   parse-name dup 0= if s" structure: missing name" STRUCT-RC die then ;

\ ---- the defining words -----------------------------------------------------

\ BEGIN-STRUCTURE publishes its size through create/does>: a generated
\ `data-base <off> + @` reads the calling thread's data region (register 20, the
\ task's own region under src/lib/task.f), so a size read inside a task faults
\ (measured). Checked source has no expression for the absolute address (`dbase@`
\ is typed `-- n`); END-STRUCTURE publishing the size as a literal would narrow
\ the three definers' `( ptr a n )` threading effects to `( n )`.
: BEGIN-STRUCTURE ( -- ptr n n )
   STRUCT-REQUIRE-CLOSED
   -1 STRUCT-ACTIVE !
   create here 0 , 0 does> ( -- n ) @ ;

: +FIELD ( ptr a n n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   over STRUCT-NAME rot TYPE-DECL:TDECL-CELL-CHILD
   + ;

: PTR-FIELD: ( ptr a n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   dup CELL mod 0 <> if s" structure: pointer field alignment" STRUCT-RC die then
   dup STRUCT-NAME rot TYPE-DECL:TDECL-PTR-CHILD
   CELL + ;

: CFIELD: ( ptr a n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   dup STRUCT-NAME rot TYPE-DECL:TDECL-BYTE-CHILD
   1 + ;

: END-STRUCTURE ( ptr n n -- )
   STRUCT-REQUIRE-OPEN
   0 STRUCT-ACTIVE !
   swap ! ;
