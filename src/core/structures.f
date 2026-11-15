\ structures.f - checked structure and field defining words. Each field name is
\ a child word sumtype.f's TDECL-CELL-CHILD, TDECL-PTR-CHILD or TDECL-BYTE-CHILD
\ makes: a fixed body that adds the field's offset, and a typed effect. A field
\ word reads no storage and names no base, so it means the same in every thread
\ and in every package. BEGIN-STRUCTURE stays create/does> (see its note).

\ The dictionary count just past the open structure's record; 0 when none is open.
variable STRUCT-OPEN

: STRUCT-BYTE+ ( ptr a n -- ptr u8 )
   swap BYTE-VIEW swap + ;

: STRUCT-OPEN$ ( -- ptr u8 n )
   STRUCT-OPEN @ 1 - CHECKER-RESOLVE:REC-AT CHECKER-RESOLVE:CK-REC-NAME$ ;

: STRUCT-REQUIRE-CLOSED ( -- )
   STRUCT-OPEN @ 0= if exit then
   s" E-BAD-DECLARATION" REJECTION:OPEN
   s" `BEGIN-STRUCTURE` opens a structure while `" REJECTION:EMIT-STR
   STRUCT-OPEN$ REJECTION:EMIT-STR  s" ` is still open" REJECTION:EMIT-STR
   SRCLOC:TOKEN$ REJECTION:AT
   STRUCT-OPEN$ REJECTION:EMIT-STR  s"  has no END-STRUCTURE yet" REJECTION:EMIT-STR
   REJECTION:HELP  s" close `" REJECTION:EMIT-STR  STRUCT-OPEN$ REJECTION:EMIT-STR
   s" ` with `END-STRUCTURE` before the next `BEGIN-STRUCTURE`" REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

\ w names the word that runs, for the rejection.
: STRUCT-REQUIRE-OPEN ( ptr u8 n -- ) {: w:ptr wu:n :}
   STRUCT-OPEN @ 0 <> if exit then
   s" E-BAD-DECLARATION" REJECTION:OPEN
   96 REJECTION:EMIT1  w wu REJECTION:EMIT-STR
   s" ` is outside a structure" REJECTION:EMIT-STR
   SRCLOC:TOKEN$ REJECTION:AT  s" no structure open" REJECTION:EMIT-STR
   REJECTION:HELP  s" open a structure with `BEGIN-STRUCTURE name` before `" REJECTION:EMIT-STR
   w wu REJECTION:EMIT-STR  96 REJECTION:EMIT1
   CHECKER-REJECT-RC REJECTION:STOP ;

\ w names the field word that has no name after it.
: STRUCT-NO-NAME ( ptr u8 n -- ) {: w:ptr wu:n :}
   w wu s" a field name" SRCLOC:TOKEN$ s" no name follows"
   REJECTION:MISSING-OPERAND
   s" write the field's name after the word, as `" REJECTION:EMIT-STR
   w wu REJECTION:EMIT-STR  s"  tag`" REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

\ The field's name, which the live input holds next; w names the field word.
: STRUCT-NAME ( ptr u8 n -- ptr u8 n ) {: w:ptr wu:n :}
   parse-name dup 0= if w wu STRUCT-NO-NAME then ;

: STRUCT-UNALIGNED ( n ptr u8 n -- ) {: off:n fa:ptr fu:n :}
   s" E-BAD-DECLARATION" REJECTION:OPEN
   s" pointer field `" REJECTION:EMIT-STR  fa fu REJECTION:EMIT-STR
   s" ` starts at byte " REJECTION:EMIT-STR  off REJECTION:EMIT-U
   s" , not on a cell boundary" REJECTION:EMIT-STR
   fa fu REJECTION:AT  s" not on a cell boundary" REJECTION:EMIT-STR
   s" offset" REJECTION:NOTE  off REJECTION:EMIT-U
   s" cell" REJECTION:NOTE  CELL REJECTION:EMIT-U
   REJECTION:HELP
   s" declare `PTR-FIELD:` fields before byte fields, or pad to a multiple of "
   REJECTION:EMIT-STR  CELL REJECTION:EMIT-U  s"  with `+FIELD`" REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

\ ---- the defining words -----------------------------------------------------

\ BEGIN-STRUCTURE publishes its size through create/does>: a generated
\ `data-base <off> + @` reads the calling thread's data region (register 20, the
\ task's own region under src/lib/task.f), so a size read inside a task faults
\ (measured). Checked source has no expression for the absolute address (`dbase@`
\ is typed `-- n`); END-STRUCTURE publishing the size as a literal would narrow
\ the three definers' `( ptr a n )` threading effects to `( n )`.
: BEGIN-STRUCTURE ( -- ptr n n )
   STRUCT-REQUIRE-CLOSED
   create ndict@ STRUCT-OPEN !
   here 0 , 0 does> ( -- n ) @ ;

: +FIELD ( ptr a n n -- ptr a n )
   s" +FIELD" STRUCT-REQUIRE-OPEN
   over s" +FIELD" STRUCT-NAME rot TYPE-DECL:TDECL-CELL-CHILD
   + ;

: PTR-FIELD: ( ptr a n -- ptr a n )
   s" PTR-FIELD:" STRUCT-REQUIRE-OPEN
   dup s" PTR-FIELD:" STRUCT-NAME {: off:n fa:ptr fu:n :}
   off CELL mod 0 <> if off fa fu STRUCT-UNALIGNED then
   fa fu off TYPE-DECL:TDECL-PTR-CHILD
   CELL + ;

: CFIELD: ( ptr a n -- ptr a n )
   s" CFIELD:" STRUCT-REQUIRE-OPEN
   dup s" CFIELD:" STRUCT-NAME rot TYPE-DECL:TDECL-BYTE-CHILD
   1 + ;

: END-STRUCTURE ( ptr n n -- )
   s" END-STRUCTURE" STRUCT-REQUIRE-OPEN
   0 STRUCT-OPEN !
   swap ! ;
