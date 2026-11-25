\ structures.f - checked structure and field defining words. Each field name is
\ a child word sumtype.f's TDECL-CELL-CHILD, TDECL-PTR-CHILD or TDECL-BYTE-CHILD
\ makes: a fixed body that adds the field's offset, and a typed effect. A field
\ word reads no storage and names no base, so it means the same in every thread
\ and in every package. BEGIN-STRUCTURE stays create/does> (see its note).

package STRUCTURES
EXPORT TYPE-DECL:TDECL-CELL-CHILD
EXPORT TYPE-DECL:TDECL-PTR-CHILD
EXPORT TYPE-DECL:TDECL-BYTE-CHILD
EXPORT SOURCE-ROOT:OPENED
EXPORT SOURCE-ROOT:CLOSED

global

\ The dictionary count just past the open structure's record; 0 when none is open.
variable STRUCT-OPEN

: STRUCT-BYTE+ ( ^a n -- ^u8 )
   swap BYTE-VIEW swap + ;

\ w names the word that runs, for the rejection.
: STRUCT-REQUIRE-OPEN ( str -- ) {: w:str :}
   STRUCT-OPEN @ 0 <> if exit then
   s" E-BAD-DECLARATION" REJECTION:OPEN
   96 REJECTION:EMIT1  w REJECTION:EMIT-STR
   s" ` is outside a structure" REJECTION:EMIT-STR
   SRCLOC:TOKEN$ >str REJECTION:AT  s" no structure open" REJECTION:EMIT-STR
   REJECTION:HELP  s" open a structure with `BEGIN-STRUCTURE name` before `" REJECTION:EMIT-STR
   w REJECTION:EMIT-STR  96 REJECTION:EMIT1
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ The field word w, at the source token t, has no name after it. It is
\ written through its help, and the caller stops it.
: STRUCT-NO-NAME ( str str -- ) {: w:str t:str :}
   w s" a field name" t s" no name follows"
   REJECTION:MISSING-OPERAND
   s" write the field's name after the word, as `" REJECTION:EMIT-STR
   w REJECTION:EMIT-STR  s"  tag`" REJECTION:EMIT-STR ;

\ The field's name, which the live input holds next; with none left the
\ source token is the field word, as the source spells it.
: STRUCT-NAME ( -- str )
   parse-name dup STR:LEN 0= if
      SRCLOC:TOKEN$ >str SRCLOC:TOKEN$ >str STRUCT-NO-NAME
      EXIT-STATUS:REJECTED REJECTION:STOP
   then ;

: STRUCT-UNALIGNED ( n str -- ) {: off:n fs:str :}
   s" E-BAD-DECLARATION" REJECTION:OPEN
   s" pointer field `" REJECTION:EMIT-STR  fs REJECTION:EMIT-STR
   s" ` starts at byte " REJECTION:EMIT-STR  off REJECTION:EMIT-U
   s" , not on a cell boundary" REJECTION:EMIT-STR
   fs REJECTION:AT  s" not on a cell boundary" REJECTION:EMIT-STR
   s" offset" REJECTION:NOTE  off REJECTION:EMIT-U
   s" cell" REJECTION:NOTE  CELL REJECTION:EMIT-U
   REJECTION:HELP
   s" declare `PTR-FIELD:` fields before byte fields, or pad to a multiple of "
   REJECTION:EMIT-STR  CELL REJECTION:EMIT-U  s"  with `+FIELD`" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ ---- the defining words -----------------------------------------------------

: STRUCT-ABANDON ( -- )
   0 STRUCT-OPEN ! ;

\ BEGIN-STRUCTURE publishes its size through create/does>: a generated
\ `data-base <off> + @` reads the calling thread's data region (register 20, the
\ task's own region under src/lib/task.f), so a size read inside a task faults
\ (measured). Checked source has no expression for the absolute address (`dbase@`
\ is typed `-- n`); END-STRUCTURE publishing the size as a literal would narrow
\ the three definers' `( ^a n )` threading effects to `( n )`. The loader's
\ abandon action is a named word: a quotation before `create` is a bend, and
\ `does>` needs its `create` before any bend.
: BEGIN-STRUCTURE ( -- ^n n )
   SRCLOC:TOKEN$ >str s" END-STRUCTURE" ['] STRUCT-ABANDON OPENED
   create ndict@ STRUCT-OPEN !
   here 0 , 0 does> ( -- n ) @ ;

: +FIELD ( ^a n n -- ^a n )
   s" +FIELD" STRUCT-REQUIRE-OPEN
   over STRUCT-NAME swap TDECL-CELL-CHILD
   + ;

: PTR-FIELD: ( ^a n -- ^a n )
   s" PTR-FIELD:" STRUCT-REQUIRE-OPEN
   dup STRUCT-NAME {: off:n fs:str :}
   off CELL mod 0 <> if off fs STRUCT-UNALIGNED then
   fs off TDECL-PTR-CHILD
   CELL + ;

: CFIELD: ( ^a n -- ^a n )
   s" CFIELD:" STRUCT-REQUIRE-OPEN
   dup STRUCT-NAME swap TDECL-BYTE-CHILD
   1 + ;

: END-STRUCTURE ( ^n n -- )
   s" END-STRUCTURE" STRUCT-REQUIRE-OPEN
   0 STRUCT-OPEN !  CLOSED
   swap ! ;

;package
