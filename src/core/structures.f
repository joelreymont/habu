\ structures.f - checked structure and field defining words.
\
\ BEGIN-STRUCTURE threads a byte offset through +FIELD / PTR-FIELD: / CFIELD:
\ and closes it at END-STRUCTURE.
\
\ EVERY FIELD NAME IS NOW AN ORDINARY CHECKED COLON WORD. A field offset is known
\ when the field is declared, so the definer parses the name, bakes the offset into
\ ": NAME ( in -- out ) <off> ... ;" and hands that text to sumtype.f's audited
\ TDECL-EVAL-XT boundary - the shape src/core/layout-buffer.f and src/core/roles.f
\ CAST: already use. A field word is pure offset arithmetic on the address its
\ caller supplies: it reads no storage and names no base, so it means the same
\ thing in every thread.
\
\ BEGIN-STRUCTURE ITSELF STILL USES create/does>, because the size it publishes is
\ not known until END-STRUCTURE runs and must therefore be read from storage. See
\ the note on that word for why generated source cannot express that read yet.

$4C constant STRUCT-RC
$100 constant STRUCT-GEN-CAP

variable STRUCT-ACTIVE
create STRUCT-GEN STRUCT-GEN-CAP allot
variable STRUCT-GEN-U
variable STRUCT-GEN-I

: STRUCT-BYTE+ ( ptr a n -- ptr u8 )
   + ;

: STRUCT-REQUIRE-CLOSED ( -- )
   STRUCT-ACTIVE @ if s" structure: nested begin" STRUCT-RC die then ;

: STRUCT-REQUIRE-OPEN ( -- )
   STRUCT-ACTIVE @ 0= if s" structure: no active structure" STRUCT-RC die then ;

\ ---- generated-source buffer ------------------------------------------------

: STRUCT-GEN-CLEAR ( -- )
   0 STRUCT-GEN-U ! ;

: STRUCT-GEN-C, ( n -- ) {: c:n :}
   STRUCT-GEN-U @ STRUCT-GEN-CAP >= if
      s" structure: generated text too long" STRUCT-RC die
   then
   c STRUCT-GEN STRUCT-GEN-U @ + c!
   STRUCT-GEN-U @ 1 + STRUCT-GEN-U ! ;

: STRUCT-GEN-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   0 STRUCT-GEN-I !
   begin STRUCT-GEN-I @ u < while
      a STRUCT-GEN-I @ + c@ STRUCT-GEN-C,
      STRUCT-GEN-I @ 1 + STRUCT-GEN-I !
   repeat ;

: STRUCT-GEN-DEC, ( n -- ) {: v:n :}
   v 10 >= if v 10 / recurse then
   v 10 mod 48 + STRUCT-GEN-C, ;

\ TDECL-EVAL-XT is sumtype.f's audited evaluate boundary. It is a deferred word,
\ so an engine that never armed it (a stage builder) fails closed here with the
\ execution-vector error rather than defining anything.
: STRUCT-EVAL ( -- )
   STRUCT-GEN STRUCT-GEN-U @ TDECL-EVAL-XT ;

\ Open ": NAME" with the name the live input holds next. The name span is
\ transient, so it is copied into the buffer before anything else parses.
: STRUCT-GEN-OPEN ( -- )
   parse-name dup 0= if s" structure: missing name" STRUCT-RC die then
   {: name:ptr nameu:n :}
   STRUCT-GEN-CLEAR
   s" : " STRUCT-GEN-APP
   name nameu STRUCT-GEN-APP ;

\ ---- the defining words -----------------------------------------------------

\ NOT CONVERTED, and the reason is an address kind rather than a shape. The size
\ word is the only one here that reads STORAGE; the field words are pure offset
\ arithmetic. A generated `data-base <off> + @` reads the CALLING thread's data
\ region - register 20, which lib/task.f's thread entry points at the task's own
\ 64 KiB region - so a structure size read inside a task lands at region-base +
\ dictionary-offset and faults (measured). `create ... does>` publishes the
\ absolute address instead, and the tree has no checked source expression for one
\ (`dbase@` holds it, typed `-- n`). Converting this word needs either that
\ expression or END-STRUCTURE publishing the size as a LITERAL - which is the
\ better answer, since the size is a compile-time constant by then, but it narrows
\ the three definers' `( ptr a n )` threading effects to `( n )`. Both are open
\ design, not something this conversion may decide on its own.
: BEGIN-STRUCTURE ( -- ptr a n )
   STRUCT-REQUIRE-CLOSED
   -1 STRUCT-ACTIVE !
   create here 0 , 0 does> ( -- n ) @ ;

: STRUCT-CELL-FIELD ( n -- ) {: off:n :}
   STRUCT-GEN-OPEN
   s"  ( ptr a -- ptr a ) " STRUCT-GEN-APP
   off STRUCT-GEN-DEC,
   s"  + ;" STRUCT-GEN-APP
   STRUCT-EVAL ;

\ `ptr ptr b`, not `ptr ptr a`: the pointer a field HOLDS is independent of the
\ record's own element type, so a cell record can store a byte pointer. This
\ matches the `ptr-field` primitive, which is ( ptr a n -- ptr ptr b ).
: STRUCT-PTR-FIELD ( n -- ) {: off:n :}
   STRUCT-GEN-OPEN
   s"  ( ptr a -- ptr ptr b ) " STRUCT-GEN-APP
   off CELL / STRUCT-GEN-DEC,
   s"  ptr-field ;" STRUCT-GEN-APP
   STRUCT-EVAL ;

: STRUCT-BYTE-FIELD ( n -- ) {: off:n :}
   STRUCT-GEN-OPEN
   s"  ( ptr a -- ptr u8 ) " STRUCT-GEN-APP
   off STRUCT-GEN-DEC,
   s"  STRUCT-BYTE+ ;" STRUCT-GEN-APP
   STRUCT-EVAL ;

: +FIELD ( ptr a n n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   over STRUCT-CELL-FIELD
   + ;

: PTR-FIELD: ( ptr a n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   dup CELL mod 0 <> if s" structure: pointer field alignment" STRUCT-RC die then
   dup STRUCT-PTR-FIELD
   CELL + ;

: CFIELD: ( ptr a n -- ptr a n )
   STRUCT-REQUIRE-OPEN
   dup STRUCT-BYTE-FIELD
   1 + ;

: END-STRUCTURE ( ptr a n -- )
   STRUCT-REQUIRE-OPEN
   0 STRUCT-ACTIVE !
   swap ! ;
