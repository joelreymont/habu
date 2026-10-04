\ A context owns this copy after the backend retires its transient emission.
require lib/prelude.f
require lib/errors.f
require src/core/bytes.f
require src/compiler/binding.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/session/backend.f
require src/compiler/native/emission.f

package NART
private

STRUCTURE key 0
   FIELD rows IR-ARENA:view
;STRUCTURE

public

STRUCTURE emission 0
   FIELD key key
;STRUCTURE

private

STRUCTURE artifact 0 DERIVE addr
   FIELD binding CBIND:binding
   FIELD code ptr u8
   FIELD rows IR-ARENA:view
;STRUCTURE

\ The side span belongs to the same context as its frozen arena. The arena
\ validates the handle before this one typed projection of the span is used.
TRUSTED: ARTIFACT-PTR ( ptr u8 -- ptr artifact ) ;

: VIEW ( NART:emission -- IR-ARENA:view )
   NART-EMISSION:UNMAKE KEY-UNMAKE ;

: EMISSION ( IR-ARENA:view -- NART:emission )
   KEY-MAKE NART-EMISSION:MAKE ;

0 constant H-SIZE
1 constant H-RET
2 constant H-PLACED
3 constant H-PLACE
4 constant H-FUNS
5 constant H-CALLS
6 constant H-ADDRS
7 constant HDR-CELLS
3 constant CALL-CELLS
2 constant ADDR-CELLS

: RECORD@ ( NART:emission -- artifact )
   VIEW IR-ARENA:OPEN IR-ARENA:SIDE-FIELD @ ARTIFACT-PTR @ ;

: READER ( NART:emission -- IR-ARENA:reader )
   RECORD@ ARTIFACT-UNMAKE
   {: binding:CBIND:binding code:ptr rows:IR-ARENA:view :}
   rows IR-ARENA:OPEN ;

: CELL, ( IR-CTX:ctx IR-ARENA:arena n -- )
   IR-ARENA:PUSH drop ;

: ROW-ROOM ( n n n -- n )
   {: used:n count:n width:n :}
   IR-CTX:SCRATCH-LIMIT CDIGEST:SLOT-BYTES / {: cap:n :}
   count 0 < count cap used - width / > or if E-NEMIT-ROW throw then
   used count width * + ;

: CELLS# ( -- n )
   HDR-CELLS NEMIT:FUNCTIONS 1 ROW-ROOM
   NEMIT:CALL-SITES CALL-CELLS ROW-ROOM
   NEMIT:ADDR-SITES ADDR-CELLS ROW-ROOM ;

: HEADER, ( IR-CTX:ctx IR-ARENA:arena -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena :}
   c a NEMIT:SIZE CELL,
   c a NEMIT:RET-BYTES CELL,
   c a NEMIT:PLACED? if 1 else 0 then CELL,
   c a NEMIT:PLACED? if NEMIT:PLACEMENT else 0 then CELL,
   c a NEMIT:FUNCTIONS CELL,
   c a NEMIT:CALL-SITES CELL,
   c a NEMIT:ADDR-SITES CELL, ;

: ROWS, ( IR-CTX:ctx IR-ARENA:arena -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena :}
   NEMIT:FUNCTIONS 0 ?do c a i NEMIT:FUNCTION-OFFSET@ CELL, loop
   NEMIT:CALL-SITES 0 ?do
      c a i NEMIT:CALL-SITE@ CELL,
      c a i NEMIT:CALL-KIND@ CELL,
      c a i NEMIT:CALL-TARGET@ CELL,
   loop
   NEMIT:ADDR-SITES 0 ?do
      c a i NEMIT:ADDR-SITE@ CELL,
      c a i NEMIT:ADDR-SITE-KIND@ CELL,
   loop ;

: ROW-CK ( n n -- n )
   {: i:n count:n :}
   i 0 < i count >= or if E-NEMIT-ROW throw then
   i ;

: CALL@ ( NART:emission n n -- n )
   {: e:NART:emission i:n field:n :}
   e READER {: r:IR-ARENA:reader :}
   i r H-CALLS IR-ARENA:RD@ ROW-CK CALL-CELLS *
   HDR-CELLS + r H-FUNS IR-ARENA:RD@ + field +
   r swap IR-ARENA:RD@ ;

: ADDR@ ( NART:emission n n -- n )
   {: e:NART:emission i:n field:n :}
   e READER {: r:IR-ARENA:reader :}
   i r H-ADDRS IR-ARENA:RD@ ROW-CK ADDR-CELLS *
   HDR-CELLS + r H-FUNS IR-ARENA:RD@ +
   r H-CALLS IR-ARENA:RD@ CALL-CELLS * + field +
   r swap IR-ARENA:RD@ ;

public

\ The active work session selects the owner before RETIRE; allocation and
\ partial rows die with that context.
: COPY ( NSESSION:session -- NART:emission )
   NSESSION:RESOLVE drop {: c:IR-CTX:ctx :}
   c IR-CTX:BINDING@ {: binding:CBIND:binding :}
   binding CBIND:TARGET@ CTARGET:ARCH@ NEMIT:ARCH CTARGET-ARCH:EQ 0=
   if E-NEMIT-STATE throw then
   CELLS# {: count:n :}
   c count IR-ARENA:NEW {: rows:IR-ARENA:arena :}
   c rows count IR-ARENA:RESERVE
   c NEMIT:SIZE IR-CTX:SCRATCH-TAKE {: bytes:ptr size:n :}
   NEMIT:BYTES bytes size BYTE-COPY
   c rows HEADER,
   c rows ROWS,
   c ARTIFACT-BYTES IR-CTX:SCRATCH-TAKE drop {: data:ptr :}
   rows IR-ARENA:FREEZE {: view:IR-ARENA:view :}
   binding bytes view ARTIFACT-MAKE data ARTIFACT-PTR !
   data view IR-ARENA:OPEN IR-ARENA:SIDE-FIELD !
   view EMISSION ;

: SIZE ( NART:emission -- n )
   READER H-SIZE IR-ARENA:RD@ ;

: BYTES ( NART:emission -- ptr u8 )
   RECORD@ ARTIFACT-UNMAKE
   {: binding:CBIND:binding code:ptr rows:IR-ARENA:view :}
   code ;

: BINDING ( NART:emission -- CBIND:binding )
   RECORD@ ARTIFACT-UNMAKE
   {: binding:CBIND:binding code:ptr rows:IR-ARENA:view :}
   binding ;

: ARCH ( NART:emission -- CTARGET:arch )
   BINDING CBIND:TARGET@ CTARGET:ARCH@ ;

: RET-BYTES ( NART:emission -- n )
   READER H-RET IR-ARENA:RD@ ;

: PLACED? ( NART:emission -- bool )
   READER H-PLACED IR-ARENA:RD@ 0<> ;

: PLACEMENT ( NART:emission -- n )
   READER {: r:IR-ARENA:reader :}
   r H-PLACED IR-ARENA:RD@ 0= if E-NEMIT-STATE throw then
   r H-PLACE IR-ARENA:RD@ ;

: FUNCTIONS ( NART:emission -- n )
   READER H-FUNS IR-ARENA:RD@ ;

: FUNCTION-OFFSET@ ( NART:emission n -- n )
   {: e:NART:emission i:n :}
   e READER {: r:IR-ARENA:reader :}
   i r H-FUNS IR-ARENA:RD@ ROW-CK HDR-CELLS +
   r swap IR-ARENA:RD@ ;

: CALL-SITES ( NART:emission -- n )
   READER H-CALLS IR-ARENA:RD@ ;

: CALL-SITE@ ( NART:emission n -- n ) 0 CALL@ ;
: CALL-KIND@ ( NART:emission n -- n ) 1 CALL@ ;
: CALL-TARGET@ ( NART:emission n -- n ) 2 CALL@ ;

: ADDR-SITES ( NART:emission -- n )
   READER H-ADDRS IR-ARENA:RD@ ;

: ADDR-SITE@ ( NART:emission n -- n ) 0 ADDR@ ;
: ADDR-SITE-KIND@ ( NART:emission n -- n ) 1 ADDR@ ;

: RELEASE ( NART:emission -- )
   VIEW IR-ARENA:RETIRE ;

;package
