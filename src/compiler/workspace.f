\ workspace.f - fixed compiler working storage in one transient mapping. Capture
\ releases it; the next access maps a fresh one and stores the defaults (OPEN).
\ The mapping's record is laid by hand, head then control cells, both scratch.
\ Each declaration makes its accessor as a typed child whose body holds its
\ offset in the mapping, then its element's bytes.
require src/core/layout-buffer.f
require src/core/dynamic-storage.f

package CWORK
$100000 constant LIMIT
PTR-VARIABLE MEM#base
create MEM#ctl DYNAMIC-STORAGE:CONTROL-CELLS cells SCRATCH-ALLOT
: MEM-RESERVE ( n -- ) MEM#base MEM#ctl 1 DYNAMIC-STORAGE:RESERVE ;
variable USED
$100 constant DEFAULT-CAP
create DEFAULT-OFF DEFAULT-CAP cells allot
create DEFAULT-VALUE DEFAULT-CAP cells allot
variable DEFAULT-N

\ The offset the next bytes take; USED moves only once they are laid.
: FIT ( n -- n ) {: bytes:n :}
   bytes 0 <= if E-LAYOUT-BUFFER throw then
   USED @ CELL 1- + CELL / CELL * {: off:n :}
   off LIMIT > if E-LAYOUT-BUFFER throw then
   bytes LIMIT off - > if E-LAYOUT-BUFFER throw then
   off ;

: DEFAULT ( n n -- ) {: off:n value:n :}
   DEFAULT-N @ DEFAULT-CAP >= if E-LAYOUT-BUFFER throw then
   off DEFAULT-OFF DEFAULT-N @ cells + !
   value DEFAULT-VALUE DEFAULT-N @ cells + !
   DEFAULT-N @ 1+ DEFAULT-N !
   MEM#base @ NULL-PTR <> if value MEM#base @ off + ! then ;

: OPEN ( -- )
   MEM#base @ NULL-PTR <> if exit then
   LIMIT MEM-RESERVE
   MEM#base @ {: base:ptr :}
   DEFAULT-N @ 0 ?do
      i cells DEFAULT-OFF + @ base +
      i cells DEFAULT-VALUE + @ swap !
   loop ;

\ The routines a child's body calls with its body.
: WORK ( ptr n -- ptr u8 ) {: body:ptr :}
   OPEN  MEM#base @ body @ + ;

: WORK-AT ( n ptr n -- ptr u8 ) {: i:n body:ptr :}
   body WORK  i body CELL + @ * + ;

0 constant BUFFER-KIND
1 constant SPAN-KIND
2 constant VARIABLE-KIND

CAST: AS-STORAGE-LINE ( n -- [ ptr u8 n -- ptr u8 n ptr u8 n n ] )
CAST: AS-CHILD-RUN ( n -- [ ptr u8 n n n ptr u8 n -- n ] )
CAST: AS-WORK-ENTRY ( [ ptr n -- ptr u8 ] -- n )
CAST: AS-WORKS-ENTRY ( [ n ptr n -- ptr u8 ] -- n )

: DEFINER$ ( n -- ptr u8 n ) {: kind:n :}
   kind BUFFER-KIND = if s" CWORK-BUFFER" exit then
   kind SPAN-KIND = if s" CWORK-SPAN" exit then
   s" CWORK-VARIABLE" ;

\ The bytes a declaration of OPERAND elements of W bytes takes.
: EXTENT ( n n n -- n ) {: operand:n kind:n w:n :}
   kind VARIABLE-KIND = if w exit then
   operand 0 <= if E-LAYOUT-BUFFER throw then
   operand LIMIT w / > if E-LAYOUT-BUFFER throw then
   operand w * ;

\ The product's USED and MEM#base place the storage; the building hb makes the
\ child.
: CHILD ( ptr u8 n n n ptr u8 n -- n )
   data-base PROVIDED-XT:CHILD-CELL + @ AS-CHILD-RUN execute ;

: ACCESSOR ( ptr u8 n ptr u8 n n -- ) {: na:ptr nu:n ta:ptr tu:n kind:n :}
   kind BUFFER-KIND = if
      na nu CHILD-ABI:WORKS ['] WORK-AT AS-WORKS-ENTRY ta tu CHILD drop exit
   then
   na nu CHILD-ABI:WORK ['] WORK AS-WORK-ENTRY ta tu CHILD drop ;

\ Every element is one CHECK:STORAGE-LINE admits: the mapping moves, so an
\ element that lives in place is refused for a variable too. A variable's
\ element is a whole cell.
: DECLARE ( n n -- ) {: operand:n kind:n :}
   kind DEFINER$ data-base PROVIDED-XT:STORAGE-LINE-CELL + @
   AS-STORAGE-LINE execute {: na:ptr nu:n ta:ptr tu:n w:n :}
   w 0= if exit then
   kind VARIABLE-KIND = w CELL mod 0<> and if
      na nu ta tu CHECKER-STORAGE-TYPE-REFUSE exit
   then
   operand kind w EXTENT {: bytes:n :}
   bytes FIT {: off:n :}
   na nu ta tu kind ACCESSOR
   off ,  w ,
   off bytes + USED !
   kind VARIABLE-KIND = operand 0<> and if off operand DEFAULT then ;

\ Global introduction forms keep their existing checker identities. Their
\ compiled calls retain these closed bridges after the names are retired.
public
: BUFFER ( n -- ) BUFFER-KIND DECLARE ;
: SPAN ( n -- ) SPAN-KIND DECLARE ;
: VARIABLE ( n -- ) VARIABLE-KIND DECLARE ;
;package

: CWORK-BUFFER ( n -- ) CWORK:BUFFER ;
: CWORK-SPAN ( n -- ) CWORK:SPAN ;
: CWORK-VARIABLE ( n -- ) CWORK:VARIABLE ;

undefine CWORK:BUFFER
undefine CWORK:SPAN
undefine CWORK:VARIABLE
