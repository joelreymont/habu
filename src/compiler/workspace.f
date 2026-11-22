split-str
\ workspace.f - fixed compiler working storage in one transient mapping. Capture
\ releases it; the next access maps a fresh one and stores the defaults (OPEN).
\ The mapping's record is laid by hand, head then control cells, both scratch.
\ Each declaration makes its accessor as a typed child whose body holds its
\ offset in the mapping, then its element's bytes.
require src/core/exit-status.f
require src/core/layout-buffer.f
require src/core/dynamic-storage.f

package CWORK
EXPORT BOOT:data-base
EXPORT CHECK:INTRINSIC
EXPORT CHECK:INTRINSIC-TOP-ONLY
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
   bytes 0 <= if
      s" compiler workspace: a declaration takes no bytes" EXIT-STATUS:INTERNAL die
   then
   USED @ CELL 1- + CELL / CELL * {: off:n :}
   off LIMIT > if
      s" compiler workspace: the mapping is full at LIMIT bytes" EXIT-STATUS:FULL die
   then
   bytes LIMIT off - > if
      s" compiler workspace: the mapping is full at LIMIT bytes" EXIT-STATUS:FULL die
   then
   off ;

: DEFAULT ( n n -- ) {: off:n value:n :}
   off DEFAULT-OFF DEFAULT-N @ cells + !
   value DEFAULT-VALUE DEFAULT-N @ cells + !
   DEFAULT-N @ 1+ DEFAULT-N !
   MEM#base @ NULL-PTR <> if value MEM#base @ off + ! then ;

: OPEN ( -- )
   MEM#base @ NULL-PTR <> if exit then
   LIMIT MEM-RESERVE
   MEM#base @ {: base:^ :}
   DEFAULT-N @ 0 ?do
      i cells DEFAULT-OFF + @ base +
      i cells DEFAULT-VALUE + @ swap !
   loop ;

\ The routines a child's body calls with its body.
: WORK ( ^n -- ^u8 ) {: body:^ :}
   OPEN  MEM#base @ body @ + ;

: WORK-AT ( n ^n -- ^u8 ) {: i:n body:^ :}
   body WORK  i body CELL + @ * + ;

0 constant BUFFER-KIND
1 constant SPAN-KIND
2 constant VARIABLE-KIND

CAST: AS-STORAGE-LINE ( n -- [ str -- str str n ] )
CAST: AS-CHILD-RUN ( n -- [ str n n str -- n ] )
CAST: AS-WORK-ENTRY ( [ ^n -- ^u8 ] -- n )
CAST: AS-WORKS-ENTRY ( [ n ^n -- ^u8 ] -- n )

: DEFINER$ ( n -- str ) {: kind:n :}
   kind BUFFER-KIND = if s" CWORK-BUFFER" exit then
   kind SPAN-KIND = if s" CWORK-SPAN" exit then
   s" CWORK-VARIABLE" ;

\ The bytes a declaration of OPERAND elements of W bytes takes.
: EXTENT ( n n n -- n ) {: operand:n kind:n w:n :}
   kind VARIABLE-KIND = if w exit then
   operand 0 <= if
      s" compiler workspace: a declaration has no elements" EXIT-STATUS:REJECTED die
   then
   operand LIMIT w / > if
      s" compiler workspace: the mapping is full at LIMIT bytes" EXIT-STATUS:FULL die
   then
   operand w * ;

\ The product's USED and MEM#base place the storage; the building hb makes the
\ child.
: CHILD ( str n n str -- n )
   data-base PROVIDED-XT:CHILD-CELL + @ AS-CHILD-RUN execute ;

: ACCESSOR ( str str n -- ) {: na:^ nu:n ta:^ tu:n kind:n :}
   kind BUFFER-KIND = if
      na nu CHILD-ABI:WORKS ['] WORK-AT AS-WORKS-ENTRY ta tu CHILD drop exit
   then
   na nu CHILD-ABI:WORK ['] WORK AS-WORK-ENTRY ta tu CHILD drop ;

\ A variable's element is whole cells, a buffer's or span's may be a byte.
\ The line reader reports a refused type and answers 0 bytes.
: DECLARE ( n n -- ) {: operand:n kind:n :}
   kind VARIABLE-KIND = if PROVIDED-XT:CELL-LINE-CELL
   else PROVIDED-XT:STORAGE-LINE-CELL then {: reader:n :}
   kind DEFINER$ data-base reader + @
   AS-STORAGE-LINE execute {: na:^ nu:n ta:^ tu:n w:n :}
   w 0= if exit then
   operand kind w EXTENT {: bytes:n :}
   kind VARIABLE-KIND = operand 0<> and
   DEFAULT-N @ DEFAULT-CAP >= and if
      s" compiler workspace: the default table is full at DEFAULT-CAP rows"
      EXIT-STATUS:FULL die
   then
   bytes FIT {: off:n :}
   na nu ta tu kind ACCESSOR
   off ,  w ,
   off bytes + USED !
   kind VARIABLE-KIND = operand 0<> and if off operand DEFAULT then ;

global
: CWORK-BUFFER ( n -- ) BUFFER-KIND DECLARE ;
INTRINSIC-TOP-ONLY INTRINSIC
: CWORK-SPAN ( n -- ) SPAN-KIND DECLARE ;
INTRINSIC-TOP-ONLY INTRINSIC
: CWORK-VARIABLE ( n -- ) VARIABLE-KIND DECLARE ;
INTRINSIC-TOP-ONLY INTRINSIC
;package
