\ One transient address space for fixed compiler working storage. The single
\ dynamic control remains in DATA, so image capture releases its mapping with
\ every other dynamic allocation. Offsets are fixed when declarations load;
\ a restored image opens and zeroes a fresh mapping on first access.
require src/core/layout-buffer.f
require src/core/dynamic-storage.f

package CWORK
public
$100000 constant LIMIT
DYNAMIC-BUFFER MEM u8
private
variable USED
$100 constant DEFAULT-CAP
create DEFAULT-OFF DEFAULT-CAP cells allot
create DEFAULT-VALUE DEFAULT-CAP cells allot
variable DEFAULT-N

public
: DECLARED-BYTES ( -- n ) USED @ ;

\ Both parts only increase, and their sum is bounded by LIMIT + DEFAULT-CAP.
\ A file object cannot replay either allocation layout or mapped defaults.
: LAYOUT-MARK ( -- n ) USED @ DEFAULT-N @ + ;

: TAKE ( n -- n ) {: bytes:n :}
   bytes 0 <= if E-LAYOUT-BUFFER throw then
   USED @ CELL 1- + CELL / CELL * {: off:n :}
   off LIMIT > if E-LAYOUT-BUFFER throw then
   bytes LIMIT off - > if E-LAYOUT-BUFFER throw then
   off bytes + USED !
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

private
0 constant BUFFER-KIND
1 constant SPAN-KIND
2 constant VARIABLE-KIND

\ This closed storage introducer owns the prehook parser and LBUF mint
\ boundary. Runtime allocation stays checked; LBUF-EVAL! checks the generated
\ accessor under the existing one-shot storage window.
TRUSTED: DECLARE ( n n -- ) {: operand:n kind:n :}
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if
      kind BUFFER-KIND = if s" CWORK-BUFFER" else
         kind SPAN-KIND = if s" CWORK-SPAN" else s" CWORK-VARIABLE" then
      then CHECKER-STORAGE-NAME-REFUSE exit
   then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   name nameu LBUF-NAME-OK? 0= if exit then
   kind VARIABLE-KIND = if
      1 name nameu type typeu STORAGE-VALIDATE 0= if exit then
      LBUF-BYTES @
   else
      type typeu CHECKER-DYNAMIC-INFO 0= if
         drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE exit
      then
      LBUF-W !
      operand LBUF-N !
      operand 0 <= LBUF-W @ 0 <= or if E-LAYOUT-BUFFER throw then
      operand LIMIT LBUF-W @ / > if E-LAYOUT-BUFFER throw then
      operand LBUF-W @ *
   then TAKE {: off:n :}
   LBUF-CLEAR
   s" : " LBUF-APP name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   kind BUFFER-KIND = if
      s"  ( n -- ptr " LBUF-APP type typeu LBUF-APP s"  ) " LBUF-APP
      LBUF-N @ LBUF-DEC,
      s"  DYNAMIC-STORAGE:BOUNDS " LBUF-APP
   else
      s"  ( -- ptr " LBUF-APP type typeu LBUF-APP s"  ) " LBUF-APP
   then
   s" CWORK:OPEN CWORK:MEM#base @ " LBUF-APP off LBUF-DEC,
   s"  + " LBUF-APP
   kind BUFFER-KIND = if
      s" swap " LBUF-APP LBUF-W @ LBUF-DEC, s"  * + " LBUF-APP
   then
   s" ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu LBUF-EVAL! drop
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
