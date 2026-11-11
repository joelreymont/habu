\ workspace.f - fixed compiler working storage in one transient mapping. Capture
\ releases it; the next access maps a fresh one and stores the defaults (OPEN).
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
\ accessor under the existing one-shot storage window. Every form's element is
\ one CHECKER-DYNAMIC-INFO admits: the mapping moves, so an element that lives
\ in place is refused for a variable too. The generated text ends with a call
\ of PLACE, which takes the bytes: inside a build hb runs this word, so the
\ text, not hb's own workspace, places the product's storage.
TRUSTED: DECLARE ( n n -- ) {: operand:n kind:n :}
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if
      kind BUFFER-KIND = if s" CWORK-BUFFER" else
         kind SPAN-KIND = if s" CWORK-SPAN" else s" CWORK-VARIABLE" then
      then CHECKER-STORAGE-NAME-REFUSE exit
   then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   name nameu LBUF-NAMES-OK? 0= if exit then
   type typeu CHECKER-DYNAMIC-INFO 0= if
      drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE exit
   then
   kind VARIABLE-KIND = if
      drop
      1 name nameu type typeu STORAGE-VALIDATE 0= if exit then
      LBUF-BYTES @
   else
      LBUF-W !
      operand LBUF-N !
      operand 0 <= LBUF-W @ 0 <= or if E-LAYOUT-BUFFER throw then
      operand LIMIT LBUF-W @ / > if E-LAYOUT-BUFFER throw then
      operand LBUF-W @ *
   then {: bytes:n :}
   LBUF-ALIGN
   ndict@ {: desc:n :}
   LBUF-CLEAR
   s" variable " LBUF-APP name nameu LBUF-BASE,
   s"  : " LBUF-APP name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   kind BUFFER-KIND = if
      s"  ( n -- ptr " LBUF-APP type typeu LBUF-APP s"  ) " LBUF-APP
      LBUF-N @ LBUF-DEC,
      s"  DYNAMIC-STORAGE:BOUNDS " LBUF-APP
   else
      s"  ( -- ptr " LBUF-APP type typeu LBUF-APP s"  ) " LBUF-APP
   then
   s" CWORK:OPEN CWORK:MEM#base @ " LBUF-APP
   name nameu LBUF-BASE, s"  @ + " LBUF-APP
   kind BUFFER-KIND = if
      s" swap " LBUF-APP LBUF-W @ LBUF-DEC, s"  * + " LBUF-APP
   then
   s" ; " LBUF-APP name nameu LBUF-BASE, s"  " LBUF-APP bytes LBUF-DEC,
   \ LBUF-DEC, writes digits alone: a negative default is its complement.
   s"  " LBUF-APP kind VARIABLE-KIND = if operand else 0 then {: value:n :}
   value 0 >= if value LBUF-DEC, else value invert LBUF-DEC, s"  invert" LBUF-APP then
   s"  CWORK:PLACE" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu LBUF-EVAL! drop
   ndict@ 1- {: decl:n :}
   desc 1+ decl <> if E-LAYOUT-BUFFER throw then ;

\ Global introduction forms keep their existing checker identities. Their
\ compiled calls retain these closed bridges after the names are retired.
public
: BUFFER ( n -- ) BUFFER-KIND DECLARE ;
: SPAN ( n -- ) SPAN-KIND DECLARE ;
: VARIABLE ( n -- ) VARIABLE-KIND DECLARE ;

\ The last word of DECLARE's generated text, run in the program it defines into.
: PLACE ( ptr n n n -- ) {: base:ptr bytes:n value:n :}
   bytes TAKE {: off:n :}
   off base !
   value 0<> if off value DEFAULT then ;
;package

: CWORK-BUFFER ( n -- ) CWORK:BUFFER ;
: CWORK-SPAN ( n -- ) CWORK:SPAN ;
: CWORK-VARIABLE ( n -- ) CWORK:VARIABLE ;

undefine CWORK:BUFFER
undefine CWORK:SPAN
undefine CWORK:VARIABLE
