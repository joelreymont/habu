\ Copy source-bound frozen fetch descriptors, bind widths and operand widths
\ before another checker scan runs.
require src/lib/errors.f
require src/lib/prelude.f
require src/compiler/digest.f
require src/compiler/native/fetch-owner.f
require src/compiler/native/string.f
require src/compiler/native/fetch-check.f

package NFETCH
public

STRUCTURE fact
   FIELD offset n
   FIELD address n
   FIELD bytes n
   FIELD width n
;STRUCTURE

private

DYNAMIC-BUFFER FACTS fact
DYNAMIC-BUFFER ORIGINS n
variable COUNT

\ Every split's certified bind widths, in capture order: split i holds widths
\ SPLIT-END(i-1) up to SPLIT-END(i), and its source starts at SPLIT-BASE(i).
DYNAMIC-BUFFER WIDTHS n
DYNAMIC-BUFFER SPLIT-BASE n
DYNAMIC-BUFFER SPLIT-END n
variable BIND-N
variable SPLIT-N

\ Every split's certified operand widths, its width-fact rows, keyed by the
\ token's offset in the whole source: offset, operand position, cells.
DYNAMIC-BUFFER OP-AT n
DYNAMIC-BUFFER OP-POS n
DYNAMIC-BUFFER OP-W n
variable OP-N

: BAD ( -- )
   s" ncomp: malformed or stale fetch certificate" 76 die ;

: CELL@ ( ptr u8 n n -- n ) {: data:ptr count:n index:n :}
   index 0 < index count >= or if BAD then
   data index CDIGEST:SLOT@ ;

: RANGE-CK ( n n n -- ) {: start:n length:n total:n :}
   start 0 < start total > or length 0 < or if BAD then
   length total start - > if BAD then ;

: HASH ( ptr u8 n -- n ) {: source:ptr size:n :}
   LOWER-CERT:FNV-OFFSET
   size 0 ?do source i + c@ xor LOWER-CERT:FNV-PRIME * loop ;

: HEADER-CK ( ptr u8 n n -- ) {: data:ptr bytes:n source:n :}
   bytes LOWER-CERT:HEADER-CELLS cells < bytes CELL mod 0<> or if BAD then
   bytes CELL / {: count:n :}
   data count LOWER-CERT:MAGIC-CELL CELL@ LOWER-CERT:MAGIC <> if BAD then
   data count LOWER-CERT:VERSION-CELL CELL@ LOWER-CERT:VERSION <> if BAD then
   data count LOWER-CERT:TOTAL-BYTES-CELL CELL@ bytes <> if BAD then
   data count LOWER-CERT:BODY-LEN-CELL CELL@ source <> if BAD then ;

: TABLE-END ( n n n n -- n ) {: start:n rows:n stride:n total:n :}
   rows 0 < start 0 < or start total > or if BAD then
   rows total start - stride / > if BAD then
   start rows stride * + ;

: BIND-START ( ptr u8 n -- n ) {: data:ptr count:n :}
   LOWER-CERT:HEADER-CELLS
   data count LOWER-CERT:WF-COUNT-CELL CELL@ LOWER-CERT:WF-CELLS count TABLE-END ;

: FETCH-START ( ptr u8 n -- n ) {: data:ptr count:n :}
   data count BIND-START
   data count LOWER-CERT:BIND-COUNT-CELL CELL@ 1 count TABLE-END ;

: STREAM-START ( ptr u8 n -- n ) {: data:ptr count:n :}
   data count FETCH-START
   data count LOWER-CERT:FETCH-COUNT-CELL CELL@ LOWER-CERT:FETCH-CELLS count TABLE-END ;

: TABLES-CK ( ptr u8 n -- ) {: data:ptr count:n :}
   data count STREAM-START
   data count LOWER-CERT:FETCH-DATA-CELLS-CELL CELL@ 1 count TABLE-END
   count <> if BAD then ;

: WIDTH-FOR ( ptr u8 n n -- n ) {: data:ptr count:n offset:n :}
   data count LOWER-CERT:WF-COUNT-CELL CELL@ 0 ?do
      LOWER-CERT:HEADER-CELLS i LOWER-CERT:WF-CELLS * + {: row:n :}
      data count row CELL@ offset = if
         data count row 3 + CELL@ LOWER-CERT:FETCH-FLAG and 0<> if
            data count row 2 + CELL@ unloop exit
         then
      then
   loop
   BAD ;

: KEEP ( n ptr u8 n n -- ) {: offset:n data:ptr bytes:n width:n :}
   data bytes width NFETCH-CHECK:SHAPE
   data 0 CDIGEST:SLOT@ 0= if exit then
   COUNT @ 1+ FACTS-RESERVE
   COUNT @ 1+ ORIGINS-RESERVE
   data bytes NSTR:INTERN-SOURCE {: address:n origin:n :}
   offset address bytes width NFETCH-FACT:MAKE COUNT @ FACTS !
   origin COUNT @ ORIGINS !
   1 COUNT +! ;

: COPY-ROW ( ptr u8 n n n n -- )
   {: data:ptr count:n index:n source:n base:n :}
   data count index CELL@ {: offset:n :}
   offset 0 < offset source >= or if BAD then
   data count index 1+ CELL@ {: start:n :}
   data count index 2 + CELL@ {: length:n :}
   start data count STREAM-START < if BAD then
   start length count RANGE-CK
   offset base + data start cells + length cells data count offset WIDTH-FOR KEEP ;

\ The certificate's bind table: one width per bind, in the scan's bind order.
: KEEP-BINDS ( ptr u8 n n -- ) {: data:ptr count:n base:n :}
   data count BIND-START {: start:n :}
   data count LOWER-CERT:BIND-COUNT-CELL CELL@ {: binds:n :}
   BIND-N @ binds + WIDTHS-RESERVE
   binds 0 ?do
      data count start i + CELL@ {: width:n :}
      width 0 < if BAD then
      width BIND-N @ i + WIDTHS !
   loop
   BIND-N @ binds + BIND-N !
   SPLIT-N @ 1+ SPLIT-BASE-RESERVE
   SPLIT-N @ 1+ SPLIT-END-RESERVE
   base SPLIT-N @ SPLIT-BASE !
   BIND-N @ SPLIT-N @ SPLIT-END !
   1 SPLIT-N +! ;

\ The certificate's width-fact table: per row the token's offset in the split,
\ the operand position, its cells and flags (lower-cert WIDTH-FACTS,).
: KEEP-WIDTHS ( ptr u8 n n n -- ) {: data:ptr count:n source:n base:n :}
   data count LOWER-CERT:WF-COUNT-CELL CELL@ {: rows:n :}
   OP-N @ rows + {: total:n :}
   total OP-AT-RESERVE  total OP-POS-RESERVE  total OP-W-RESERVE
   rows 0 ?do
      LOWER-CERT:HEADER-CELLS i LOWER-CERT:WF-CELLS * + {: row:n :}
      data count row CELL@ {: offset:n :}
      data count row 1+ CELL@ {: pos:n :}
      data count row 2 + CELL@ {: width:n :}
      offset 0 < offset source >= or pos 0 < or width 0 < or if BAD then
      OP-N @ i + {: k:n :}
      offset base + k OP-AT !  pos k OP-POS !  width k OP-W !
   loop
   total OP-N ! ;

\ The split a source offset lies in: the last one captured at or below it.
: SPLIT-AT ( n -- n ) {: offset:n :}
   -1
   SPLIT-N @ 0 ?do
      i SPLIT-BASE @ offset <= if drop i then
   loop
   dup 0 < if BAD then ;

: SPLIT-FIRST ( n -- n ) {: split:n :}
   split 0= if 0 exit then
   split 1- SPLIT-END @ ;

public

\ Forget the definition's facts. The buffers keep their mappings, grown to the
\ largest definition seen, for the next one: the four counts bound every read,
\ so no row past them is seen. Image capture unmaps them through the dynamic
\ storage registry.
: RELEASE ( -- )
   0 COUNT !
   0 BIND-N !
   0 SPLIT-N !
   0 OP-N !
   CHECKER-OWNER:RELEASE-CERTIFICATE ;

\ Each split body contributes its own complete snapshot under its source base.
\ Only NSTR-owned immutable bytes survive this call; no live certificate or
\ checker mapping is retained by the compiler or the emitted routine.
\ A scan that lost a width fact past a token it could not apply while a wide
\ value was live (checker.f SCAN-INCOMPLETE) leaves nothing to place that
\ value's cells from, so the definition is refused E-NELAB-BUNDLE before
\ anything is lowered, as tier 0 refuses it (habu2.f VALIDATE). The refusal
\ holds in every regime that scans, a hookless body's included: an unenforced
\ verdict still needs complete width facts to lower from.
: CAPTURE ( ptr u8 n n -- ) {: source:ptr size:n base:n :}
   source size HASH CHECKER-OWNER:CERTIFICATE {: data:ptr bytes:n :}
   data bytes size HEADER-CK
   bytes CELL / {: count:n :}
   data count LOWER-CERT:NEEDS-CELL CELL@ LOWER-CERT:INCOMPLETE-NEEDS =
   if E-NELAB-BUNDLE throw then
   data count TABLES-CK
   data count size base KEEP-WIDTHS
   data count base KEEP-BINDS
   data count FETCH-START {: start:n :}
   data count LOWER-CERT:FETCH-COUNT-CELL CELL@ 0 ?do
      data count start i LOWER-CERT:FETCH-CELLS * + size base COPY-ROW
   loop ;

: AT ( n -- n n n n ) {: offset:n :}
   COUNT @ 0 ?do
      i FACTS @ NFETCH-FACT:UNMAKE {: at:n address:n bytes:n width:n :}
      at offset = if address bytes width i ORIGINS @ unloop exit then
   loop
   0 0 0 -1 ;

: CHECKED? ( n -- bool )
   AT drop 0 > nip nip ;

\ How many binds the scan of the split holding this source offset counted.
: BINDS ( n -- n )
   SPLIT-AT {: split:n :}
   split SPLIT-END @  split SPLIT-FIRST - ;

\ The cells the checker certified for operand pos of the token at this offset
\ in the whole source, position 0 the top (checker.f WF-W-AT). An operand no
\ row names is one cell, the checker's own answer: it files only layout values.
: OPERAND-CELLS ( n n -- n ) {: offset:n pos:n :}
   OP-N @ 0 ?do
      i OP-AT @ offset =  i OP-POS @ pos =  and if i OP-W @ unloop exit then
   loop
   1 ;

\ The certified width of bind seq in the split holding this source offset
\ (checker.f LOCW-HW@): the cells that local holds once its body is checked.
: BIND-WIDTH ( n n -- n ) {: offset:n seq:n :}
   offset SPLIT-AT {: split:n :}
   split SPLIT-FIRST seq + {: at:n :}
   seq 0 <  at split SPLIT-END @ >=  or if BAD then
   at WIDTHS @ ;

;package
