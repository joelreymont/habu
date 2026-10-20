\ The FILE linker carries the interpreter namespace across an imported range.
\ Load this beside the linker so an existing engine need not be regenerated
\ before the build command can use FILE objects.
require src/habu/packages.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/string.f
require tools/native-file-reference.f

package OUTER
private

DYNAMIC-BUFFER INPUT-BYTES u8
variable INPUT-U

: FILE-SCOPE-ROOM ( n -- n ) {: size:n :}
   size 0 < size MEM-MAX-N INPUT-U @ - > or if RC-REJECT throw then
   INPUT-U @ size + INPUT-BYTES-RESERVE
   INPUT-U @ dup size + INPUT-U ! ;

: FILE-SCOPE-N ( n -- )
   8 FILE-SCOPE-ROOM INPUT-BYTES LE:U64! ;

: FILE-SCOPE-S ( ptr u8 n -- ) {: bytes:ptr size:n :}
   size FILE-SCOPE-N
   size 0 > if
      size FILE-SCOPE-ROOM INPUT-BYTES bytes swap size BYTE-COPY
   then ;

: FILE-SCOPE-REC ( n -- ) {: ord:n :}
   ord -1 = if NULL-PTR 0 FILE-SCOPE-S exit then
   ord NFILE-REF:DECL-KEY$ FILE-SCOPE-S ;

: FILE-SCOPE-WID ( n -- ) NFILE-REF:WID-KEY$ FILE-SCOPE-S ;

: LIVE-INPUT ( -- )
   0 INPUT-U !
   data-base PKG-REC-CELL + 0 ptr-field @ {: rec:ptr :}
   rec 0= if -1 else rec PKG-INDEX then FILE-SCOPE-REC
   PKG-PARENT-CELL CELL@ FILE-SCOPE-WID
   get-current FILE-SCOPE-WID
   USE-PKG-SAVE-CELL CELL@ FILE-SCOPE-N
   USE-DEPTH-CELL CELL@ {: depth:n :}
   depth FILE-SCOPE-N
   depth 0 ?do i cells USE-WIDS-OFF + CELL@ FILE-SCOPE-WID loop ;

public

: SCOPE-INPUT$ ( -- ptr u8 n )
   LIVE-INPUT
   0 INPUT-BYTES INPUT-U @ ;

: SCOPE-MATCH? ( ptr u8 n -- bool )
   {: bytes:ptr size:n :}
   SCOPE-INPUT$ bytes size STR= ;

: SCOPE-CLOSE ( -- )
   INPUT-BYTES-RELEASE 0 INPUT-U ! ;


\ The saved trailer keeps reference indices into its range's section 13.
\ Selection resolves those indices in its writable object copy while that
\ range owns the active table. LASTC includes retired records.
22 cells constant SCOPE-BYTES

: SCOPE-SAVE ( ptr u8 -- ) {: dst:ptr :}
   data-base PKG-REC-CELL + 0 ptr-field @ {: rec:ptr :}
   rec 0= if 0 else rec PKG-INDEX NFILE-REF:REF-RECORD then
      dst 0 cells + CELL-VIEW !
   PKG-PARENT-CELL CELL@ NFILE-REF:REF-WID
      dst 1 cells + CELL-VIEW !
   get-current NFILE-REF:REF-WID dst 2 cells + CELL-VIEW !
   USE-PKG-SAVE-CELL CELL@ dst 3 cells + CELL-VIEW !
   USE-DEPTH-CELL CELL@ dst 4 cells + CELL-VIEW !
   USE-MAX 0 ?do
      i USE-DEPTH-CELL CELL@ < if
         i cells USE-WIDS-OFF + CELL@ NFILE-REF:REF-WID
      else 0 then dst i 5 + cells + CELL-VIEW !
   loop
   LASTC-CELL CELL@ {: last:n :}
   last 0= if 0 else
      last dbase@ - {: off:n :}
      off 0 < off DREC mod 0<> or
      off ndict@ DREC * >= or if RC-REJECT throw then
      off DREC / NFILE-REF:REF-RECORD
   then dst 21 cells + CELL-VIEW ! ;

: SCOPE-PREPARE ( ptr u8 n n -- )
   {: src:ptr recs:n wids:n :}
   src 0 cells + CELL-VIEW @ NFILE-REF:REF>RECORD {: recid:n :}
   src 1 cells + CELL-VIEW @ NFILE-REF:REF>WID {: parent:n :}
   src 2 cells + CELL-VIEW @ NFILE-REF:REF>WID {: cur:n :}
   src 3 cells + CELL-VIEW @ {: floor:n :}
   src 4 cells + CELL-VIEW @ {: depth:n :}
   src 21 cells + CELL-VIEW @ NFILE-REF:REF>RECORD {: lastid:n :}
   recid -1 < recid recs >= or
   lastid -1 < lastid recs >= or or
   depth 0 < depth USE-MAX > or or
   floor 0 < floor USE-MAX > or or
   parent 0 < parent wids >= or or
   cur 0 < cur wids >= or or if RC-REJECT throw then
   depth 0 ?do
      src i 5 + cells + CELL-VIEW @ NFILE-REF:REF>WID {: wid:n :}
      wid 0 < wid wids >= or if RC-REJECT throw then
   loop
   recid src 0 cells + CELL-VIEW !
   parent src 1 cells + CELL-VIEW !
   cur src 2 cells + CELL-VIEW !
   lastid src 21 cells + CELL-VIEW !
   depth 0 ?do
      src i 5 + cells + CELL-VIEW @ NFILE-REF:REF>WID
         src i 5 + cells + CELL-VIEW !
   loop ;

: SCOPE-INSTALL ( ptr u8 -- ) {: src:ptr :}
   src 0 cells + CELL-VIEW @ {: recid:n :}
   src 1 cells + CELL-VIEW @ {: parent:n :}
   src 2 cells + CELL-VIEW @ {: cur:n :}
   src 3 cells + CELL-VIEW @ {: floor:n :}
   src 4 cells + CELL-VIEW @ {: depth:n :}
   src 21 cells + CELL-VIEW @ {: lastid:n :}
   USE-MAX 0 ?do
      i depth < if
         src i 5 + cells + CELL-VIEW @
      else 0 then i cells USE-WIDS-OFF + CELL!
   loop
   depth USE-DEPTH-CELL CELL!
   lastid -1 = if 0 else lastid XREF-REC-ADDR then
      LASTC-CELL CELL!
   recid -1 = if 0 else recid XREF-REC-ADDR then
      parent cur floor PKG-RECOVER ;

;package
