\ dynamic-storage.f - checked allocation behind DYNAMIC-BUFFER declarations.
\ A control record contains a mapping pointer and its byte capacity. Element
\ identity and width are fixed by the declaration that owns that record.
package DYNAMIC-STORAGE
private

$7FFFFFFFFFFFFFFF constant MAX-BYTES
7121 constant E-SIZE

: EXTENT ( n n -- n ) {: count:n width:n :}
   count 0 < width 0 <= or if E-SIZE throw then
   count MAX-BYTES width / > if E-SIZE throw then
   count width * ;

: CAPACITY ( n n -- n ) {: need:n old:n :}
   old MAX-BYTES 2 / > if need exit then
   need old 2 * 64 max max ;

: COPY ( ptr n ptr n n -- ) {: src:ptr dst:ptr bytes:n :}
   bytes CELL / 0 ?do src i cells + @ dst i cells + ! loop ;

public

: RESERVE ( n ptr n n -- ) {: count:n cb:ptr width:n :}
   count width EXTENT {: need:n :}
   cb cell+ @ {: old:n :}
   need old <= if exit then
   need old CAPACITY {: cap:n :}
   cap map-anon 0< if drop E-MEM-MAP throw then {: fresh:ptr :}
   old 0 > if
      cb 0 ptr-field @ fresh old COPY
      cb 0 ptr-field @ old munmap 0< if
         fresh cap munmap drop E-MEM-UNMAP throw
      then
   then
   fresh cb 0 ptr-field !
   cap cb cell+ ! ;

: RELEASE ( ptr n -- ) {: cb:ptr :}
   cb cell+ @ {: cap:n :}
   cap 0 > if
      cb 0 ptr-field @ cap munmap 0< if E-MEM-UNMAP throw then
   then
   0 cb ! 0 cb cell+ ! ;

;package
