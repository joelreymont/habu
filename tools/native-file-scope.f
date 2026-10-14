\ The FILE linker carries the interpreter namespace across an imported range.
\ Load this beside the linker so an existing engine need not be regenerated
\ before the build command can use FILE objects.
require src/habu/packages.f

package OUTER
public

\ Record numbers and wordlist IDs survive the ordered prefix; raw
\ dictionary addresses do not survive a new build process. LASTC is the
\ last created record, including a retired one, or zero after a rollback.
22 cells constant SCOPE-BYTES

: SCOPE-SAVE ( ptr u8 -- ) {: dst:ptr :}
   data-base PKG-REC-CELL + 0 ptr-field @ {: rec:ptr :}
   rec 0= if 0 else rec PKG-INDEX 1+ then
      dst 0 cells + CELL-VIEW !
   PKG-PARENT-CELL CELL@ dst 1 cells + CELL-VIEW !
   get-current dst 2 cells + CELL-VIEW !
   USE-PKG-SAVE-CELL CELL@ dst 3 cells + CELL-VIEW !
   USE-DEPTH-CELL CELL@ dst 4 cells + CELL-VIEW !
   USE-MAX 0 ?do
      i cells USE-WIDS-OFF + CELL@
         dst i 5 + cells + CELL-VIEW !
   loop
   LASTC-CELL CELL@ {: last:n :}
   last 0= if 0 else
      last dbase@ - {: off:n :}
      off 0 < off DREC mod 0<> or
      off ndict@ DREC * >= or if RC-REJECT throw then
      off DREC / 1+
   then dst 21 cells + CELL-VIEW ! ;

: SCOPE-PREPARE ( ptr u8 n n -- )
   {: src:ptr recs:n wids:n :}
   src 0 cells + CELL-VIEW @ {: recid:n :}
   src 1 cells + CELL-VIEW @ {: parent:n :}
   src 2 cells + CELL-VIEW @ {: cur:n :}
   src 3 cells + CELL-VIEW @ {: floor:n :}
   src 4 cells + CELL-VIEW @ {: depth:n :}
   src 21 cells + CELL-VIEW @ {: lastid:n :}
   recid 0 < recid recs > or
   lastid 0 < lastid recs > or or
   depth 0 < depth USE-MAX > or or
   floor 0 < floor USE-MAX > or or
   parent 0 < parent wids >= or or
   cur 0 < cur wids >= or or if RC-REJECT throw then
   depth 0 ?do
      src i 5 + cells + CELL-VIEW @ {: wid:n :}
      wid 0 < wid wids >= or if RC-REJECT throw then
   loop ;

: SCOPE-INSTALL ( ptr u8 -- ) {: src:ptr :}
   src 0 cells + CELL-VIEW @ {: recid:n :}
   src 1 cells + CELL-VIEW @ {: parent:n :}
   src 2 cells + CELL-VIEW @ {: cur:n :}
   src 3 cells + CELL-VIEW @ {: floor:n :}
   src 4 cells + CELL-VIEW @ {: depth:n :}
   src 21 cells + CELL-VIEW @ {: lastid:n :}
   USE-MAX 0 ?do
      src i 5 + cells + CELL-VIEW @
         i cells USE-WIDS-OFF + CELL!
   loop
   depth USE-DEPTH-CELL CELL!
   lastid 0= if 0 else lastid 1- XREF-REC-ADDR then
      LASTC-CELL CELL!
   recid 0= if 0 else recid 1- XREF-REC-ADDR then
      parent cur floor PKG-RECOVER ;

;package
