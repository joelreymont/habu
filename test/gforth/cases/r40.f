\ A word whose effect is wider than a cell is refused at top level before the
\ min-in gate (habu2.f:10162 LWIDE); no catch receives the throw, so the exit
\ hook runs, then rc 70.
package XP
private
CAST: >VECTOR ( n -- ptr [ -- ] )
: VECTOR ( -- ptr [ -- ] ) data-base BYTE-VIEW NULL-PTR BYTE-VIEW - EXIT-HOOK-CELL + >VECTOR ;
public
: HOOK ( -- ) ." hook" cr ;
: ARM ( -- ) ['] HOOK VECTOR ! ;
;package
XP:ARM
ENUM shape 0
   VARIANT dot ;VARIANT
   VARIANT circle FIELD r n ;VARIANT
;ENUM
: CIRC ( n -- shape ) construct shape circle ;
CIRC
." after" cr
