\ Evaluation rollback retires declarations by address, not registration order.
require src/lib/test.f
require src/habu/address-cells.f
require src/habu/xref.f

package ADDRESS-CELL-ROLLBACK
variable BASE-N
variable BASE-DP
variable FAILED-OFF
create OLDER CELL allot
create SLOTS ADDRESS-CELLS:BOOT-CAP 1+ cells allot
DYNAMIC-BUFFER SAVED n

: ROWS ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;
: HEADER@ ( n -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + + @ ;
: OFFSET ( n -- n )
   ADDRESS-CELLS:ROW@ SNAP-RELOC:XTCELL-OFF-MASK and ;
: START ( -- ) ROWS BASE-N ! here data-base - BASE-DP ! ;
: DECLARED ( -- )
   ROWS BASE-N @ 1+ T=
   ROWS 1- OFFSET FAILED-OFF ! ;
: REUSED ( -- )
   ROWS BASE-N @ 1+ T=
   ROWS 1- OFFSET FAILED-OFF @ T= ;
: RESTORED ( n -- )
   12345 T=
   ROWS BASE-N @ T=
   here data-base - BASE-DP @ T= ;

: FAIL-QUOTATION ( -- )
   s" TYPED-VARIABLE FAILED [ n -- n ] FAILED @ FAILED xt! DECLARED 12345 throw"
   INCLUDE-EVALUATE ;
: FAIL-DEFER ( -- )
   s" defer FAILED ( -- n ) DECLARED 12345 throw" INCLUDE-EVALUATE ;
: FAIL-POINTER ( -- )
   s" PERSISTED-PTR-VARIABLE FAILED DECLARED 12345 throw" INCLUDE-EVALUATE ;
: THREE-SHAPES ( -- )
   s" failed quotation cell can become a persisted pointer" T-LABEL
   START [: FAIL-QUOTATION ;] catch RESTORED
   \ Stop a cell short of the failed cell: the next declaration's gap ends there.
   FAILED-OFF @ BASE-DP @ - 1 cells - allot
   s" PERSISTED-PTR-VARIABLE AFTER-QUOTATION" INCLUDE-EVALUATE REUSED
   s" failed defer cell can become a persisted pointer" T-LABEL
   START [: FAIL-DEFER ;] catch RESTORED
   s" PERSISTED-PTR-VARIABLE AFTER-DEFER" INCLUDE-EVALUATE REUSED
   s" failed persisted pointer can become an executable defer" T-LABEL
   START [: FAIL-POINTER ;] catch RESTORED
   s" defer AFTER-POINTER ( -- n ) : INSTALL-ANSWER ( -- ) [: 42 ;] is AFTER-POINTER ; INSTALL-ANSWER AFTER-POINTER 42 T="
   INCLUDE-EVALUATE REUSED ;

: FAIL-UNORDERED ( -- )
   s" PERSISTED-PTR-VARIABLE FAILED DECLARED 0 OLDER xt! 12345 throw"
   INCLUDE-EVALUATE ;
: UNORDERED ( -- )
   s" late declaration of an older cell survives rollback" T-LABEL
   START [: FAIL-UNORDERED ;] catch 12345 T=
   ROWS BASE-N @ 1+ T=
   here data-base - BASE-DP @ T=
   ROWS 1- OFFSET OLDER data-base - T=
   0 OLDER xt! ROWS BASE-N @ 1+ T=
   \ Rebuild the derived index, then reuse the retired address with a new kind.
   s" defer AFTER-UNORDERED ( -- n ) : INSTALL-OLDER ( -- ) [: 42 ;] is AFTER-UNORDERED ; INSTALL-OLDER AFTER-UNORDERED 42 T="
   INCLUDE-EVALUATE
   ROWS BASE-N @ 2 + T=
   ROWS 1- OFFSET FAILED-OFF @ T= ;

: INNER-FAILURE ( -- )
   s" PERSISTED-PTR-VARIABLE INNER-FAILED ROWS BASE-N @ 2 + T= 12345 throw"
   INCLUDE-EVALUATE ;
: NESTED-FAILURE ( -- )
   s" defer OUTER-FAILED ( -- n ) INNER-FAILURE" INCLUDE-EVALUATE ;
: NESTED ( -- )
   s" every escaped evaluation boundary retires its suffix" T-LABEL
   START [: NESTED-FAILURE ;] catch RESTORED
   s" PERSISTED-PTR-VARIABLE AFTER-NESTED" INCLUDE-EVALUATE
   ROWS BASE-N @ 1+ T= ;

: FAIL-PERSIST ( -- )
   s" ADDRESS-CELLS:PERSIST 12345 throw" INCLUDE-EVALUATE ;
: BACKING ( -- )
   s" row backing in abandoned DATA survives before DP rewinds" T-LABEL
   ADDRESS-CELLS:BOOT-CAP ROWS - 1+ 0 ?do
      SLOTS i cells + ptr-cell-mark
   loop
   ADDRESS-CELLS:MODE-FIELD HEADER@ 1 T=
   ROWS SAVED-RESERVE
   ROWS 0 ?do i ADDRESS-CELLS:ROW@ i SAVED ! loop
   START [: FAIL-PERSIST ;] catch RESTORED
   ADDRESS-CELLS:MODE-FIELD HEADER@ 1 T=
   ROWS 0 ?do i ADDRESS-CELLS:ROW@ i SAVED @ T= loop
   \ Write over the abandoned backing, then exercise its rebuilt index.
   123 ,
   SLOTS ptr-cell-mark ROWS BASE-N @ T=
   ROWS 0 ?do i ADDRESS-CELLS:ROW@ i SAVED @ T= loop
   SAVED-RELEASE ;

: LAY-TEXT ( -- ) s" address-cell-rollback-text" INTERN:TEXT drop ;
: FAIL-TEXT ( -- ) s" LAY-TEXT 12345 throw" INCLUDE-EVALUATE ;
: TEXT-GONE ( -- )
   s" a failed load's text leaves the string table" T-LABEL
   START [: FAIL-TEXT ;] catch RESTORED
   s" address-cell-rollback-text" INTERN:FIND 0 T= ;

\ The facts text `;` stores for ACR-FACTS; no other word in hb has this effect.
: FACTS$ ( -- ptr u8 n ) s" ( u16 i64 u8 -- u32 u16 ) " ;
: ACR-SOURCE$ ( -- ptr u8 n ) s" : ACR-FACTS ( u16 i64 u8 -- u32 u16 ) nip swap ;" ;
: FAIL-FACTS ( -- )
   s" : ACR-FACTS ( u16 i64 u8 -- u32 u16 ) nip swap ; 12345 throw" INCLUDE-EVALUATE ;
: FACTS-GONE ( -- )
   s" a failed line's facts text leaves the string table" T-LABEL
   FACTS$ INTERN:FIND 0 T=
   START [: FAIL-FACTS ;] catch RESTORED
   FACTS$ INTERN:FIND 0 T=
   s" a published definition's record holds its facts text's offset" T-LABEL
   ACR-SOURCE$ INCLUDE-EVALUATE
   LATEST CK-REC-FACTS-SLOT XREF-CELL@  FACTS$ INTERN:FIND data-base NULL-PTR - -  T= ;

\ here lands 3 bytes into the text's length cell.
: CUT-TEXT ( -- )
   s" a negative allot into a text forgets it" T-LABEL
   s" address-cell-rollback-cut" INTERN:TEXT CELL - 3 +
   here NULL-PTR - - allot
   s" address-cell-rollback-cut" INTERN:FIND 0 T=
   align ;

: RUN ( -- )
   T-RESET THREE-SHAPES UNORDERED NESTED BACKING TEXT-GONE FACTS-GONE CUT-TEXT T-REPORT ;
RUN
;package
