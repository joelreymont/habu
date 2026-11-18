split-str
\ Evaluation rollback retires declarations by address, not registration order.
require src/lib/test.f
require src/habu/address-cells.f

package ADDRESS-CELL-ROLLBACK
variable BASE-N
variable BASE-HERE
variable FAILED-AT
variable FAILED-OFF
create OLDER CELL allot

: ROWS ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;
: OFFSET ( n -- n )
   ADDRESS-CELLS:ROW@ SNAP-RELOC:XTCELL-OFF-MASK and ;
: START ( -- ) ROWS BASE-N ! here NULL-PTR - BASE-HERE ! ;
: DECLARED ( -- )
   ROWS BASE-N @ 1+ T=
   ROWS 1- OFFSET FAILED-OFF ! ;
: REUSED ( -- )
   ROWS BASE-N @ 1+ T=
   ROWS 1- OFFSET FAILED-OFF @ T= ;
: RESTORED ( n -- )
   12345 T=
   ROWS BASE-N @ T=
   here NULL-PTR - BASE-HERE @ T= ;

: FAIL-QUOTATION ( -- )
   s" TYPED-VARIABLE FAILED [ n -- n ] FAILED @ FAILED xt! FAILED BYTE-VIEW NULL-PTR - FAILED-AT ! DECLARED 12345 throw"
   evaluate-closed ;
: FAIL-DEFER ( -- )
   s" defer FAILED ( -- n ) DECLARED 12345 throw" evaluate-closed ;
: FAIL-POINTER ( -- )
   s" PERSISTED-PTR-VARIABLE FAILED DECLARED 12345 throw" evaluate-closed ;
: THREE-SHAPES ( -- )
   s" failed quotation cell can become a persisted pointer" T-LABEL
   START [: FAIL-QUOTATION ;] catch RESTORED
   \ Stop a cell short of the failed cell: the next declaration's gap ends there.
   FAILED-AT @ BASE-HERE @ - 1 cells - allot
   s" PERSISTED-PTR-VARIABLE AFTER-QUOTATION" evaluate-closed REUSED
   s" failed defer cell can become a persisted pointer" T-LABEL
   START [: FAIL-DEFER ;] catch RESTORED
   s" PERSISTED-PTR-VARIABLE AFTER-DEFER" evaluate-closed REUSED
   s" failed persisted pointer can become an executable defer" T-LABEL
   START [: FAIL-POINTER ;] catch RESTORED
   s" defer AFTER-POINTER ( -- n ) : INSTALL-ANSWER ( -- ) [: 42 ;] is AFTER-POINTER ; INSTALL-ANSWER AFTER-POINTER 42 T="
   evaluate-closed REUSED ;

: FAIL-UNORDERED ( -- )
   s" PERSISTED-PTR-VARIABLE FAILED DECLARED 0 OLDER xt! 12345 throw"
   evaluate-closed ;
: UNORDERED ( -- )
   s" late declaration of an older cell survives rollback" T-LABEL
   START [: FAIL-UNORDERED ;] catch 12345 T=
   ROWS BASE-N @ 1+ T=
   here NULL-PTR - BASE-HERE @ T=
   \ The row that survived is OLDER's: declaring OLDER again adds none.
   0 OLDER xt!  ROWS BASE-N @ 1+ T=
   \ Rebuild the derived index, then reuse the retired address with a new kind.
   s" defer AFTER-UNORDERED ( -- n ) : INSTALL-OLDER ( -- ) [: 42 ;] is AFTER-UNORDERED ; INSTALL-OLDER AFTER-UNORDERED 42 T="
   evaluate-closed
   ROWS BASE-N @ 2 + T=
   ROWS 1- OFFSET FAILED-OFF @ T= ;

: INNER-FAILURE ( -- )
   s" PERSISTED-PTR-VARIABLE INNER-FAILED ROWS BASE-N @ 2 + T= 12345 throw"
   evaluate-closed ;
: NESTED-FAILURE ( -- )
   s" defer OUTER-FAILED ( -- n ) INNER-FAILURE" evaluate-closed ;
: NESTED ( -- )
   s" every escaped evaluation boundary retires its suffix" T-LABEL
   START [: NESTED-FAILURE ;] catch RESTORED
   s" PERSISTED-PTR-VARIABLE AFTER-NESTED" evaluate-closed
   ROWS BASE-N @ 1+ T= ;

: LAY-TEXT ( -- ) s" address-cell-rollback-text" INTERN:TEXT drop ;
: FAIL-TEXT ( -- ) s" LAY-TEXT 12345 throw" evaluate-closed ;
: TEXT-GONE ( -- )
   s" a failed load's text leaves the string table" T-LABEL
   START [: FAIL-TEXT ;] catch RESTORED
   s" address-cell-rollback-text" INTERN:FIND 0 T= ;

\ The facts text `;` stores for ACR-FACTS; no other word in hb has this effect.
: FACTS$ ( -- str ) s" ( u16 i64 u8 -- u32 u16 ) " ;
: FAIL-FACTS ( -- )
   s" : ACR-FACTS ( u16 i64 u8 -- u32 u16 ) nip swap ; 12345 throw" evaluate-closed ;
: FACTS-GONE ( -- )
   s" a failed line's facts text leaves the string table" T-LABEL
   FACTS$ INTERN:FIND 0 T=
   START [: FAIL-FACTS ;] catch RESTORED
   FACTS$ INTERN:FIND 0 T= ;

\ here lands 3 bytes into the text's length cell.
: CUT-TEXT ( -- )
   s" a negative allot into a text forgets it" T-LABEL
   s" address-cell-rollback-cut" INTERN:TEXT CELL - 3 +
   here NULL-PTR - - allot
   s" address-cell-rollback-cut" INTERN:FIND 0 T=
   align ;

: RUN ( -- )
   T-RESET THREE-SHAPES UNORDERED NESTED TEXT-GONE FACTS-GONE CUT-TEXT T-REPORT ;
RUN
;package
