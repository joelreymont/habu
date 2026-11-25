\ A caught failed text keeps the address cells it declared.
require src/lib/test/assert.f
require src/habu/address-cells.f

package ADDRESS-CELL-ROLLBACK
variable BASE-N
variable BASE-HERE
variable FAILED-OFF
create OLDER CELL allot

: ROWS ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;
: OFFSET ( n -- n )
   ADDRESS-CELLS:ROW@ RELOC:XTCELL-OFF-MASK and ;
: START ( -- ) ROWS BASE-N ! here NULL-PTR - BASE-HERE ! ;
: DECLARED ( -- )
   ROWS BASE-N @ 1+ T=
   ROWS 1- OFFSET FAILED-OFF ! ;
\ The failed text's k rows stay, and so does its data.
: KEPT ( n n -- ) {: rc:n k:n :}
   rc 12345 T=
   ROWS BASE-N @ k + T=
   here NULL-PTR - BASE-HERE @ k cells + >= TTRUE ;
\ A declaration after it adds a row of its own, past the failed cell.
: ADDED ( n -- ) {: k:n :}
   ROWS BASE-N @ k + 1+ T=
   ROWS 1- OFFSET FAILED-OFF @ > TTRUE ;

: FAIL-QUOTATION ( -- )
   s" TYPED-VARIABLE FAILED [ n -- n ] FAILED @ FAILED xt! DECLARED 12345 throw"
   evaluate-closed ;
: FAIL-DEFER ( -- )
   s" defer FAILED ( -- n ) DECLARED 12345 throw" evaluate-closed ;
: FAIL-POINTER ( -- )
   s" PERSISTED-PTR-VARIABLE FAILED DECLARED 12345 throw" evaluate-closed ;
: THREE-SHAPES ( -- )
   s" a failed quotation cell keeps its row" T-LABEL
   START [: FAIL-QUOTATION ;] catch 1 KEPT
   s" PERSISTED-PTR-VARIABLE AFTER-QUOTATION" evaluate-closed 1 ADDED
   s" a failed defer cell keeps its row" T-LABEL
   START [: FAIL-DEFER ;] catch 1 KEPT
   s" PERSISTED-PTR-VARIABLE AFTER-DEFER" evaluate-closed 1 ADDED
   s" a failed persisted pointer keeps its row" T-LABEL
   START [: FAIL-POINTER ;] catch 1 KEPT
   s" defer AFTER-POINTER ( -- n ) : INSTALL-ANSWER ( -- ) [: 42 ;] is AFTER-POINTER ; INSTALL-ANSWER AFTER-POINTER 42 T="
   evaluate-closed 1 ADDED ;

: FAIL-UNORDERED ( -- )
   s" PERSISTED-PTR-VARIABLE FAILED DECLARED 0 OLDER xt! 12345 throw"
   evaluate-closed ;
: UNORDERED ( -- )
   s" a failed text's late declaration of an older cell stays" T-LABEL
   START [: FAIL-UNORDERED ;] catch 2 KEPT
   \ OLDER's row is among them: declaring OLDER again adds none.
   0 OLDER xt!  ROWS BASE-N @ 2 + T=
   s" defer AFTER-UNORDERED ( -- n ) : INSTALL-OLDER ( -- ) [: 42 ;] is AFTER-UNORDERED ; INSTALL-OLDER AFTER-UNORDERED 42 T="
   evaluate-closed 2 ADDED ;

: INNER-FAILURE ( -- )
   s" PERSISTED-PTR-VARIABLE INNER-FAILED ROWS BASE-N @ 2 + T= 12345 throw"
   evaluate-closed ;
: NESTED-FAILURE ( -- )
   s" defer OUTER-FAILED ( -- n ) INNER-FAILURE" evaluate-closed ;
: NESTED ( -- )
   s" every escaped evaluation boundary keeps its rows" T-LABEL
   START [: NESTED-FAILURE ;] catch 2 KEPT
   s" PERSISTED-PTR-VARIABLE AFTER-NESTED" evaluate-closed
   ROWS BASE-N @ 3 + T= ;

\ here lands 3 bytes into the text's length cell.
: CUT-TEXT ( -- )
   s" a negative allot into a text forgets it" T-LABEL
   s" address-cell-rollback-cut" INTERN:TEXT CELL - 3 +
   here NULL-PTR - - allot
   s" address-cell-rollback-cut" INTERN:FIND 0 T=
   align ;

: RUN ( -- )
   T-RESET THREE-SHAPES UNORDERED NESTED CUT-TEXT T-REPORT ;
RUN
;package
