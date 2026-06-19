\ table-test.f - focused tests for checked fixed-cell table helpers.
\ Run: cat lib/errors.f lib/test.f lib/array.f lib/table.f lib/table-test.f | bin/hb

create TBLT-TABLE 12 cells allot

: TBLT-CLEAR ( -- )
   0 TBLT-TABLE 12 A-FILL! ;

: TBLT-TRUE ( -- bool )
   0 0= ;

: TBLT-FALSE ( -- bool )
   0 0= 0= ;

: TBLT-MARK-FIELD ( n n n -- ) {: value row field :}
   value TBLT-TABLE 3 4 row field TBL-FIELD ! ;

: TBLT-ROW-HIGH ( -- )
   TBLT-TABLE 3 4 3 0 TBL-CELL@ drop ;

: TBLT-ROW-NEG ( -- )
   TBLT-TABLE 3 4 -1 0 TBL-CELL@ drop ;

: TBLT-FIELD-HIGH ( -- )
   TBLT-TABLE 3 4 0 4 TBL-CELL@ drop ;

: TBLT-FIELD-NEG ( -- )
   TBLT-TABLE 3 4 0 -1 TBL-CELL@ drop ;

: TBLT-PAIR-HIGH ( -- )
   s" tool" TBLT-TABLE 3 4 0 3 TBL-PAIR! ;

: TBLT-SETUP ( -- )
   T-RESET
   TBLT-CLEAR ;

: TBLT-LAYOUT ( -- )
   TBL-PAIR-CELLS 2 T=
   3 4 TBL-CELLS 12 T=
   77 0 0 TBLT-MARK-FIELD
   88 0 3 TBLT-MARK-FIELD
   99 2 1 TBLT-MARK-FIELD
   TBLT-TABLE 12 0 A@ 77 T=
   TBLT-TABLE 12 3 A@ 88 T=
   TBLT-TABLE 12 9 A@ 99 T= ;

: TBLT-GET-SET ( -- )
   99 TBLT-TABLE 3 4 1 2 TBL-N!
   TBLT-TABLE 3 4 1 2 TBL-N@ 99 T=
   TBLT-TRUE TBLT-TABLE 3 4 2 0 TBL-BOOL!
   TBLT-TABLE 3 4 2 0 TBL-BOOL@ TTRUE
   TBLT-FALSE TBLT-TABLE 3 4 2 0 TBL-BOOL!
   TBLT-TABLE 3 4 2 0 TBL-BOOL@ TFALSE
   12345 TBLT-TABLE 3 4 0 1 TBL-CELL!
   TBLT-TABLE 3 4 0 1 TBL-CELL@ 12345 T= ;

: TBLT-BOUNDS ( -- )
   ['] TBLT-ROW-HIGH E-TBL-BOUNDS TTHROWS
   ['] TBLT-ROW-NEG E-TBL-BOUNDS TTHROWS
   ['] TBLT-FIELD-HIGH E-TBL-FIELD TTHROWS
   ['] TBLT-FIELD-NEG E-TBL-FIELD TTHROWS
   ['] TBLT-PAIR-HIGH E-TBL-FIELD TTHROWS ;

: TBLT-STRING ( -- )
   s" model" TBLT-TABLE 3 4 1 2 TBL-PAIR!
   TBLT-TABLE 3 4 1 2 TBL-PAIR$ s" model" T$= ;

: TBLT-RUN ( -- )
   TBLT-SETUP
   TBLT-LAYOUT
   TBLT-GET-SET
   TBLT-BOUNDS
   TBLT-STRING
   T-REPORT ;

TBLT-RUN
