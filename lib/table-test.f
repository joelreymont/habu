\ table-test.f - focused tests for checked fixed-cell table helpers.
\ Run: bin/hb --load lib/errors.f lib/test.f lib/array.f lib/table.f lib/table-test.f

create TBLT-TABLE 12 cells allot

: TBLT-CLEAR ( -- )
   0 TBLT-TABLE 12 >LEN A-FILL! ;

: TBLT-TRUE ( -- bool )
   0 0= ;

: TBLT-FALSE ( -- bool )
   0 0= 0= ;

: TBLT-CELLS ( n n -- n ) {: rows fields :}
   rows >COUNT fields >COUNT TBL-CELLS COUNT>N ;

: TBLT-FIELD ( ptr a n n n n -- ptr a ) {: tbl:ptr rows fields row field :}
   tbl rows >COUNT fields >COUNT row >IDX field >IDX TBL-FIELD ;

: TBLT-CELL@ ( ptr a n n n n -- a ) {: tbl:ptr rows fields row field :}
   tbl rows >COUNT fields >COUNT row >IDX field >IDX TBL-CELL@ ;

: TBLT-CELL! ( a ptr a n n n n -- ) {: value tbl:ptr rows fields row field :}
   value tbl rows >COUNT fields >COUNT row >IDX field >IDX TBL-CELL! ;

: TBLT-N@ ( ptr a n n n n -- n ) {: tbl:ptr rows fields row field :}
   tbl rows >COUNT fields >COUNT row >IDX field >IDX TBL-N@ ;

: TBLT-N! ( n ptr a n n n n -- ) {: value tbl:ptr rows fields row field :}
   value tbl rows >COUNT fields >COUNT row >IDX field >IDX TBL-N! ;

: TBLT-BOOL@ ( ptr a n n n n -- bool ) {: tbl:ptr rows fields row field :}
   tbl rows >COUNT fields >COUNT row >IDX field >IDX TBL-BOOL@ ;

: TBLT-BOOL! ( bool ptr a n n n n -- ) {: value tbl:ptr rows fields row field :}
   value tbl rows >COUNT fields >COUNT row >IDX field >IDX TBL-BOOL! ;

: TBLT-PAIR! ( ptr u8 n ptr a n n n n -- ) {: a:ptr u tbl:ptr rows fields row field :}
   a u >LEN tbl rows >COUNT fields >COUNT row >IDX field >IDX TBL-PAIR! ;

: TBLT-PAIR$ ( ptr a n n n n -- ptr u8 n ) {: tbl:ptr rows fields row field :}
   tbl rows >COUNT fields >COUNT row >IDX field >IDX TBL-PAIR$ LEN>N ;

: TBLT-MARK-FIELD ( n n n -- ) {: value row field :}
   value TBLT-TABLE 3 4 row field TBLT-FIELD ! ;

: TBLT-ROW-HIGH ( -- )
   TBLT-TABLE 3 4 3 0 TBLT-CELL@ drop ;

: TBLT-ROW-NEG ( -- )
   TBLT-TABLE 3 4 -1 0 TBLT-CELL@ drop ;

: TBLT-FIELD-HIGH ( -- )
   TBLT-TABLE 3 4 0 4 TBLT-CELL@ drop ;

: TBLT-FIELD-NEG ( -- )
   TBLT-TABLE 3 4 0 -1 TBLT-CELL@ drop ;

: TBLT-CELLS-OVERFLOW ( -- )
   TBL-MAX-CELLS 2 / 1 + 2 TBLT-CELLS drop ;

: TBLT-PAIR-HIGH ( -- )
   s" tool" TBLT-TABLE 3 4 0 3 TBLT-PAIR! ;

: TBLT-SETUP ( -- )
   T-RESET
   TBLT-CLEAR ;

: TBLT-LAYOUT ( -- )
   TBL-PAIR-CELLS 2 T=
   3 4 TBLT-CELLS 12 T=
   77 0 0 TBLT-MARK-FIELD
   88 0 3 TBLT-MARK-FIELD
   99 2 1 TBLT-MARK-FIELD
   TBLT-TABLE 12 >LEN 0 >IDX A@ 77 T=
   TBLT-TABLE 12 >LEN 3 >IDX A@ 88 T=
   TBLT-TABLE 12 >LEN 9 >IDX A@ 99 T= ;

: TBLT-GET-SET ( -- )
   99 TBLT-TABLE 3 4 1 2 TBLT-N!
   TBLT-TABLE 3 4 1 2 TBLT-N@ 99 T=
   TBLT-TRUE TBLT-TABLE 3 4 2 0 TBLT-BOOL!
   TBLT-TABLE 3 4 2 0 TBLT-BOOL@ TTRUE
   TBLT-FALSE TBLT-TABLE 3 4 2 0 TBLT-BOOL!
   TBLT-TABLE 3 4 2 0 TBLT-BOOL@ TFALSE
   12345 TBLT-TABLE 3 4 0 1 TBLT-CELL!
   TBLT-TABLE 3 4 0 1 TBLT-CELL@ 12345 T= ;

: TBLT-BOUNDS ( -- )
   [: TBLT-ROW-HIGH ;] E-TBL-BOUNDS TTHROWSQ
   [: TBLT-ROW-NEG ;] E-TBL-BOUNDS TTHROWSQ
   [: TBLT-CELLS-OVERFLOW ;] E-TBL-BOUNDS TTHROWSQ
   [: TBLT-FIELD-HIGH ;] E-TBL-FIELD TTHROWSQ
   [: TBLT-FIELD-NEG ;] E-TBL-FIELD TTHROWSQ
   [: TBLT-PAIR-HIGH ;] E-TBL-FIELD TTHROWSQ ;

: TBLT-STRING ( -- )
   s" model" TBLT-TABLE 3 4 1 2 TBLT-PAIR!
   TBLT-TABLE 3 4 1 2 TBLT-PAIR$ s" model" T$= ;

: TBLT-RUN ( -- )
   TBLT-SETUP
   TBLT-LAYOUT
   TBLT-GET-SET
   TBLT-BOUNDS
   TBLT-STRING
   T-REPORT ;

TBLT-RUN
