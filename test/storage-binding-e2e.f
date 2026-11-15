\ Fixed storage binds the accessor that owns its allocation, and the binding
\ marks cells by their stored type: a quotation cell as code, a nullable cell as
\ a DATA address whether it holds an address or zero.
require src/lib/test.f
require src/habu/address-cells.f
require src/lib/adt/nullable.f

package STORAGE-BIND-TEST

T-RESET

STRUCTURE plain-row
   FIELD first n
   FIELD second ptr u8
;STRUCTURE

4096 TYPED-BUFFER PLAIN plain-row

private
TYPED-VARIABLE ROW [ n -- n ]
: PRIVATE-STORE ( -- ) [: 1 + ;] ROW ! ;
: PRIVATE-CALL ( n -- n ) ROW @ execute ;
: PRIVATE-OFF ( -- n ) ROW data-base - ;

public
\ The public accessor has the same tail as the private quote accessor. Record
\ its own allocation directly: bare name lookup in this scope prefers the
\ private word, while the generated public accessor still mints its own row.
PTR-VARIABLE PUBLIC-BASE
here PUBLIC-BASE !
TYPED-VARIABLE ROW n
: PUBLIC-STORE ( n -- ) STORAGE-BIND-TEST:ROW ! ;
: PUBLIC-READ ( -- n ) STORAGE-BIND-TEST:ROW @ ;
: PUBLIC-OFF ( -- n ) PUBLIC-BASE @ BYTE-VIEW data-base BYTE-VIEW - ;

\ The kind bits of the address-cell row at a DATA offset, -1 for no row.
: CELL-KIND ( n -- n ) {: off:n :}
   ADDRESS-CELLS:LIVE-SPAN nip 0 ?do
      i ADDRESS-CELLS:ROW@ {: row:n :}
      row SNAP-RELOC:XTCELL-OFF-MASK and off = if
         row SNAP-RELOC:XTCELL-OFF-MASK invert and unloop exit
      then
   loop -1 ;
: XT-MARKED? ( n -- bool ) CELL-KIND 0 = ;
: DATA-MARKED? ( n -- bool ) CELL-KIND SNAP-RELOC:XTCELL-DATA-TAG = ;

TYPED-VARIABLE NULL-SOME nullable<u8>
TYPED-VARIABLE NULL-NONE nullable<u8>
: NULL-STORE ( -- )
   PUBLIC-BASE @ BYTE-VIEW NULLABLE:SOME NULL-SOME ! ;

\ A created word's `,` cells are its storage grid, and a child the definer
\ anchors between `create` and `does>` ends them: each grid starts at its
\ word's body and counts the cells before the child; the control has none.
: SB-GA ( -- ) create 1 , 2 , ARENA 3 , 4 , does> ( -- ptr a ) ;
: SB-GH ( -- ) create 1 , 2 , HEAP 3 , 4 , does> ( -- ptr a ) ;
: SB-GC ( -- ) create 1 , 2 , 3 , 4 , does> ( -- ptr a ) ;
: SB-LASTC ( -- ptr n ) data-base LASTC-CELL + 0 ptr-field @ ;
PTR-VARIABLE SB-GA-REC
PTR-VARIABLE SB-GH-REC
PTR-VARIABLE SB-GC-REC
SB-GA SB-GA-X SB-GA-Z n  SB-LASTC SB-GA-REC !
SB-GH SB-GH-X SB-GH-Z n  SB-LASTC SB-GH-REC !
SB-GC SB-GC-X  SB-LASTC SB-GC-REC !
create SB-G-FENCE

\ The record's grid starts at the body BODY and covers WANT cells, holding 1 up.
: GRID-IS ( ptr n ptr n n -- ) {: rec:ptr body:ptr want:n :}
   rec RECORD-GRID-BEFORE {: grid:n :}
   grid 0<> TTRUE
   grid 0= if exit then
   grid RECORD-GRID-REC {: er:ptr :}
   er ER.STORAGE-OFF @ body PTR>N data-base PTR>N - T=
   er ER.STORAGE-N @ want T=
   want 0 ?do body i cells + @ i 1+ T= loop ;

: GRID-RUN ( -- )
   SB-GA-REC @ SB-GA-X 2 GRID-IS
   SB-GH-REC @ SB-GH-X 2 GRID-IS
   SB-GC-REC @ SB-GC-X 4 GRID-IS ;

: RUN ( -- )
   GRID-RUN
   17 PUBLIC-STORE PUBLIC-READ 17 T=
   PRIVATE-STORE 5 PRIVATE-CALL 6 T=
   3 s" abc" drop PLAIN-ROW-MAKE 4095 PLAIN !
   4095 PLAIN @ PLAIN-ROW-UNMAKE 3
   s" abc" T$= 3 T=
   NULL-STORE
   PRIVATE-OFF XT-MARKED? TTRUE
   PUBLIC-OFF XT-MARKED? TFALSE
   NULL-SOME PTR>N data-base PTR>N - DATA-MARKED? TTRUE
   NULL-NONE PTR>N data-base PTR>N - DATA-MARKED? TTRUE
   5 PRIVATE-CALL 6 T=
   T-REPORT ;

RUN
;package
