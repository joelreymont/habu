\ Fixed storage binds the accessor that owns its allocation, and capture marks
\ cells by their stored type: a quotation cell as code, a nullable cell as a
\ DATA address whether it holds an address or zero.
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

\ A pointer to a family value binds no ptr a, so each nullable cell's address
\ is recorded before it is allocated, as PUBLIC-BASE records ROW's.
PTR-VARIABLE NULL-SOME-BASE
here NULL-SOME-BASE !
TYPED-VARIABLE NULL-SOME nullable<u8>
PTR-VARIABLE NULL-NONE-BASE
here NULL-NONE-BASE !
TYPED-VARIABLE NULL-NONE nullable<u8>
: NULL-STORE ( -- )
   PUBLIC-BASE @ BYTE-VIEW NULLABLE:SOME NULL-SOME ! ;

: RUN ( -- )
   17 PUBLIC-STORE PUBLIC-READ 17 T=
   PRIVATE-STORE 5 PRIVATE-CALL 6 T=
   3 s" abc" drop PLAIN-ROW-MAKE 4095 PLAIN !
   4095 PLAIN @ PLAIN-ROW-UNMAKE 3
   s" abc" T$= 3 T=
   NULL-STORE
   ['] CHECKER-CAPTURE-PREPARE catch 0 T=
   PRIVATE-OFF XT-MARKED? TTRUE
   PUBLIC-OFF XT-MARKED? TFALSE
   NULL-SOME-BASE @ BYTE-VIEW data-base BYTE-VIEW - DATA-MARKED? TTRUE
   NULL-NONE-BASE @ BYTE-VIEW data-base BYTE-VIEW - DATA-MARKED? TTRUE
   5 PRIVATE-CALL 6 T=
   T-REPORT ;

RUN
;package
