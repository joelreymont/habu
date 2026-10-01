\ Fixed storage binds the accessor that owns its allocation, and capture walks
\ only cells whose stored type can contain a quotation.
require lib/test.f
require src/habu/address-cells.f

package STORAGE-BIND-TEST

variable SAVED-CHECK
check@ SAVED-CHECK !
0 set-check
TYPED-VARIABLE UNCHECKED-N n
SAVED-CHECK @ set-check

STRUCTURE plain-row 0
   FIELD first n
   FIELD second ptr u8
;STRUCTURE

4096 TYPED-BUFFER PLAIN plain-row

private
TYPED-VARIABLE ROW [ n -- n ]
: PRIVATE-STORE ( -- ) [: 1 + ;] ROW ! ;
: PRIVATE-CALL ( n -- n ) ROW @ execute ;
TRUSTED: PRIVATE-OFF ( -- n ) ROW BYTE-VIEW data-base BYTE-VIEW - ;

public
\ The public accessor has the same tail as the private quote accessor. Record
\ its own allocation directly: bare name lookup in this scope prefers the
\ private word, while the generated public accessor still mints its own row.
PTR-VARIABLE PUBLIC-BASE
here PUBLIC-BASE !
TYPED-VARIABLE ROW n
: PUBLIC-STORE ( n -- ) STORAGE-BIND-TEST:ROW ! ;
: PUBLIC-READ ( -- n ) STORAGE-BIND-TEST:ROW @ ;
TRUSTED: PUBLIC-OFF ( -- n ) PUBLIC-BASE @ BYTE-VIEW data-base BYTE-VIEW - ;

TRUSTED: CAPTURE-STORAGE ( -- ) CHECKER-CAPTURE-PREPARE ;

: XT-MARKED? ( n -- bool ) {: off:n :}
   ADDRESS-CELLS:LIVE-SPAN nip 0 ?do
      i ADDRESS-CELLS:ROW@ {: row:n :}
      row SNAP-RELOC:XTCELL-OFF-MASK and off =
      row SNAP-RELOC:XTCELL-DATA-TAG and 0= and if
         true unloop exit
      then
   loop false ;

: RUN ( -- )
   17 PUBLIC-STORE PUBLIC-READ 17 T=
   PRIVATE-STORE 5 PRIVATE-CALL 6 T=
   3 s" abc" drop PLAIN-ROW-MAKE 4095 PLAIN !
   4095 PLAIN @ PLAIN-ROW-UNMAKE 3
   s" abc" T$= 3 T=
   CAPTURE-STORAGE
   PRIVATE-OFF XT-MARKED? TTRUE
   PUBLIC-OFF XT-MARKED? TFALSE
   5 PRIVATE-CALL 6 T=
   T-REPORT ;

T-RESET
9 UNCHECKED-N ! UNCHECKED-N @ 9 T=
RUN
;package
