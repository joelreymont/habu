\ Dynamic typed storage uses the real compiler, allocator and accessor.
require src/lib/test.f
require src/lib/adt/option.f

package DYNAMIC-BUFFER-TEST

NEWTYPE token
CAST: TOKEN ( n -- token )
CAST: TOKEN-N ( token -- n )

DYNAMIC-BUFFER TOKENS token
DYNAMIC-BUFFER OPTIONS option<n>
\ Cells that hold addresses: the accessor hands back a pointer to a pointer, so
\ this is the element type that reads the control head's own declared cell and
\ moves pointers when a growing reserve copies the old capacity.
DYNAMIC-BUFFER ADDRS ptr u8
\ The growable BYTE row (dot habu-give-dynamic-buf-6a738e18): the element is one
\ byte, so the accessor is `ptr u8` at byte i and c@ / c! read and write it, and
\ a reserve counts bytes. birch's reproducer is this line and `DYNAMIC-BUFFER
\ CELLS n`; that second name collides with the built-in `cells`
\ (E-DUPLICATE-DEFINITION, rc 78, measured), so the bare-cell row is named here.
DYNAMIC-BUFFER BYTES u8
DYNAMIC-BUFFER CELLROW n
STRUCTURE triple DERIVE addr FIELD a n FIELD b n FIELD c n ;STRUCTURE
DYNAMIC-BUFFER WIDE triple
\ A control record no definer generated, as a downstream table's is: a declared
\ head and a control block of DYNAMIC-STORAGE:CONTROL-CELLS (four) cells, as
\ the generated `NAME#base` and `NAME#ctl` are. A shorter hand-made block would
\ overlap its neighbour's cells.
PTR-VARIABLE RAW-CTRL
create RAW-CTL 0 , 0 , 0 , 0 ,
create MARK 8 allot
variable RAW

\ A declaration-time refusal is measured by evaluating the line and catching its
\ code, as test/typed-storage-test.f does.
TYPED-VARIABLE DB-EVAL-A ptr u8
variable DB-EVAL-U
: DB-EVAL-RUN ( -- ) DB-EVAL-A @ DB-EVAL-U @ INCLUDE-EVALUATE ;
: DB-EVAL ( ptr u8 n -- n ) DB-EVAL-U ! DB-EVAL-A ! [: DB-EVAL-RUN ;] catch ;

: OPTION-N ( option<n> -- n )
   MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: BAD-LOW ( -- ) -1 TOKENS drop ;
: BAD-HIGH ( -- ) $7FFFFFFFFFFFFFFF TOKENS drop ;
\ A negative index is refused before the control record is read.
: NEG-NULL-CONTROL ( -- ) -1 NULL-PTR RAW-CTL 8 DYNAMIC-STORAGE:ADDRESS drop ;
: BAD-SIZE ( -- ) -1 TOKENS-RESERVE ;
: BAD-OVERFLOW ( -- ) $7FFFFFFFFFFFFFFF OPTIONS-RESERVE ;
: AFTER-RELEASE ( -- ) 0 TOKENS drop ;
: BAD-BYTE-LOW ( -- ) -1 BYTES drop ;
: BAD-BYTE-HIGH ( -- ) $7FFFFFFFFFFFFFFF BYTES drop ;
: AFTER-BYTE-RELEASE ( -- ) 0 BYTES drop ;
\ 100 bytes reserved from empty is a capacity of 104 - the first cell multiple
\ at or above it - and the accessor bounds by that capacity, in bytes.
: BYTE-AT-CAP ( -- ) 104 BYTES drop ;

\ The byte row retains its cell-rounded capacity across growth, including the
\ four bytes beyond the requested 100.
: BYTE-ROWS ( -- )
   100 BYTES-RESERVE
   7 0 BYTES c!
   9 99 BYTES c!
   0 BYTES c@ 7 T=
   99 BYTES c@ 9 T=
   103 BYTES c@ 0 T=
   ['] BYTE-AT-CAP 7122 TTHROWS
   100000 BYTES-RESERVE
   0 BYTES c@ 7 T=
   99 BYTES c@ 9 T=
   1 BYTES-RESERVE
   0 BYTES c@ 7 T=
   99 BYTES c@ 9 T=
   ['] BAD-BYTE-LOW 7122 TTHROWS
   ['] BAD-BYTE-HIGH 7122 TTHROWS
   s" BAD-BYTE-FETCH ( n -- n ) BYTES @" CHECK! 0 T=
   s" DYNAMIC-BUFFER BAD-U16 u16" DB-EVAL CHECKER-REJECT-RC T=
   s" BAD-U16" 0 search-wl 0= TTRUE
   BYTES-RELEASE
   ['] AFTER-BYTE-RELEASE 7122 TTHROWS
   1 BYTES-RESERVE
   65 0 BYTES c!
   0 BYTES c@ 65 T=
   BYTES-RELEASE ;

: WIDE-PAST ( -- ) 2 WIDE drop ;
: WIDE-PAST-GROWN ( -- ) 5 WIDE drop ;
: FOREIGN-WIDTH ( -- ) 2048 WIDE#base WIDE#ctl 8 DYNAMIC-STORAGE:RESERVE ;
: WIDE-2047 ( -- ) 2047 WIDE drop ;
: WIDE-682 ( -- ) 682 WIDE drop ;
: WIDE-100 ( -- ) 100 WIDE drop ;
: WIDE-2048 ( -- ) 2048 WIDE drop ;
: RAW-RESERVE ( -- ) 3 RAW-CTRL RAW-CTL 24 DYNAMIC-STORAGE:RESERVE ;
: WIDE-MAP-FAIL ( -- ) $7FFFFFFFFFFFFFFF 24 / WIDE-RESERVE ;

: WIDE-ROWS ( -- )
   0 WIDE-RESERVE
   ['] WIDE-PAST 7122 TTHROWS
   1 WIDE-RESERVE                         \ capacity 64, two complete elements
   7 0 WIDE TRIPLE-A byte-view 63 + c!    \ fractional tail belongs to mapping
   1 WIDE drop
   ['] WIDE-PAST 7122 TTHROWS
   ['] WIDE-MAP-FAIL 7138 TTHROWS
   0 WIDE TRIPLE-A byte-view 63 + c@ 7 T=
   ['] WIDE-PAST 7122 TTHROWS
   3 WIDE-RESERVE                         \ capacity doubles to 128
   0 WIDE TRIPLE-A byte-view 63 + c@ 7 T=
   4 WIDE drop
   ['] WIDE-PAST-GROWN 7122 TTHROWS
   \ A reserve through the generic entry with a foreign width is a byte request:
   \ 2048 cells grow the capacity to 16384 bytes, which hold 682 complete
   \ triples. The accessor still bounds by its declared width: 2047 would be
   \ byte 49128 and 682 is the fractional one, while 681's last field is bytes
   \ 16360..16367. The fractional tail survives the growth.
   FOREIGN-WIDTH
   ['] WIDE-2047 7122 TTHROWS
   ['] WIDE-682 7122 TTHROWS
   7 681 WIDE TRIPLE-C !
   681 WIDE TRIPLE-C @ 7 T=
   0 WIDE TRIPLE-A byte-view 63 + c@ 7 T=
   \ The generated reserve grows the foreign-width record to 49152 bytes and
   \ retags its bound for width 24: 2048 triples, the last at 2047.
   2048 WIDE-RESERVE
   2047 WIDE drop
   ['] WIDE-2048 7122 TTHROWS
   681 WIDE TRIPLE-C @ 7 T=
   0 WIDE TRIPLE-A byte-view 63 + c@ 7 T=
   \ The generic release empties the generated control for its accessor too.
   WIDE#base WIDE#ctl DYNAMIC-STORAGE:RELEASE
   ['] WIDE-100 7122 TTHROWS
   1 WIDE-RESERVE
   1 WIDE drop
   WIDE-RELEASE
   \ The downstream contract: a hand-made record reserves 72 bytes and releases
   \ to empty, its capacity read from the control block. The reserve tags the
   \ record with its width 24 and caches the bound 72 / 24.
   RAW-RESERVE
   RAW-CTL @ 72 T=
   RAW-CTL 2 cells + @ 24 T=
   RAW-CTL 3 cells + @ 3 T=
   RAW-CTRL RAW-CTL DYNAMIC-STORAGE:RELEASE
   RAW-CTL @ 0 T= ;

\ Every element admitted before the byte one keeps its old accessor: a bare
\ cell's is `( n -- ptr n )` over whole cells.
: CELL-ROWS ( -- )
   4 CELLROW-RESERVE
   5 0 CELLROW !
   6 3 CELLROW !
   0 CELLROW @ 5 T=
   3 CELLROW @ 6 T=
   CELLROW-RELEASE ;

\ RELEASE-RANGE (src/habu/aot-capture.f's capture window) owns a record only
\ when both its head and its control block lie inside the span; a span holding
\ one part, or cutting either, is E-SIZE. Generated records sit head-then-ctl,
\ so the record starts one cell before RNG-A#ctl.
DYNAMIC-BUFFER RNG-A n
DYNAMIC-BUFFER RNG-B n
DYNAMIC-BUFFER RNG-C n
: RNG-REC ( -- ptr u8 ) RNG-A#ctl byte-view CELL - ;
: RNG-WHOLE ( -- ) RNG-REC 5 cells DYNAMIC-STORAGE:RELEASE-RANGE ;
: RNG-HEAD ( -- ) RNG-REC CELL DYNAMIC-STORAGE:RELEASE-RANGE ;
: RNG-CTL ( -- ) RNG-A#ctl byte-view 4 cells DYNAMIC-STORAGE:RELEASE-RANGE ;
: RNG-CUT-HEAD ( -- ) RNG-REC 4 DYNAMIC-STORAGE:RELEASE-RANGE ;
: RNG-CUT-CTL ( -- ) RNG-REC 3 cells DYNAMIC-STORAGE:RELEASE-RANGE ;
\ Starts inside A's control block and ends at A's end: only the front cut refuses.
: RNG-CUT-TAIL ( -- ) RNG-A#ctl byte-view CELL + 3 cells DYNAMIC-STORAGE:RELEASE-RANGE ;
: RNG-BEFORE ( -- ) RNG-REC 64 - 64 DYNAMIC-STORAGE:RELEASE-RANGE ;
: RNG-EMPTY ( -- ) RNG-REC 4 + 0 DYNAMIC-STORAGE:RELEASE-RANGE ;
: RNG-NEG ( -- ) RNG-REC -1 DYNAMIC-STORAGE:RELEASE-RANGE ;
variable RNG-BASE
: RNG-N ( -- n ) DYNAMIC-STORAGE:REGISTERED-N RNG-BASE @ - ;
: RNG-SLOT ( ptr n -- n ) CELL + @ RNG-BASE @ - ;

: RANGE-ROWS ( -- )
   DYNAMIC-STORAGE:REGISTERED-N RNG-BASE !
   4 RNG-A-RESERVE 4 RNG-B-RESERVE 4 RNG-C-RESERVE
   RNG-N 3 T=
   ['] RNG-HEAD 7121 TTHROWS
   ['] RNG-CTL 7121 TTHROWS
   ['] RNG-CUT-HEAD 7121 TTHROWS
   ['] RNG-CUT-CTL 7121 TTHROWS
   ['] RNG-CUT-TAIL 7121 TTHROWS
   ['] RNG-NEG 7121 TTHROWS
   RNG-BEFORE RNG-EMPTY
   RNG-N 3 T=
   RNG-A#ctl @ 64 T=
   \ Swap removal moves the last row into the freed slot and repairs its slot.
   RNG-A-RELEASE RNG-N 2 T=
   RNG-C#ctl RNG-SLOT 1 T=
   RNG-C-RELEASE RNG-N 1 T=
   RNG-B#ctl RNG-SLOT 1 T=
   7 1 RNG-B !  1 RNG-B @ 7 T=
   RNG-B-RELEASE RNG-N 0 T=
   RNG-B#ctl CELL + @ 0 T=
   \ A span holding the whole record releases exactly that record.
   4 RNG-A-RESERVE 4 RNG-B-RESERVE
   RNG-WHOLE
   RNG-N 1 T=
   RNG-A#ctl @ 0 T=
   RNG-B#ctl @ 64 T=
   RNG-B#ctl RNG-SLOT 1 T=
   RNG-B-RELEASE
   RNG-N 0 T= ;

: RUN ( -- )
   T-RESET
   0 map-anon nip 0< TTRUE
   1 TOKENS-RESERVE
   42 TOKEN 0 TOKENS !
   1024 TOKENS-RESERVE
   0 TOKENS @ TOKEN-N 42 T=
   99 TOKEN 1023 TOKENS !
   1 TOKENS-RESERVE
   1023 TOKENS @ TOKEN-N 99 T=
   1 OPTIONS-RESERVE
   123 OPTION:SOME 0 OPTIONS !
   1024 OPTIONS-RESERVE
   0 OPTIONS @ OPTION-N 123 T=
   OPTION:NONE 1023 OPTIONS !
   1023 OPTIONS @ OPTION-N -1 T=
   4 ADDRS-RESERVE
   MARK byte-view 0 ADDRS !
   MARK byte-view 3 ADDRS !
   0 ADDRS @ MARK byte-view = TTRUE
   1024 ADDRS-RESERVE
   0 ADDRS @ MARK byte-view = TTRUE
   3 ADDRS @ MARK byte-view = TTRUE
   ['] BAD-LOW 7122 TTHROWS
   ['] BAD-HIGH 7122 TTHROWS
   ['] NEG-NULL-CONTROL 7122 TTHROWS
   ['] BAD-SIZE 7121 TTHROWS
   ['] BAD-OVERFLOW 7121 TTHROWS
   0 TOKENS @ TOKEN-N 42 T=
   s" BAD-TOKEN-STORE ( n -- ) 0 TOKENS !" CHECK! 0 T=
   s" BAD-TOKEN-POINTER ( -- ptr token ) RAW" CHECK! 0 T=
   s" BAD-DYNAMIC-DEF ( -- ) DYNAMIC-BUFFER X n" CHECK! 0 T=
   TOKENS-RELEASE
   TOKENS-RELEASE
   ['] AFTER-RELEASE 7122 TTHROWS
   1 TOKENS-RESERVE
   7 TOKEN 0 TOKENS !
   0 TOKENS @ TOKEN-N 7 T=
   TOKENS-RELEASE
   OPTIONS-RELEASE
   ADDRS-RELEASE
   BYTE-ROWS
   CELL-ROWS
   WIDE-ROWS
   RANGE-ROWS
   T-REPORT ;

RUN
;package
