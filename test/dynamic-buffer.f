\ Dynamic typed storage uses the real compiler, allocator and accessor.
require src/lib/test.f
require src/lib/adt/option.f

package DYNAMIC-BUFFER-TEST

NEWTYPE token
CAST: TOKEN ( n -- token )
CAST: TOKEN-N ( token -- n )

DYNAMIC-BUFFER TOKENS token
DYNAMIC-BUFFER OPTIONS option<n>
\ Address cells: the accessor hands back a pointer to a pointer; this element
\ type reads the head's declared cell and moves pointers when a reserve copies.
DYNAMIC-BUFFER ADDRS ptr u8
\ The byte row: the accessor is `ptr u8` at byte i and a reserve counts bytes.
\ A cell row cannot be named CELLS: it collides with the built-in `cells`
\ (E-DUPLICATE-DEFINITION, rc 78, measured).
DYNAMIC-BUFFER BYTES u8
DYNAMIC-BUFFER CELLROW n
STRUCTURE triple DERIVE addr FIELD a n FIELD b n FIELD c n ;STRUCTURE
DYNAMIC-BUFFER WIDE triple
\ A control record no definer made, as a downstream table's: a declared head
\ and DYNAMIC-STORAGE:CONTROL-CELLS (two) cells; a shorter block would overlap
\ its neighbour's cells.
PTR-VARIABLE RAW-CTRL
create RAW-CTL 0 , 0 ,
create MARK 8 allot
variable RAW

\ Declaration refusals are measured by evaluating the line and catching its
\ code.
TYPED-VARIABLE DB-EVAL-A ptr u8
variable DB-EVAL-U
: DB-EVAL-RUN ( -- ) DB-EVAL-A @ DB-EVAL-U @ INCLUDE-EVALUATE ;
: DB-EVAL ( ptr u8 n -- n ) DB-EVAL-U ! DB-EVAL-A ! [: DB-EVAL-RUN ;] catch ;

: OPTION-N ( option<n> -- n )
   MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ The byte row retains its cell-rounded capacity across growth, including the
\ four bytes beyond the requested 100.
: BYTE-ROWS ( -- )
   100 BYTES-RESERVE
   7 0 BYTES c!
   9 99 BYTES c!
   0 BYTES c@ 7 T=
   99 BYTES c@ 9 T=
   103 BYTES c@ 0 T=
   100000 BYTES-RESERVE
   0 BYTES c@ 7 T=
   99 BYTES c@ 9 T=
   1 BYTES-RESERVE
   0 BYTES c@ 7 T=
   99 BYTES c@ 9 T=
   s" BAD-BYTE-FETCH ( n -- n ) BYTES @" CHECK! 0 T=
   s" DYNAMIC-BUFFER BAD-U16 u16" DB-EVAL CHECKER-REJECT-RC T=
   s" BAD-U16" 0 search-wl 0= TTRUE
   BYTES-RELEASE
   1 BYTES-RESERVE
   65 0 BYTES c!
   0 BYTES c@ 65 T=
   BYTES-RELEASE ;

: RAW-RESERVE ( -- ) 3 RAW-CTRL RAW-CTL 24 DYNAMIC-STORAGE:RESERVE ;
: WIDE-MAP-FAIL ( -- ) $7FFFFFFFFFFFFFFF 24 / WIDE-RESERVE ;

: WIDE-ROWS ( -- )
   0 WIDE-RESERVE
   1 WIDE-RESERVE                         \ capacity 64, two complete elements
   7 0 WIDE TRIPLE-A byte-view 63 + c!    \ fractional tail belongs to mapping
   1 WIDE drop
   ['] WIDE-MAP-FAIL 7138 TTHROWS
   0 WIDE TRIPLE-A byte-view 63 + c@ 7 T=
   3 WIDE-RESERVE                         \ capacity doubles to 128
   0 WIDE TRIPLE-A byte-view 63 + c@ 7 T=
   4 WIDE drop
   WIDE-RELEASE
   \ The downstream contract: a hand-made record reserves 72 bytes and releases
   \ to empty, its capacity read from the control block.
   RAW-RESERVE
   RAW-CTL @ 72 T=
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
   0 TOKENS @ TOKEN-N 42 T=
   s" BAD-TOKEN-STORE ( n -- ) 0 TOKENS !" CHECK! 0 T=
   s" BAD-TOKEN-POINTER ( -- ptr token ) RAW" CHECK! 0 T=
   s" BAD-DYNAMIC-DEF ( -- ) DYNAMIC-BUFFER X n" CHECK! 0 T=
   TOKENS-RELEASE
   TOKENS-RELEASE
   1 TOKENS-RESERVE
   7 TOKEN 0 TOKENS !
   0 TOKENS @ TOKEN-N 7 T=
   TOKENS-RELEASE
   OPTIONS-RELEASE
   ADDRS-RELEASE
   BYTE-ROWS
   CELL-ROWS
   WIDE-ROWS
   T-REPORT ;

RUN

false [if] \ off until dot habu-add-the-checks-a49d054d
: BAD-LOW ( -- ) -1 TOKENS drop ;
: BAD-HIGH ( -- ) $7FFFFFFFFFFFFFFF TOKENS drop ;
: BAD-SIZE ( -- ) -1 TOKENS-RESERVE ;
: BAD-OVERFLOW ( -- ) $7FFFFFFFFFFFFFFF OPTIONS-RESERVE ;
: AFTER-RELEASE ( -- ) 0 TOKENS drop ;
: BAD-BYTE-LOW ( -- ) -1 BYTES drop ;
: BAD-BYTE-HIGH ( -- ) $7FFFFFFFFFFFFFFF BYTES drop ;
: AFTER-BYTE-RELEASE ( -- ) 0 BYTES drop ;
\ 100 bytes reserved from empty is a capacity of 104 - the first cell multiple
\ at or above it - and the accessor bounds by that capacity, in bytes.
: BYTE-AT-CAP ( -- ) 104 BYTES drop ;
: WIDE-PAST ( -- ) 2 WIDE drop ;
: WIDE-PAST-GROWN ( -- ) 5 WIDE drop ;

: BOUNDS-ROWS ( -- )
   T-RESET
   1 TOKENS-RESERVE
   ['] BAD-LOW 7122 TTHROWS
   ['] BAD-HIGH 7122 TTHROWS
   ['] BAD-SIZE 7121 TTHROWS
   ['] BAD-OVERFLOW 7121 TTHROWS
   TOKENS-RELEASE
   ['] AFTER-RELEASE 7122 TTHROWS
   100 BYTES-RESERVE
   ['] BYTE-AT-CAP 7122 TTHROWS
   ['] BAD-BYTE-LOW 7122 TTHROWS
   ['] BAD-BYTE-HIGH 7122 TTHROWS
   BYTES-RELEASE
   ['] AFTER-BYTE-RELEASE 7122 TTHROWS
   0 WIDE-RESERVE
   ['] WIDE-PAST 7122 TTHROWS
   1 WIDE-RESERVE
   ['] WIDE-PAST 7122 TTHROWS
   3 WIDE-RESERVE
   ['] WIDE-PAST-GROWN 7122 TTHROWS
   T-REPORT ;

BOUNDS-ROWS
[then]
;package
