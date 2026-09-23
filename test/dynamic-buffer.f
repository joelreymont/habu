\ Dynamic typed storage uses the real compiler, allocator and accessor.
require lib/test.f
require lib/adt/option.f

package DYNAMIC-BUFFER-TEST

NEWTYPE token 0
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
: BAD-SIZE ( -- ) -1 TOKENS-RESERVE ;
: BAD-OVERFLOW ( -- ) $7FFFFFFFFFFFFFFF OPTIONS-RESERVE ;
: AFTER-RELEASE ( -- ) 0 TOKENS drop ;
: BAD-BYTE-LOW ( -- ) -1 BYTES drop ;
: BAD-BYTE-HIGH ( -- ) $7FFFFFFFFFFFFFFF BYTES drop ;
: AFTER-BYTE-RELEASE ( -- ) 0 BYTES drop ;
\ 100 bytes reserved from empty is a capacity of 104 - the first cell multiple
\ at or above it - and the accessor bounds by that capacity, in bytes.
: BYTE-AT-CAP ( -- ) 104 BYTES drop ;

\ The byte row grows from a capacity that is NOT a cell multiple: a first
\ reserve of 100 bytes is the whole capacity (need beats both 2 * old and the
\ 64-byte floor), and the move that follows copies it cell by cell. Rounding
\ that capacity up to a cell is what keeps byte 99; without it the growth copies
\ 100 CELL / = 12 cells = 96 bytes and drops the last four.
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
   s" DYNAMIC-BUFFER BAD-U16 u16" DB-EVAL 7121 T=
   s" BAD-U16" 0 search-wl 0= TTRUE
   BYTES-RELEASE
   ['] AFTER-BYTE-RELEASE 7122 TTHROWS
   1 BYTES-RESERVE
   65 0 BYTES c!
   0 BYTES c@ 65 T=
   BYTES-RELEASE ;

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
   ['] BAD-LOW 7122 TTHROWS
   ['] BAD-HIGH 7122 TTHROWS
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
   T-REPORT ;

RUN
;package
