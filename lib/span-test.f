\ span-test.f - lib/span.f: the reach travels with the pointer.
\
\ Red first: every E-SPAN code is reached through the public word that owes it,
\ at the boundary and one past it. The file also pins the measurement the type's
\ shape rests on - a `span<u8>` IS accepted where a `span<cell>` is declared, and
\ the byte reach is what makes that safe - so a later change to either the
\ widening or the reach unit fails here and not in a consumer.

require lib/errors.f
require lib/test.f
require test/checker-assert.f
require lib/span.f

\ ---- fixtures ----------------------------------------------------------------
64 SPAN-BUFFER: ST-BUF          \ 64 bytes
8 SPAN-CELLS: ST-CELLS          \ 8 cells = 64 bytes of reach
8 SPAN-BUFFER: ST-TINY          \ exactly one cell wide
4 TYPED-BUFFER ST-IDX NUM:index \ a family element: 4 NUM:index cells

package SPANT
public

\ Declared over a CELL span on purpose: the widening test calls it with a BYTE
\ span, which the checker accepts, and the byte reach is what refuses the read
\ that would have run past the end.
: CELL-READ ( SPAN:span<cell> n -- n )
   SPAN:CELL@ ;

\ Declared over `span<n>`: `n` is the universal integer, so this accepts every
\ integer-element span in both directions. That is why no word in lib/span.f is
\ declared over `span<n>`.
: N-LEN ( SPAN:span<n> -- n )
   SPAN:LEN ;

private

: SRC8 ( -- ptr u8 n )
   s" abcdefgh" ;

: SRC9 ( -- ptr u8 n )
   s" abcdefghi" ;

\ ---- the producers publish the reach with the base ----------------------------
: T-PRODUCERS ( -- )
   ST-BUF SPAN:LEN 64 T=
   ST-TINY SPAN:LEN 8 T=
   ST-CELLS SPAN:LEN 64 T=
   ST-CELLS SPAN:CELL-LEN 8 T=
   ST-BUF SPAN:$ nip 64 T= ;

\ ---- the byte set -------------------------------------------------------------
: T-BYTES ( -- )
   65 ST-BUF 0 SPAN:U8!
   90 ST-BUF 63 SPAN:U8!
   ST-BUF 0 SPAN:U8@ 65 T=
   ST-BUF 63 SPAN:U8@ 90 T=
   [: ST-BUF 64 SPAN:U8@ drop ;] E-SPAN-RANGE TTHROWS
   [: ST-BUF -1 SPAN:U8@ drop ;] E-SPAN-RANGE TTHROWS
   [: 1 ST-BUF 64 SPAN:U8! ;] E-SPAN-RANGE TTHROWS
   [: 1 ST-BUF -1 SPAN:U8! ;] E-SPAN-RANGE TTHROWS
   [: ST-BUF 64 SPAN:AT drop ;] E-SPAN-RANGE TTHROWS
   ST-BUF 63 SPAN:AT c@ 90 T= ;

\ ---- the cell set -------------------------------------------------------------
: T-CELLS ( -- )
   1234 ST-CELLS 7 SPAN:CELL!
   ST-CELLS 7 SPAN:CELL@ 1234 T=
   65 ST-CELLS 0 SPAN:CELL!
   ST-CELLS SPAN:BYTES SPAN:LEN 64 T=
   ST-CELLS SPAN:BYTES 0 SPAN:U8@ 65 T=
   [: ST-CELLS 8 SPAN:CELL@ drop ;] E-SPAN-RANGE TTHROWS
   [: ST-CELLS -1 SPAN:CELL@ drop ;] E-SPAN-RANGE TTHROWS
   [: 1 ST-CELLS 8 SPAN:CELL! ;] E-SPAN-RANGE TTHROWS
   [: ST-CELLS 8 SPAN:CELL-AT drop ;] E-SPAN-RANGE TTHROWS ;

\ ---- narrowing: at the reach is an answer, past it is a refusal ---------------
: T-NARROW ( -- )
   ST-BUF 0 SPAN:SKIP SPAN:LEN 64 T=
   ST-BUF 64 SPAN:SKIP SPAN:LEN 0 T=
   ST-BUF 0 SPAN:TAKE SPAN:LEN 0 T=
   ST-BUF 64 SPAN:TAKE SPAN:LEN 64 T=
   ST-BUF 8 SPAN:SKIP 8 SPAN:TAKE SPAN:LEN 8 T=
   ST-BUF 8 56 SPAN:SUB SPAN:LEN 56 T=
   ST-BUF 64 0 SPAN:SUB SPAN:LEN 0 T=
   [: ST-BUF 65 SPAN:SKIP SPAN:LEN drop ;] E-SPAN-RANGE TTHROWS
   [: ST-BUF -1 SPAN:SKIP SPAN:LEN drop ;] E-SPAN-RANGE TTHROWS
   [: ST-BUF 65 SPAN:TAKE SPAN:LEN drop ;] E-SPAN-RANGE TTHROWS
   [: ST-BUF -1 SPAN:TAKE SPAN:LEN drop ;] E-SPAN-RANGE TTHROWS
   [: ST-BUF 8 57 SPAN:SUB SPAN:LEN drop ;] E-SPAN-RANGE TTHROWS
   [: ST-BUF 65 0 SPAN:SUB SPAN:LEN drop ;] E-SPAN-RANGE TTHROWS
   [: ST-BUF 0 65 SPAN:SUB SPAN:LEN drop ;] E-SPAN-RANGE TTHROWS
   [: ST-BUF 8 -1 SPAN:SUB SPAN:LEN drop ;] E-SPAN-RANGE TTHROWS ;

\ A narrowed span addresses the narrowed storage and refuses the rest of it: the
\ reach shrank, so the bytes past it are unreachable through the narrow span
\ even though the buffer behind it still owns them.
: T-NARROW-ADDRESSES ( -- )
   77 ST-BUF 8 SPAN:U8!
   ST-BUF 8 SPAN:SKIP 0 SPAN:U8@ 77 T=
   ST-BUF 8 SPAN:SKIP SPAN:LEN 56 T=
   [: ST-BUF 8 SPAN:SKIP 56 SPAN:U8@ drop ;] E-SPAN-RANGE TTHROWS
   88 ST-BUF 8 8 SPAN:SUB 0 SPAN:U8!
   ST-BUF 8 SPAN:U8@ 88 T=
   [: ST-BUF 8 8 SPAN:SUB 8 SPAN:U8@ drop ;] E-SPAN-RANGE TTHROWS ;

\ ---- copy: all of it or none of it --------------------------------------------
: T-COPY ( -- )
   42 ST-TINY SPAN:FILL
   SRC8 ST-TINY SPAN:COPY                        \ exact fit
   ST-TINY 0 SPAN:U8@ 97 T=
   ST-TINY 7 SPAN:U8@ 104 T=
   SRC8 drop 0 ST-TINY SPAN:COPY                 \ zero length
   ST-TINY 0 SPAN:U8@ 97 T=
   [: SRC9 ST-TINY SPAN:COPY ;] E-SPAN-CAPACITY TTHROWS
   ST-TINY 0 SPAN:U8@ 97 T=                      \ no partial copy
   ST-TINY 7 SPAN:U8@ 104 T=
   [: SRC8 drop -1 ST-TINY SPAN:COPY ;] E-SPAN-LENGTH TTHROWS
   ST-TINY 0 SPAN:U8@ 97 T= ;

: T-COPY-NARROWED ( -- )
   42 ST-TINY SPAN:FILL
   SRC8 drop 4 ST-TINY 4 SPAN:TAKE SPAN:COPY     \ fits the narrowed reach
   ST-TINY 3 SPAN:U8@ 100 T=
   ST-TINY 4 SPAN:U8@ 42 T=                      \ untouched past the narrowing
   [: SRC8 ST-TINY 4 SPAN:TAKE SPAN:COPY ;] E-SPAN-CAPACITY TTHROWS
   ST-TINY 4 SPAN:U8@ 42 T= ;

: T-FILL ( -- )
   7 ST-BUF SPAN:FILL
   ST-BUF 0 SPAN:U8@ 7 T=
   ST-BUF 63 SPAN:U8@ 7 T=
   9 ST-BUF 8 SPAN:TAKE SPAN:FILL
   ST-BUF 7 SPAN:U8@ 9 T=
   ST-BUF 8 SPAN:U8@ 7 T= ;

\ ---- the mint -----------------------------------------------------------------
: T-MAKE ( -- )
   ST-BUF SPAN:$ SPAN:MAKE SPAN:LEN 64 T=
   ST-BUF SPAN:$ drop 0 SPAN:MAKE SPAN:LEN 0 T=
   [: ST-BUF SPAN:$ drop -1 SPAN:MAKE SPAN:LEN drop ;] E-SPAN-LENGTH TTHROWS ;

\ ---- allocated spans ----------------------------------------------------------
: T-ALLOCATION ( ptr u8 NUM:alloc-byte-len -- ) {: base:ptr bytes :}
   base bytes MEM:ALLOCATION>SPAN {: s :}
   s SPAN:LEN 37 T=
   s SPAN:$ drop base = TTRUE
   65 s 36 SPAN:U8!
   base 36 + c@ 65 T= ;

: T-ALLOC ( -- )
   37 MEM:BYTES-ALLOC-LEN [: T-ALLOCATION ;] MEM:WITH-BYTES
   128 MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: s :}
   s SPAN:LEN 128 T=
   s 127 SPAN:U8@ 0 T=
   65 s 0 SPAN:U8!
   s 0 SPAN:U8@ 65 T=
   s 64 SPAN:SKIP SPAN:LEN 64 T=
   s MEM:FREE-SPAN ;

\ ---- one type, three instantiations -------------------------------------------
\ u8, cell and a nominal family element (NUM:index, from a TYPED-BUFFER, which is
\ the only checked way to hold a `ptr NUM:index` today). The generic words are the
\ same words in all three cases.
: T-GENERIC ( -- )
   ST-BUF 8 SPAN:SKIP SPAN:LEN 56 T=
   ST-CELLS 8 SPAN:SKIP SPAN:CELL-LEN 7 T=
   0 ST-IDX 4 cells SPAN:MAKE SPAN:LEN 32 T=
   0 ST-IDX 4 cells SPAN:MAKE 8 SPAN:SKIP SPAN:LEN 24 T=
   0 ST-IDX 4 cells SPAN:MAKE 8 SPAN:TAKE SPAN:LEN 8 T=
   [: 0 ST-IDX 4 cells SPAN:MAKE 33 SPAN:SKIP SPAN:LEN drop ;] E-SPAN-RANGE TTHROWS ;

\ ---- the widening measurement the byte reach answers --------------------------
: T-WIDENING ( -- )
   \ a byte span IS accepted where a cell span is declared (u8 widens into cell)
   s" STW-OPEN ( SPAN:span<u8> n -- n ) SPANT:CELL-READ" CHECK-QUIET-CANDIDATE! -1 T=
   \ and `n` is the universal integer, in both directions
   s" STW-N ( SPAN:span<u8> -- n ) SPANT:N-LEN" CHECK-QUIET-CANDIDATE! -1 T=
   s" STW-NC ( SPAN:span<cell> -- n ) SPANT:N-LEN" CHECK-QUIET-CANDIDATE! -1 T=
   \ the byte reach is what keeps the accepted call in bounds: one whole cell
   \ fits in an 8-byte span, the second one does not
   ST-TINY 0 CELL-READ drop
   [: ST-TINY 1 CELL-READ drop ;] E-SPAN-RANGE TTHROWS
   \ the other direction stays a checker refusal at the element read
   s" STW-CU ( SPAN:span<cell> n -- u8 ) SPAN:U8@" CHECK-QUIET-CANDIDATE! 0 T=
   \ THE MINT RUNS THE SAME WIDENING AS THE CALL BOUNDARY ABOVE, in the same
   \ direction: `SPAN:MAKE` binds its element from the pointer it is given, so a
   \ byte pointer mints a `span<u8>` and that span widens into a declared
   \ `span<cell>` exactly as STW-OPEN's argument does; the reverse narrowing is
   \ refused in both places. Measured once the open `span<t>` row became
   \ placeable (dot habu-place-an-open-d7bcba49): while an open instance stayed
   \ one opaque cell, the mint answered the two directions the other way round,
   \ which disagreed with STW-OPEN on the same pair of types. Two concrete
   \ pointees still never unify (`( ptr cell -- u8 ) c@` is refused), so nothing
   \ here weakens pointer strictness, and SPAN:MAKE stays audited to lib/ and
   \ src/ because the reach it takes is unchecked.
   s" STW-MINT ( ptr u8 n -- SPAN:span<cell> ) SPAN:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
   s" STW-MINT2 ( ptr cell n -- SPAN:span<u8> ) SPAN:MAKE" CHECK-QUIET-CANDIDATE! 0 T=
   s" STW-PTRSTRICT ( ptr cell -- u8 ) c@" CHECK-QUIET-CANDIDATE! 0 T=
   \ a bare pointer is not a span and a span is not a bare pointer
   s" STW-BARE ( ptr u8 n -- n ) SPAN:LEN" CHECK-QUIET-CANDIDATE! 0 T=
   s" STW-PTR ( SPAN:span<u8> -- u8 ) c@" CHECK-QUIET-CANDIDATE! 0 T= ;

\ An OPEN instantiation is a value like any other: `span`'s parameter occurs only
\ as a pointee, so every instance is two cells and the checker places the row
\ whatever the element is (dot habu-place-an-open-d7bcba49). A local captures it
\ and a transport moves it whole; only a family whose width READS its open
\ argument stays one conservative cell.
: T-OPEN-TRANSPORT ( -- )
   s" STO-LOCAL ( SPAN:span<a> -- n ) {: s :} 0" CHECK-QUIET-CANDIDATE! -1 T=
   s" STO-DUP ( SPAN:span<a> -- n ) dup SPAN:LEN nip" CHECK-QUIET-CANDIDATE! -1 T=
   s" STO-CLOSED ( SPAN:span<u8> -- n ) dup SPAN:LEN swap SPAN:LEN +" CHECK-QUIET-CANDIDATE! -1 T= ;

public

: SPANT-MAIN ( -- )
   T-RESET
   T-PRODUCERS
   T-BYTES
   T-CELLS
   T-NARROW
   T-NARROW-ADDRESSES
   T-COPY
   T-COPY-NARROWED
   T-FILL
   T-MAKE
   T-ALLOC
   T-GENERIC
   T-WIDENING
   T-OPEN-TRANSPORT
   T-REPORT ;

SPANT-MAIN

;package

s" span-test: ok" type cr
