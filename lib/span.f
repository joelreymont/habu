\ span.f - a pointer that carries its reach.
\
\ A span is ONE two-cell value: a base `ptr t` and a REACH IN BYTES. It travels
\ on the stack like any other family value, so it needs no engine change, and it
\ is the writable destination and the indexed view that a bare `ptr t` cannot be:
\ every access through it is bounds-checked against the reach it carries, so a
\ forgotten `u cap > if throw` is not a possible mistake at the call.
\ The read-only ( ptr u8 n ) source pair stays the string idiom - it already
\ carries its length - and SPAN:COPY takes exactly that pair as its source.
\
\ THE REACH IS COUNTED IN BYTES, NOT IN ELEMENTS, AND THAT IS A SOUNDNESS
\ DECISION, not a convenience. A family argument unifies with the ordinary
\ integer widening lattice, so `span<u8>` IS ACCEPTED where `span<cell>` is
\ declared (u8 widens into cell; measured, lib/span-test.f pins it). With an
\ element-counted reach a 64-byte `span<u8>` handed to a word declared over
\ `span<cell>` would pass `i < len` at i=7 and cell-read bytes 56..63 - eight
\ times past its end, the exact overrun this type exists to refuse. With a byte
\ reach the same call stays inside 64 bytes: SPAN:CELL-AT checks `i < reach/CELL`
\ and every other accessor checks bytes too, so a widened element type is a
\ semantic surprise and never an out-of-bounds access. Closing the widening
\ itself is a checker question (the campaign's band 3), not a declaration's.
\
\ WHO MAY CALL SPAN:MAKE. It is the one crossing where an address and a number
\ become a reach, so it is where a wrong number becomes an overrun. Only lib/
\ and src/ may call it: a producer there publishes spans (SPAN-BUFFER:,
\ SPAN-CELLS:, MEM:ALLOC-SPAN) and every consumer takes one from a producer or
\ narrows one it was given. tools/lint/bare-copy-lint.f reports a call anywhere
\ else, with the site and the replacement named. It is an ordinary checked word:
\ the body is the generated constructor plus a sign check, it trusts nothing, and
\ the audit is the lint rather than a TRUST row.
\
\ WHAT IS NOT HERE. A generic element-indexed accessor: `+` on a `ptr t` is byte
\ arithmetic and checked code has no element size for a type parameter, so an
\ `AT` that steps by elements is not expressible. The byte set (AT, U8@, U8!)
\ works over `span<u8>`, the cell set (CELL-AT, CELL@, CELL!, CELL-LEN) over
\ `span<cell>`, and SKIP/TAKE/SUB narrow in bytes for every element type. There
\ is no TYPED-BUFFER span form either: its generated accessor is an index
\ function ( n -- ptr t ), not a base-and-reach pair, and adding one means
\ changing the generated accessor set at the sealed generative storage boundary
\ (src/core/layout-buffer.f). A span over a nominal element is already reachable
\ today - hand a TYPED-BUFFER accessor result to SPAN:MAKE.

require lib/errors.f
require lib/memory.f

package SPAN
public

\ base is `ptr a` so the pointee survives (c@ on a byte span, @ on a cell span,
\ and nothing else); len is the reach in bytes.
STRUCTURE span 1
   FIELD base ptr a
   FIELD len n
;STRUCTURE

private

1 cells constant CELL-BYTES

\ A narrowing offset is good when it lands inside the reach, the end included -
\ an empty span at the end is an ordinary answer, one byte past it is not.
: BOUND ( n n -- ) {: l k :}
   k 0 < k l > or if E-SPAN-RANGE throw then ;

public

\ ---- the audited crossing ----------------------------------------------------
\ lib/ and src/ only. `n` is the reach in BYTES from `p`, and the caller owes
\ that fact: the checker knows the pointee, never the extent behind it.
: MAKE ( ptr t n -- span<t> )
   dup 0 < if E-SPAN-LENGTH throw then
   SPAN-SPAN:MAKE ;

\ ---- what a span says about itself -------------------------------------------
: LEN ( span<t> -- n )
   SPAN-SPAN:UNMAKE nip ;

: CELL-LEN ( span<cell> -- n )
   LEN CELL-BYTES / ;

: $ ( span<u8> -- ptr u8 n )
   SPAN-SPAN:UNMAKE ;

: BYTES ( span<cell> -- span<u8> )
   SPAN-SPAN:UNMAKE {: b l :}
   b byte-view l MAKE ;

\ ---- narrowing: never wider, never past the reach ----------------------------
\ These three are generic. An open instantiation is a placeable two-cell value
\ like any other (a `span`'s element is only ever a pointee, so no argument can
\ move its width), so a local would work here too; the return stack keeps the
\ scalar out of the way with no frame, and the bundle is unmade where it stands.
: SKIP ( span<t> n -- span<t> )
   >r SPAN-SPAN:UNMAKE r>
   2dup BOUND
   tuck - >r + r> MAKE ;

: TAKE ( span<t> n -- span<t> )
   >r SPAN-SPAN:UNMAKE r>
   2dup BOUND
   nip MAKE ;

\ at past the reach and a length past what is left are the two refusals SKIP and
\ TAKE already make, in that order, so SUB is their composition and not a third
\ bounds rule to keep in step with them.
: SUB ( span<t> n n -- span<t> )
   >r SKIP r> TAKE ;

\ ---- the byte set ------------------------------------------------------------
: AT ( span<u8> n -- ptr u8 ) {: s i :}
   s SPAN-SPAN:UNMAKE {: b l :}
   i 0 < i l >= or if E-SPAN-RANGE throw then
   b i + ;

: U8@ ( span<u8> n -- u8 )
   AT c@ ;

: U8! ( u8 span<u8> n -- ) {: v s i :}
   v s i AT c! ;

\ ---- the cell set ------------------------------------------------------------
\ The index is checked against the reach in WHOLE cells, so a byte span that
\ reached here through integer widening still cannot address past its end.
: CELL-AT ( span<cell> n -- ptr cell ) {: s i :}
   s SPAN-SPAN:UNMAKE {: b l :}
   i 0 < i l CELL-BYTES / >= or if E-SPAN-RANGE throw then
   b i cells + ;

: CELL@ ( span<cell> n -- n )
   CELL-AT @ ;

: CELL! ( n span<cell> n -- ) {: v s i :}
   v s i CELL-AT ! ;

\ ---- bulk: all of it or none of it -------------------------------------------
: COPY ( ptr u8 n span<u8> -- ) {: src u s :}
   u 0 < if E-SPAN-LENGTH throw then
   s SPAN-SPAN:UNMAKE {: b l :}
   u l > if E-SPAN-CAPACITY throw then
   src b u BYTE-COPY ;

: FILL ( u8 span<u8> -- ) {: v s :}
   s SPAN-SPAN:UNMAKE {: b l :}
   0 begin dup l < while
      v over b + c!
      1+
   repeat drop ;

;package

\ ---- allocated spans ----------------------------------------------------------
\ Package MEM, reopened here rather than written in lib/memory.f: that file is a
\ boot-prefix row and must not depend on this one (lib/memory.f says the same at
\ its allocation surface). The private projection is proof erasure - SPAN:MAKE
\ takes the reach as a raw byte count - and has no public inverse.
package MEM
private

CAST: SPAN-ALLOC-LEN>N ( NUM:alloc-byte-len -- n )

public

: ALLOCATION>SPAN ( ptr u8 NUM:alloc-byte-len -- SPAN:span<u8> )
   SPAN-ALLOC-LEN>N SPAN:MAKE ;

: ALLOC-SPAN ( NUM:alloc-byte-len -- SPAN:span<u8> )
   MEM:ALLOC-BYTES ALLOCATION>SPAN ;

: FREE-SPAN ( SPAN:span<u8> -- )
   SPAN:$ MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

;package

\ ---- static span producers ----------------------------------------------------
\ `n SPAN-BUFFER: NAME` publishes NAME as the whole buffer, reach and all, so no
\ caller of NAME ever holds the base without the reach. The body is [reach][data]
\ and the clause reads the reach it was built with; `create`'s raw pointee takes
\ the byte con through `byte-view` and the cell con directly, so neither definer
\ mints anything and neither needs a trusted row.
: SPAN-BUFFER: ( n -- )
   create dup , allot
   does> ( -- SPAN:span<u8> ) dup @ >r cell+ byte-view r> SPAN:MAKE ;

: SPAN-CELLS: ( n -- )
   create dup cells , cells allot
   does> ( -- SPAN:span<cell> ) dup @ >r cell+ r> SPAN:MAKE ;
