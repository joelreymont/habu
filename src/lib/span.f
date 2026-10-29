\ span.f - a pointer that carries its reach: one two-cell value, a base `ptr t`
\ and a reach in bytes, every access bounds-checked against it. The read-only
\ ( ptr u8 n ) pair stays the string idiom; SPAN:COPY takes it as its source.
\ STORAGE CLASS. CALLER-OWNED: a span names its producer's storage; SPAN-BUFFER:
\ and SPAN-CELLS: allot theirs.

\ A narrowing or index outside the reach is E-SPAN-RANGE, a copy longer than the
\ destination E-SPAN-CAPACITY (nothing moves), a negative reach or length
\ E-SPAN-LENGTH.

require src/lib/errors.f

package SPAN
public

\ base is `ptr a` so the pointee survives (c@ on a byte span, @ on a cell span,
\ and nothing else); len is the reach in bytes, a soundness rule: a family
\ argument widens like an integer, so `span<u8>` is accepted where `span<cell>` is
\ declared (src/lib/span-test.f pins it). An element-counted reach would let a
\ 64-byte `span<u8>` pass `i < len` at i=7 and cell-read bytes 56..63; a byte
\ reach keeps every accessor inside it. Closing the widening is the checker's.
STRUCTURE span<a>
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

\ The one crossing where an address and a number become a reach. Only src/lib/
\ and src/ call it: producers there publish spans (SPAN-BUFFER:, SPAN-CELLS:, and
\ MEM:ALLOC-SPAN in src/lib/memory.f), and every consumer takes one from a
\ producer or narrows one. tools/lint/bare-copy-lint.f reports a call anywhere
\ else; the audit is that lint, not a TRUST row. `n` is the reach in bytes from
\ `p`, and the caller owes that fact: the checker knows the pointee, never the
\ extent behind it.
: MAKE ( ptr t n -- span<t> )
   dup 0 < if E-SPAN-LENGTH throw then
   SPAN-SPAN:MAKE ;

: LEN ( span<t> -- n )
   SPAN-SPAN:UNMAKE nip ;

: CELL-LEN ( span<cell> -- n )
   LEN CELL-BYTES / ;

: $ ( span<u8> -- ptr u8 n )
   SPAN-SPAN:UNMAKE ;

: BYTES ( span<cell> -- span<u8> )
   SPAN-SPAN:UNMAKE {: b l :}
   b byte-view l MAKE ;

\ Generic: the return stack keeps the scalar out of the way with no frame, and the
\ bundle is unmade where it stands.
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

\ No generic element-indexed accessor: `+` on a `ptr t` is byte arithmetic and
\ checked code has no element size for a type parameter. The byte set works over
\ `span<u8>`, the cell set over `span<cell>`, and SKIP/TAKE/SUB narrow in bytes.
\ There is no TYPED-BUFFER span form: its accessor is an index function, and one
\ would change the generated accessor set (src/core/layout-buffer.f); hand a
\ TYPED-BUFFER accessor result to SPAN:MAKE instead.
: AT ( span<u8> n -- ptr u8 ) {: s i :}
   s SPAN-SPAN:UNMAKE {: b l :}
   i 0 < i l >= or if E-SPAN-RANGE throw then
   b i + ;

: U8@ ( span<u8> n -- u8 )
   AT c@ ;

: U8! ( u8 span<u8> n -- ) {: v s i :}
   v s i AT c! ;

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

\ All of it or none of it.
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
