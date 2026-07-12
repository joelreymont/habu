\ src-shape.f - shared source-shape assertions for emitter regression tests.
\
\ One owner for "read a whole source file into a right-sized buffer, then assert
\ its shape". The four emitter-shape tests (c-call, signature-scan,
\ compiler-dispatch, spawn) used to each carry their own CAP/BUF/READ-ALL plus
\ HAS?/COUNT/MUST-HAVE machinery with DIVERGENT capacities; one stale $20000 cap
\ silently under-sized when habu2.f grew and overflowed with a bare E-FS-CAPACITY.
\ This helper removes the divergence: LOAD auto-sizes an OS-backed buffer to the
\ file (FILE-SIZE -> MEM-ALLOC-64K-SPAN, the proven idiom), so a test can never
\ under-size again, and CHECK-FIT fails closed with a diagnostic naming the file
\ and sizes if a source ever exceeds the generous ceiling.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/test.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/test.f

package SHAPE

$400000 constant SHAPE-MAX-BYTES   \ generous ceiling (~28x habu2.f); above it, fail closed

variable SHAPE-BUF-A               \ holds the OS-backed buffer pointer (via ptr-field)
variable SHAPE-CAP                 \ allocated buffer capacity, bytes
variable SHAPE-LEN                 \ length of the currently loaded source

: BUF-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: BUF@ ( -- ptr u8 )
   SHAPE-BUF-A BUF-FIELD @ ;

: BUF! ( ptr u8 -- )
   SHAPE-BUF-A BUF-FIELD ! ;

: ALLOC-NEED ( n -- n )
   dup 0 <= if drop 1 then ;

: ALLOC ( n -- )
   ALLOC-NEED MEM-ALLOC-64K-SPAN SHAPE-CAP ! BUF! ;

: OVERSIZE ( ptr u8 n n n -- ) {: path:ptr pathu:n size:n cap:n :}
   s" src-shape: " type path pathu type
   s"  exceeds buffer cap; bytes=" type size .   \ . prints the number then a newline
   s" src-shape: cap=" type cap .
   E-FS-CAPACITY throw ;

: COUNT-BYTES ( ptr u8 n ptr u8 n -- n ) {: hay:ptr hayu:n needle:ptr needleu:n :}
   needleu 0= if 0 exit then
   hayu needleu < if 0 exit then
   0 0 begin dup hayu needleu - <= while
      hay over + needleu needle needleu STR= if swap 1+ swap then
      1+
   repeat drop ;

public

: CHECK-FIT ( ptr u8 n n n -- ) {: path:ptr pathu:n size:n cap:n :}
   size cap > if path pathu size cap OVERSIZE then ;

: LOAD ( ptr u8 n -- )
   2dup FILE-SIZE {: path:ptr pathu:n size:n :}
   path pathu size SHAPE-MAX-BYTES CHECK-FIT
   size ALLOC
   path pathu BUF@ SHAPE-CAP @ READ-ALL SHAPE-LEN ! ;

: TEXT ( -- ptr u8 n )
   BUF@ SHAPE-LEN @ ;

: HAS? ( ptr u8 n -- bool )
   TEXT 2swap CONTAINS? ;

: COUNT ( ptr u8 n -- n )
   TEXT 2swap COUNT-BYTES ;

: MUST-HAVE ( ptr u8 n -- )
   HAS? TTRUE ;

: MUST-LACK ( ptr u8 n -- )
   HAS? 0= TTRUE ;

: COUNT= ( ptr u8 n n -- ) {: needle:ptr needleu:n want:n :}
   needle needleu COUNT want T= ;

;package
