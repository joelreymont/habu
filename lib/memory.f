\ memory.f - checked OS-backed byte buffers.
\
\ Load after lib/errors.f.

s" lib/errors.f" required
s" src/core/result.f" required

$10000 constant MEM-64K
$7FFFFFFFFFFFFFFF constant MEM-MAX-N
MEM-MAX-N MEM-64K / constant MEM-MAX-64K-BUFFERS
1 cells constant MEM-CELL-BYTES
MEM-MAX-N MEM-CELL-BYTES / constant MEM-MAX-CELLS

0 constant MEM-ADDR-ANY
3 constant MEM-PROT-RW
1 constant MEM-MAP-SHARED
$1002 constant MEM-MAP-PRIVATE-ANON
-1 constant MEM-ANON-FD
0 constant MEM-OFF-ZERO

: MEM-CHECK-SIZE ( n -- )
   dup 0 <= if E-MEM-SIZE throw then
   drop ;

: MEM-CHECK-64K-COUNT ( n -- )
   dup 0 <= if E-MEM-SIZE throw then
   dup MEM-MAX-64K-BUFFERS > if E-MEM-SIZE throw then
   drop ;

: MEM-CHECK-CELL-COUNT ( count -- )
   dup COUNT>N 0 <= if E-MEM-SIZE throw then
   dup COUNT>N MEM-MAX-CELLS > if E-MEM-SIZE throw then
   drop ;

: MEM-64K-BYTES ( n -- n ) {: cnt :}
   cnt MEM-CHECK-64K-COUNT
   cnt MEM-64K * ;

: MEM-CELLS>BYTES ( count -- n )
   dup MEM-CHECK-CELL-COUNT
   COUNT>N cells ;

: MEM-64K-COUNT-FOR ( n -- n ) {: bytes :}
   bytes MEM-CHECK-SIZE
   bytes 1 - MEM-64K / 1 + dup MEM-CHECK-64K-COUNT ;

: MEM-64K-SPAN-BYTES ( n -- n )
   MEM-64K-COUNT-FOR MEM-64K-BYTES ;

: MEM-MMAP-RC ( n -- n ) {: bytes :}
   bytes MEM-CHECK-SIZE
   MEM-ADDR-ANY bytes MEM-PROT-RW MEM-MAP-PRIVATE-ANON MEM-ANON-FD MEM-OFF-ZERO mmap ;

: MEM-MMAP-BYTES ( n -- result<ptr u8,n> )
   MEM-MMAP-RC RESULT:MMAP>BYTES ;

: MEM-MMAP-CELLS ( count -- result<ptr a,n> )
   MEM-CELLS>BYTES MEM-MMAP-RC RESULT:MMAP>CELLS ;

: MEM-MAP-ERR ( n -- )
   drop E-MEM-MAP throw ;

: MEM-ALLOC-PTR ( n -- ptr u8 )
   MEM-MMAP-BYTES
   [: ;] [: MEM-MAP-ERR ;] RESULT:CASE ;

: MEM-ALLOC-BYTES ( n -- ptr u8 n ) {: bytes :}
   bytes MEM-CHECK-SIZE
   bytes MEM-ALLOC-PTR bytes ;

: MEM-ALLOC-CELLS ( count -- ptr a )
   MEM-MMAP-CELLS
   [: ;] [: MEM-MAP-ERR ;] RESULT:CASE ;

: MEM-ALLOC-64K-BUFFERS ( n -- ptr u8 n )
   MEM-64K-BYTES MEM-ALLOC-BYTES ;

: MEM-ALLOC-64K-SPAN ( n -- ptr u8 n )
   MEM-64K-SPAN-BYTES MEM-ALLOC-BYTES ;

: MEM-ALLOC-64K ( -- ptr u8 n )
   1 MEM-ALLOC-64K-BUFFERS ;
