\ memory.f - checked OS-backed byte buffers.
\
\ Load after lib/errors.f.

s" lib/errors.f" required
require lib/cad-num-arithmetic.f

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

TRUSTED: MEM-ALLOC-PTR ( n -- ptr u8 )
   MEM-MMAP-RC dup 0 < if E-MEM-MAP throw then ;

: MEM-ALLOC-BYTES ( n -- ptr u8 n ) {: bytes :}
   bytes MEM-CHECK-SIZE
   bytes MEM-ALLOC-PTR bytes ;

: MEM-ALLOC-CELLS ( count -- ptr a )
   MEM-CELLS>BYTES MEM-ALLOC-PTR ;

: MEM-ALLOC-64K-BUFFERS ( n -- ptr u8 n )
   MEM-64K-BYTES MEM-ALLOC-BYTES ;

: MEM-ALLOC-64K-SPAN ( n -- ptr u8 n )
   MEM-64K-SPAN-BYTES MEM-ALLOC-BYTES ;

: MEM-ALLOC-64K ( -- ptr u8 n )
   1 MEM-ALLOC-64K-BUFFERS ;

\ ---- B5 package-first typed allocation surface (MODEL-CAD-V2-PLAN.md B5.5) -----
\
\ The raw MEM-ALLOC-* words above enforce positivity at RUNTIME on interchangeable
\ `n`. Package MEM re-states the same sizing as CHECKED words over CAD-NUM roles:
\ the scalar words (CELLS>BYTES, 64K-BYTES, 64K-COUNT-FOR, 64K-SPAN-BYTES) are
\ typed compositions of the closed B5.2 algebra that return `numeric-result<a>`
\ (zero is a valid scalar answer), while the allocation sinks (ALLOC-BYTES,
\ ALLOC-CELLS, ALLOC-64K) accept only the `alloc-*` roles, which reject zero and
\ over-allocation at VALIDATION - so a byte/cell role swap or a zero/overflow
\ allocation cannot reach `mmap`. MEM owns exactly two audited representation
\ projections (ALLOC-BYTES>N, ALLOC-CELLS>N): the checked algebra never reads a
\ role's raw cell, so these are the ONLY unchecked words in the package. They read
\ an `alloc-*` cell where the raw allocation primitive still consumes a bare `n`;
\ retire them when `mmap`/`cells` accept the nominal allocation role directly.
\
\ The legacy MEM-ALLOC-BYTES surface stays untouched for its four caller waves;
\ MEM-ALLOC-CELLS and the multi-64K conveniences are out of this B5 wave.

package MEM
private

\ Internal invariant code (never reachable): a validator/narrowing arm proven
\ impossible by the input still needs an exhaustive MATCH arm. Mirrors the
\ CAD-NUM E-CADNUM-TOTALITY discipline; lives in-file, not lib/errors.f.

\ ---- ok extractors for compile-time-valid role constants (arms unreachable) ----
: OK-BYTE-LEN ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-ALIGNMENT ( CAD-NUM:numeric-result<CAD-NUM:alignment> -- CAD-NUM:alignment )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-ALLOC-BYTE-LEN ( CAD-NUM:numeric-result<CAD-NUM:alloc-byte-len> -- CAD-NUM:alloc-byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;

\ ---- size-refusal extractors for caller-supplied n (arms REACHABLE) ------------
\ Unlike the OK-* extractors above (compile-time constants; refusal arms are
\ unreachable invariants -> E-MEM-TOTALITY), these narrow an arbitrary runtime
\ `n`, so a refusal is the real memory-sizing outcome and throws E-MEM-SIZE.
: SIZE-BYTE-LEN ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;
: SIZE-ALLOC-BYTE-LEN ( CAD-NUM:numeric-result<CAD-NUM:alloc-byte-len> -- CAD-NUM:alloc-byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;
: SIZE-CELL-COUNT ( CAD-NUM:numeric-result<CAD-NUM:cell-count> -- CAD-NUM:cell-count )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;
: SIZE-ALLOC-CELL-COUNT ( CAD-NUM:numeric-result<CAD-NUM:alloc-cell-count> -- CAD-NUM:alloc-cell-count )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-SIZE throw ENDOF
      zero OF E-MEM-SIZE throw ENDOF            overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF       bad-alignment OF E-MEM-SIZE throw ENDOF
      misaligned OF E-MEM-SIZE throw ENDOF
   ;MATCH ;

\ ---- the 64K granularity as validated CAD-NUM roles ---------------------------
\ MEM-64K is a compile-time positive power of two, so BYTE-LEN / ALIGNMENT /
\ AS-ALLOC-BYTE-LEN all succeed; the extractors' failure arms are unreachable.
: 64K-LEN ( -- CAD-NUM:byte-len )
   MEM-64K CAD-NUM:BYTE-LEN OK-BYTE-LEN ;
: 64K-ALIGN ( -- CAD-NUM:alignment )
   MEM-64K CAD-NUM:ALIGNMENT OK-ALIGNMENT ;
: 64K-ALLOC-LEN ( -- CAD-NUM:alloc-byte-len )
   64K-LEN CAD-NUM:AS-ALLOC-BYTE-LEN OK-ALLOC-BYTE-LEN ;

\ ---- audited representation projections (the ONLY unchecked words in MEM) ------
TRUSTED: ALLOC-BYTES>N ( CAD-NUM:alloc-byte-len -- n ) ;
TRUSTED: ALLOC-CELLS>N ( CAD-NUM:alloc-cell-count -- n ) ;

public

\ ---- scalar sizing: typed compositions of the closed B5.2 algebra --------------
: CELLS>BYTES ( CAD-NUM:cell-count -- CAD-NUM:numeric-result<CAD-NUM:byte-len> )
   CAD-NUM:CELLS>BYTES ;
: 64K-BYTES ( CAD-NUM:item-count -- CAD-NUM:numeric-result<CAD-NUM:byte-len> )
   64K-LEN swap CAD-NUM:SCALE-BYTES ;
: 64K-SPAN-BYTES ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> )
   64K-ALIGN CAD-NUM:ALIGN-UP-BYTES ;
: 64K-COUNT-FOR ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:item-count> )
   \ ceil(bytes / 64K) as a logical buffer count, purely over the typed
   \ extent-division op: the byte need is the extent, 64K the unit size. A zero
   \ need is 0 buffers (0 / 64K); 64K is a positive extent, so DIV-BYTES-CEIL's
   \ zero-size-unit refusal is unreachable. No raw cell is read here.
   64K-LEN CAD-NUM:DIV-BYTES-CEIL ;

\ ---- allocation sinks: only the alloc-* roles reach the mmap primitive ---------
: ALLOC-BYTES ( CAD-NUM:alloc-byte-len -- ptr u8 CAD-NUM:alloc-byte-len )
   dup ALLOC-BYTES>N MEM-ALLOC-PTR swap ;
: ALLOC-CELLS ( CAD-NUM:alloc-cell-count -- ptr a )
   ALLOC-CELLS>N cells MEM-ALLOC-PTR ;
: ALLOC-64K ( -- ptr u8 CAD-NUM:alloc-byte-len )
   64K-ALLOC-LEN ALLOC-BYTES ;

\ ---- caller-facing size narrowing: raw n -> validated alloc role --------------
\ The fixed-capacity buffer callers (source, codesign, content-key, object-cache,
\ process-argv, process-env) narrow a raw size to the positive alloc role BEFORE
\ the allocation sink; any refusal (zero/negative/overflow) throws E-MEM-SIZE.
\ Composes the public CAD-NUM validators only, so no new unchecked boundary.
: BYTES-ALLOC-LEN ( n -- CAD-NUM:alloc-byte-len )
   CAD-NUM:BYTE-LEN SIZE-BYTE-LEN
   CAD-NUM:AS-ALLOC-BYTE-LEN SIZE-ALLOC-BYTE-LEN ;
: CELLS-ALLOC-COUNT ( n -- CAD-NUM:alloc-cell-count )
   CAD-NUM:CELL-COUNT SIZE-CELL-COUNT
   CAD-NUM:AS-ALLOC-CELL-COUNT SIZE-ALLOC-CELL-COUNT ;
;package
