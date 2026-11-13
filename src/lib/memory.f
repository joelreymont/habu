\ memory.f - checked OS-backed mappings for runtime-sized storage; `create ...
\ allot` is for dictionary-sized static storage. A refused mmap throws
\ E-MEM-MAP; a kernel munmap refusal writes "memory: unmap failed" and exits 71,
\ bypassing catch. STORAGE CLASS. CALLER-OWNED: the caller releases every
\ mapping. WITH-BYTES's scopes are TASK-LOCAL: their chunks hang off one head
\ cell in the MEM-ABI band (src/habu/layout.f), a halted task unwinds its scopes
\ at TASK:PAUSE, and MEM:RELEASE-SCOPES unmaps a freed region's chunks.

s" src/lib/error.f" required
require src/lib/num-arithmetic.f
require src/lib/span.f

$10000 constant MEM-64K
$7FFFFFFFFFFFFFFF constant MEM-MAX-N
MEM-MAX-N MEM-64K / constant MEM-MAX-64K-BUFFERS
1 cells constant MEM-CELL-BYTES
MEM-MAX-N MEM-CELL-BYTES / constant MEM-MAX-CELLS

0 constant MEM-ADDR-ANY
0 constant MEM-PROT-NONE
3 constant MEM-PROT-RW
1 constant MEM-MAP-SHARED
$1002 constant MEM-MAP-PRIVATE-ANON
$1012 constant MEM-MAP-PRIVATE-ANON-FIXED   \ the engine's mmap treats $10 as MAP_FIXED on every target
-1 constant MEM-ANON-FD
0 constant MEM-OFF-ZERO

: MEM-64K-BYTES ( n -- n )
   MEM-64K * ;

: MEM-CELLS>BYTES ( count -- n )
   COUNT>N cells ;

: MEM-64K-COUNT-FOR ( n -- n )
   1 - MEM-64K / 1 + ;

: MEM-64K-SPAN-BYTES ( n -- n )
   MEM-64K-COUNT-FOR MEM-64K-BYTES ;

\ Fresh storage gets its element type from the caller. The engine owns only
\ the OS mapping and its pointer/error result.
: MEM-ALLOC-PTR ( n -- ptr a )
   map-anon 0<> if drop E-MEM-MAP throw then ;

: MEM-ALLOC-BYTES ( n -- ptr u8 n ) {: bytes :}
   bytes MEM-ALLOC-PTR bytes ;

: MEM-ALLOC-CELLS ( count -- ptr a )
   MEM-CELLS>BYTES MEM-ALLOC-PTR ;

: MEM-ALLOC-64K-BUFFERS ( n -- ptr u8 n )
   MEM-64K-BYTES MEM-ALLOC-BYTES ;

: MEM-ALLOC-64K-SPAN ( n -- ptr u8 n )
   MEM-64K-SPAN-BYTES MEM-ALLOC-BYTES ;

: MEM-ALLOC-64K ( -- ptr u8 n )
   1 MEM-ALLOC-64K-BUFFERS ;

\ Package MEM states the raw MEM-ALLOC-* sizing as checked words over NUM roles:
\ the scalar words answer numeric-result<a> (zero is a valid answer), and the
\ allocation sinks take only alloc-* roles, so a role swap never reaches mmap.

package MEM
private

\ The OK-* extractors take compile-time-valid role constants: their refusal
\ arms are unreachable and throw E-MEM-TOTALITY, as NUM's E-NUM-TOTALITY.
: OK-BYTE-LEN ( NUM:numeric-result<NUM:byte-len> -- NUM:byte-len )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-ALIGNMENT ( NUM:numeric-result<NUM:alignment> -- NUM:alignment )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;
: OK-ALLOC-BYTE-LEN ( NUM:numeric-result<NUM:alloc-byte-len> -- NUM:alloc-byte-len )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-MEM-TOTALITY throw ENDOF
      zero OF E-MEM-TOTALITY throw ENDOF        overflow OF E-MEM-TOTALITY throw ENDOF
      underflow OF E-MEM-TOTALITY throw ENDOF   bad-alignment OF E-MEM-TOTALITY throw ENDOF
      misaligned OF E-MEM-TOTALITY throw ENDOF
   ;MATCH ;

\ MEM-64K is a compile-time positive power of two, so BYTE-LEN / ALIGNMENT /
\ AS-ALLOC-BYTE-LEN all succeed; the extractors' failure arms are unreachable.
: 64K-LEN ( -- NUM:byte-len )
   MEM-64K NUM:BYTE-LEN OK-BYTE-LEN ;
: 64K-ALIGN ( -- NUM:alignment )
   MEM-64K NUM:ALIGNMENT OK-ALIGNMENT ;
: 64K-ALLOC-LEN ( -- NUM:alloc-byte-len )
   64K-LEN NUM:AS-ALLOC-BYTE-LEN OK-ALLOC-BYTE-LEN ;

\ Private allocation-role erasure for mmap, cells and munmap; no raw value
\ escapes.
CAST: ALLOC-BYTES>N ( NUM:alloc-byte-len -- n )
CAST: ALLOC-CELLS>N ( NUM:alloc-cell-count -- n )
CAST: BYTE-LEN>N ( NUM:byte-len -- n )

$47 constant UNMAP-EXIT

: RELEASE-RANGE ( ptr a n -- )
   munmap 0 < if
      s" memory: unmap failed" UNMAP-EXIT die
   then ;

public

: CELLS>BYTES ( NUM:cell-count -- NUM:numeric-result<NUM:byte-len> )
   NUM:CELLS>BYTES ;
: 64K-BYTES ( NUM:item-count -- NUM:numeric-result<NUM:byte-len> )
   64K-LEN swap NUM:SCALE-BYTES ;
: 64K-SPAN-BYTES ( NUM:byte-len -- NUM:numeric-result<NUM:byte-len> )
   64K-ALIGN NUM:ALIGN-UP-BYTES ;
: 64K-COUNT-FOR ( NUM:byte-len -- NUM:numeric-result<NUM:item-count> )
   \ 64K is a positive extent, so DIV-BYTES-CEIL's zero-size-unit arm is
   \ unreachable; a zero need is 0 buffers.
   64K-LEN NUM:DIV-BYTES-CEIL ;

: ALLOC-BYTES ( NUM:alloc-byte-len -- ptr u8 NUM:alloc-byte-len )
   dup ALLOC-BYTES>N MEM-ALLOC-PTR
   swap ;
: ALLOC-CELLS ( NUM:alloc-cell-count -- ptr a )
   ALLOC-CELLS>N cells MEM-ALLOC-PTR ;
\ Takes the count ALLOC-CELLS took.
: RELEASE-CELLS ( ptr a NUM:alloc-cell-count -- )
   ALLOC-CELLS>N cells RELEASE-RANGE ;
: ALLOC-64K ( -- ptr u8 NUM:alloc-byte-len )
   64K-ALLOC-LEN ALLOC-BYTES ;

\ Takes the exact extent ALLOC-BYTES gave.
: RELEASE-BYTES ( ptr u8 NUM:alloc-byte-len -- )
   ALLOC-BYTES>N RELEASE-RANGE ;

: UNMAP ( ptr u8 NUM:byte-len -- )
   BYTE-LEN>N RELEASE-RANGE ;

\ Callers name a raw size as the positive alloc role before the allocation
\ sink.
CAST: BYTES-ALLOC-LEN ( n -- NUM:alloc-byte-len )
CAST: CELLS-ALLOC-COUNT ( n -- NUM:alloc-cell-count )

\ Here and not in src/lib/span.f, because this file owns MEM
\ (docs/forth-card.md § 6); that makes src/lib/span.f a boot-prefix row ahead of
\ this one. The private projection is proof erasure - SPAN:MAKE takes the reach
\ as a raw byte count - and has no public inverse.
private

CAST: SPAN-ALLOC-LEN>N ( NUM:alloc-byte-len -- n )

public

: ALLOCATION>SPAN ( ptr u8 NUM:alloc-byte-len -- SPAN:span<u8> )
   SPAN-ALLOC-LEN>N SPAN:MAKE ;

: ALLOC-SPAN ( NUM:alloc-byte-len -- SPAN:span<u8> )
   ALLOC-BYTES ALLOCATION>SPAN ;

: FREE-SPAN ( SPAN:span<u8> -- )
   SPAN:$ BYTES-ALLOC-LEN RELEASE-BYTES ;

private

\ A scope's header; the body follows FRAME-BYTES in, 16-aligned. extent is the
\ pooled slot (<= POOL-MAX) or the mapping, and WB-LEAVE routes on it.
STRUCTURE frame DERIVE addr
   FIELD prev ptr u8
   FIELD extent n
;STRUCTURE

\ Only the first chunk's inner and cur are read: the innermost frame and the chunk of the
\ innermost pooled slot. CHUNK-BYTES is a multiple of 16, so slot bodies stay 16-aligned.
STRUCTURE chunk DERIVE addr
   FIELD prev ptr u8
   FIELD next ptr u8
   FIELD extent n
   FIELD top n
   FIELD inner ptr u8
   FIELD cur ptr u8
;STRUCTURE

\ The largest pooled slot, a 1024-byte body's: SPAN:FILL zeroes a slot byte by
\ byte, so on Linux arm64 a 2048-byte body costs as much pooled as mapped. A
\ multiple of 16, so WB-LEAVE's test matches WB-ENTER's, and below MEM-64K + 16,
\ the extent of a MEM-64K body memory-test.f maps.
1040 constant POOL-MAX
\ The first chunk's extent: the fewest whole pages holding its header and a POOL-MAX slot.
POOL-MAX CHUNK-BYTES + STACK-ABI:PAGE-BYTES 1 - +
   STACK-ABI:PAGE-BYTES negate and constant FIRST-EXTENT

CAST: >HEAD ( ptr u8 -- ptr ptr chunk )
CAST: FRAME>BYTES ( ptr frame -- ptr u8 )
CAST: >FRAME ( ptr u8 -- ptr frame )
CAST: CHUNK>BYTES ( ptr chunk -- ptr u8 )
CAST: >CHUNK ( ptr u8 -- ptr chunk )

\ The head cell of the DATA region at `base`: its first chunk, null until its
\ first scope.
: REGION-HEAD ( ptr u8 -- ptr ptr chunk )
   MEM-ABI:HEAD-OFF + >HEAD ;

: HEAD ( -- ptr ptr chunk )
   data-base BYTE-VIEW REGION-HEAD ;

: NEW-FRAME ( n -- ptr frame )
   MEM-ALLOC-PTR >FRAME ;

: FRAME>BODY ( ptr frame -- ptr u8 )
   FRAME>BYTES FRAME-BYTES + ;

\ An empty chunk of `extent` bytes after `prev`; a fresh mapping reads 0, so
\ its next, inner and cur are null.
: NEW-CHUNK ( n ptr u8 -- ptr chunk )
   {: extent:n prev:ptr :}
   extent MEM-ALLOC-PTR >CHUNK {: c :}
   prev c CHUNK-PREV !
   extent c CHUNK-EXTENT !
   CHUNK-BYTES c CHUNK-TOP !
   c ;

: FIRST-CHUNK ( -- ptr chunk )
   HEAD @ {: first :}
   first NULL-PTR = 0= if first exit then
   FIRST-EXTENT NULL-PTR NEW-CHUNK {: fresh :}
   fresh CHUNK>BYTES fresh CHUNK-CUR !
   fresh HEAD !
   fresh ;

\ The chunk after `cur`: the kept one, empty under LIFO, else a new one twice
\ `cur`'s extent.
: NEXT-CHUNK ( ptr chunk -- ptr chunk )
   {: cur :}
   cur CHUNK-NEXT @ >CHUNK {: next :}
   next NULL-PTR = 0= if next exit then
   cur CHUNK-EXTENT @ 2 * cur CHUNK>BYTES NEW-CHUNK {: fresh :}
   fresh CHUNK>BYTES cur CHUNK-NEXT !
   fresh ;

\ Takes the next `slot` bytes in the chunks of the region whose first chunk is
\ `first`.
: SLOT ( ptr chunk n -- ptr frame )
   {: first slot:n :}
   first CHUNK-CUR @ >CHUNK
   dup CHUNK-TOP @ slot + over CHUNK-EXTENT @ > if NEXT-CHUNK then {: cur :}
   cur CHUNK>BYTES first CHUNK-CUR !
   cur CHUNK-TOP @ {: top:n :}
   top slot + cur CHUNK-TOP !
   cur CHUNK>BYTES top + >FRAME ;

\ The extent for a body of `len` bytes.
: SCOPE-EXTENT ( NUM:alloc-byte-len -- n )
   ALLOC-BYTES>N FRAME-BYTES + ;

\ A scope links only once its storage is taken, so a refused mapping links
\ nothing.
: PUSH-FRAME ( ptr frame n ptr chunk -- )
   {: extent:n first :}
   first CHUNK-INNER @ over FRAME-PREV !
   extent over FRAME-EXTENT !
   FRAME>BYTES first CHUNK-INNER ! ;

: WB-ENTER ( NUM:alloc-byte-len -- ptr u8 NUM:alloc-byte-len )
   dup SCOPE-EXTENT {: extent:n :}
   FIRST-CHUNK {: first :}
   extent POOL-MAX > if
      extent NEW-FRAME extent
   else
      extent 15 + 16 negate and {: slot:n :}
      first slot SLOT
      0 over FRAME>BODY slot FRAME-BYTES - SPAN:MAKE SPAN:FILL
      slot
   then
   over swap first PUSH-FRAME
   FRAME>BODY swap ;

\ Inner scopes leave first, also under a throw; emptying a chunk after the first
\ steps cur back, and the release unlinks before it unmaps.
: WB-LEAVE ( -- )
   HEAD @ {: first :}
   first CHUNK-INNER @ >FRAME {: frame :}
   frame FRAME-PREV @ first CHUNK-INNER !
   frame FRAME-EXTENT @ {: extent:n :}
   extent POOL-MAX > if frame FRAME>BYTES extent RELEASE-RANGE exit then
   first CHUNK-CUR @ >CHUNK {: cur :}
   frame FRAME>BYTES cur CHUNK>BYTES - cur CHUNK-TOP !
   cur CHUNK-TOP @ CHUNK-BYTES = cur CHUNK-PREV @ NULL-PTR = 0= and if
      cur CHUNK-PREV @ first CHUNK-CUR !
   then ;

: WB-BODY ( R ptr u8 NUM:alloc-byte-len [ R ptr u8 NUM:alloc-byte-len -- S ] -- S )
   execute ;

public

: WITH-BYTES ( R NUM:alloc-byte-len [ R ptr u8 NUM:alloc-byte-len -- S ] -- S )
   {: body :}
   WB-ENTER body [: WB-BODY ;] [: WB-LEAVE ;] finally ;

\ Unmaps the scope chunks of the DATA region at `base`, which has no scope
\ open, after clearing its head cell.
: RELEASE-SCOPES ( ptr u8 -- )
   REGION-HEAD {: head :}
   head @ CHUNK>BYTES
   NULL-PTR head !
   begin dup NULL-PTR = 0= while
      >CHUNK dup CHUNK-NEXT @ swap
      dup CHUNK>BYTES swap CHUNK-EXTENT @ RELEASE-RANGE
   repeat
   drop ;

private

\ A stack the engine may run on (run-in-stack, a task's data, return and loop
\ stacks) is a mapping with an inaccessible page on each side: a push past its
\ capacity or a read below its base faults, and the engine's crash handler
\ names the stack (src/habu/crash.f). This is the only way to make such a
\ stack: run-in-stack refuses any other extent (E-STACK-UNGUARDED), because a
\ heap buffer has nothing beyond it to stop an overflow. The layout is the one
\ src/habu/rt.f STACK-GUARD:EMIT-MAP gives the boot stacks: the whole span is
\ mapped inaccessible first, then the capacity is remapped read/write at a
\ STACK-ABI:PAGE-BYTES boundary inside it, so the pages on both sides stay
\ inaccessible whatever granule the kernel returned.

\ The address a raw mmap returned, as the byte pointer of the mapping the
\ word below just made; the only place in this file a bare address becomes a pointer.
CAST: MAPPED>PTR ( n -- ptr u8 )

: GUARDED-SPAN-BYTES ( n -- n ) {: cap:n :}
   cap STACK-ABI:PAGE-BYTES 3 * + ;

: GUARDED-BASE ( n n -- n ) {: span:n cap:n :}
   span STACK-ABI:PAGE-BYTES 2 * + 1 - STACK-ABI:PAGE-BYTES negate and ;

: UNMAP-SLIVER ( n n -- ) {: at:n bytes:n :}
   bytes 0 = if exit then
   at MAPPED>PTR bytes munmap 0 <> if E-MEM-UNMAP throw then ;

public

\ The kernel hands back a span on its own granule, so the page-aligned base
\ lands up to one page into it. The head and tail slivers outside
\ [base - PAGE, base + cap + PAGE) are unmapped right away, so the extent that
\ stays mapped is exactly the two guard pages around the capacity and the
\ release below unmaps exactly that. A release computed from the span would
\ reach past its end into whatever the kernel mapped next (a task's DATA
\ region).
: ALLOC-GUARDED ( n -- ptr u8 n ) {: cap:n :}
   cap GUARDED-SPAN-BYTES {: span-bytes:n :}
   MEM-ADDR-ANY span-bytes MEM-PROT-NONE MEM-MAP-PRIVATE-ANON MEM-ANON-FD MEM-OFF-ZERO mmap {: span:n :}
   span 0 < if E-MEM-MAP throw then
   span cap GUARDED-BASE {: base:n :}
   base STACK-ABI:PAGE-BYTES - {: lo:n :}
   base cap + STACK-ABI:PAGE-BYTES + {: hi:n :}
   span lo span - UNMAP-SLIVER
   hi span span-bytes + hi - UNMAP-SLIVER
   base cap MEM-PROT-RW MEM-MAP-PRIVATE-ANON-FIXED MEM-ANON-FD MEM-OFF-ZERO mmap base <> if
      \ The reserve still owns [lo, hi) when the fixed writable remap fails.
      \ Report a refusing cleanup rather than losing that mapping too.
      lo cap STACK-ABI:PAGE-BYTES 2 * + UNMAP-SLIVER
      E-MEM-MAP throw
   then
   base MAPPED>PTR cap ;

\ Release a guarded stack: exactly the extent ALLOC-GUARDED kept, the
\ capacity and its two guard pages.
: RELEASE-GUARDED ( ptr u8 n -- ) {: base:ptr cap:n :}
   base STACK-ABI:PAGE-BYTES - cap STACK-ABI:PAGE-BYTES 2 * + munmap 0 <> if E-MEM-UNMAP throw then ;
;package
