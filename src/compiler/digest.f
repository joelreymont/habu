\ digest.f - SHA-256 values and little-endian eight-byte slots for compiler data.
\
\ Not reentrant: this package keeps one digest context (SHA-CTX) and one output
\ span (DG), so digests run on one thread at a time.

require src/lib/error.f
require src/lib/le.f
require src/compiler/workspace.f

package CDIGEST
public

\ The four 64-bit little-endian words of the SHA-256 output, deepest first.
\ Multi-cell, so no single-cell scalar can launder into it.
STRUCTURE digest DERIVE eq
   FIELD w0 n
   FIELD w1 n
   FIELD w2 n
   FIELD w3 n
;STRUCTURE

8 constant SLOT-BYTES

private

32 constant OUT-BYTES

OUT-BYTES CWORK-SPAN DG u8
SHA256-CTX-BYTES CWORK-SPAN SHA-CTX u8   \ this package's digest context

public

: SLOT! ( n ptr u8 n -- ) SLOT-BYTES * + LE:U64! ;
: SLOT@ ( ptr u8 n -- n ) SLOT-BYTES * + LE:U64@ ;

\ ---- whole runs of slots -----------------------------------------------------

: SLOTS-COPY ( ptr u8 ptr u8 n -- )
   {: src:ptr dst:ptr n:n :}
   n 0 <= if exit then
   n 0 ?do
      src i SLOT-BYTES * + CELL-VIEW @
      dst i SLOT-BYTES * + CELL-VIEW !
   loop ;

: SLOTS-ZERO ( ptr u8 n -- )
   {: dst:ptr n:n :}
   n 0 <= if exit then
   n 0 ?do 0 dst i SLOT-BYTES * + CELL-VIEW ! loop ;

: COMPUTE ( ptr u8 n -- CDIGEST:digest ) {: a u:n :}
   SHA-CTX a u DG SHA256-IN
   DG 0 SLOT@  DG 1 SLOT@  DG 2 SLOT@  DG 3 SLOT@
   CDIGEST-DIGEST:MAKE ;

;package
