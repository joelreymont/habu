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

\ LE:U64@'s test (src/lib/le.f NATIVE-SLOT?), for the runs of slots below.
create HOST-ORDER 1 ,
HOST-ORDER c@ 1 = CELL SLOT-BYTES = and constant NATIVE-SLOTS?

: NATIVE-SLOT? ( ptr u8 -- bool )
   NULL-PTR - SLOT-BYTES 1- and 0= NATIVE-SLOTS? and ;

public

: SLOT! ( n ptr u8 n -- ) SLOT-BYTES * + LE:U64! ;
: SLOT@ ( ptr u8 n -- n ) SLOT-BYTES * + LE:U64@ ;

\ ---- whole runs of slots -----------------------------------------------------
\ A run whose first slot is a native cell has every slot a native cell, so these
\ ask NATIVE-SLOT? once per run, else fall back to SLOT! and SLOT@ per slot. The
\ IR arena's span growth and bulk append and the symbol interner's bucket tables
\ call them, and those are most of a module's creation time.

: SLOTS-COPY ( ptr u8 ptr u8 n -- )
   {: src:ptr dst:ptr n:n :}
   n 0 <= if exit then
   src NATIVE-SLOT? dst NATIVE-SLOT? and if
      n 0 ?do
         src i SLOT-BYTES * + CELL-VIEW @
         dst i SLOT-BYTES * + CELL-VIEW !
      loop
      exit
   then
   n 0 ?do src i SLOT@ dst i SLOT! loop ;

: SLOTS-ZERO ( ptr u8 n -- )
   {: dst:ptr n:n :}
   n 0 <= if exit then
   dst NATIVE-SLOT? if
      n 0 ?do 0 dst i SLOT-BYTES * + CELL-VIEW ! loop
      exit
   then
   n 0 ?do 0 dst i SLOT! loop ;

: COMPUTE ( ptr u8 n -- CDIGEST:digest ) {: a u:n :}
   SHA-CTX a u DG SHA256-IN
   DG 0 SLOT@  DG 1 SLOT@  DG 2 SLOT@  DG 3 SLOT@
   CDIGEST-DIGEST:MAKE ;

;package
