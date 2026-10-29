\ digest.f - SHA-256 values and little-endian eight-byte slots for compiler data.
\
\ Not reentrant: the engine's SHA-256 state (src/core/sha256.f) is one
\ process-wide context and this package keeps one output buffer, so digests run
\ on one thread at a time.

require src/lib/errors.f
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

\ The host memory order is independent of the compilation target. An aligned
\ native cell is a canonical slot only on an eight-byte little-endian host.
create HOST-ORDER 1 ,
HOST-ORDER c@ 1 = CELL SLOT-BYTES = and constant NATIVE-SLOTS?

: NATIVE-SLOT? ( ptr u8 -- bool )
   NULL-PTR - SLOT-BYTES 1- and 0= NATIVE-SLOTS? and ;

public

: SLOT! ( n ptr u8 n -- )
   {: v:n base:ptr slot:n :}
   base slot SLOT-BYTES * + {: at:ptr :}
   at NATIVE-SLOT? if v at CELL-VIEW ! exit then
   SLOT-BYTES 0 ?do
      v i 8 * rshift $FF and  at i + c!
   loop ;

: SLOT@ ( ptr u8 n -- n )
   {: base:ptr slot:n :}
   base slot SLOT-BYTES * + {: at:ptr :}
   at NATIVE-SLOT? if at CELL-VIEW @ exit then
   0
   SLOT-BYTES 0 ?do
      at i + c@  i 8 * lshift  or
   loop ;

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
