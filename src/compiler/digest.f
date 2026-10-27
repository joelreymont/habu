\ digest.f - SHA-256 values and little-endian slots for compiler data.
\
\ Compiler arenas use eight-byte little-endian slots; callers may also hash
\ byte spans directly for build identities.
\
\ Not reentrant. The engine's SHA-256 state (src/core/sha256.f) is a single
\ process-wide context, and this module keeps one output buffer, so a digest must
\ be computed on one thread at a time. That is a pre-existing property of every
\ SHA-256 caller in the tree (src/lib/content-key.f and maki/target/target.f), not
\ something this module introduces; a multi-threaded compiler needs a
\ reentrant hash context first.

require src/lib/errors.f
require src/compiler/workspace.f

package CDIGEST
public

\ A 256-bit digest held as the four 64-bit little-endian words of the SHA-256
\ output, deepest word first. Multi-cell, so no single-cell scalar can launder
\ into it. DERIVE eq gives the typed identity compare CDIGEST-DIGEST:EQ.
STRUCTURE digest DERIVE eq
   FIELD w0 n
   FIELD w1 n
   FIELD w2 n
   FIELD w3 n
;STRUCTURE

\ One slot is one 64-bit little-endian cell.
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

\ Write one 64-bit slot, little-endian.
: SLOT! ( n ptr u8 n -- )
   {: v:n base:ptr slot:n :}
   base slot SLOT-BYTES * + {: at:ptr :}
   at NATIVE-SLOT? if v at CELL-VIEW ! exit then
   SLOT-BYTES 0 ?do
      v i 8 * rshift $FF and  at i + c!
   loop ;

\ Read one slot back, the inverse of SLOT!.
: SLOT@ ( ptr u8 n -- n )
   {: base:ptr slot:n :}
   base slot SLOT-BYTES * + {: at:ptr :}
   at NATIVE-SLOT? if at CELL-VIEW @ exit then
   0
   SLOT-BYTES 0 ?do
      at i + c@  i 8 * lshift  or
   loop ;

\ ---- whole runs of slots -----------------------------------------------------
\ A RUN'S ALIGNMENT IS ONE QUESTION, NOT ONE PER SLOT. SLOT! and SLOT@ each ask
\ NATIVE-SLOT? about their own address: a call, a subtraction of NULL-PTR and
\ the eight-byte test. A caller copying or clearing a run would pay that test per
\ cell, and the run's addresses advance by SLOT-BYTES, so a run whose first slot
\ is a native cell has every slot a native cell. These two words ask once and
\ then move cells, and fall back to the two words above - one slot at a time,
\ byte by byte - on a host where a canonical slot is not a native cell.
\
\ They are the canonical slot words for a span and nothing more: same order,
\ same bytes, same result as the loop each replaces. The IR arena's span growth
\ and bulk append and the symbol interner's bucket tables are the callers, and
\ between them they are what a module's creation spends most of its time on.

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

\ SHA-256 over exactly the caller's preimage bytes, read out as four words.
: COMPUTE ( ptr u8 n -- CDIGEST:digest ) {: a u:n :}
   SHA-CTX a u DG SHA256-IN
   DG 0 SLOT@  DG 1 SLOT@  DG 2 SLOT@  DG 3 SLOT@
   CDIGEST-DIGEST:MAKE ;

;package
