\ intern-table.f - the build space's string table, package INTERN's first part.
\ Loaded before checker.f by each core prefix so the checker can reach it;
\ src/habu/intern.f reopens INTERN.

package INTERN
private

\ error.f owns E-INTERN-REWIND but loads later: the same (code, name) is
\ registered here, as error-code-lint admits; test/linker.f CASE-F checks it.
-8638 constant E-INTERN-REWIND

\ src/habu/layout.f DPHW-CELL, which loads after this file.
$2D58 constant DPHW-OFF

variable HV

\ FNV-1a; the top bit can run into the sign.
: FNV ( ptr u8 n -- n )
   {: a:ptr u:n :}
   $811C9DC5 HV !
   u 0 ?do
      a i + c@ HV @ xor  $1000193 *  HV !
   loop
   HV @ ;

\ ---- a build's data space ------------------------------------------------------
\ Open while BOOT:IMAGE-BASE-CELL is nonzero (boot-cells.f says when it is 0); a
\ task's region starts at 0. The table grows, so it is mapped outside DATA; the
\ space's close frees it (CLEANUP).
variable SPACE-IMAGE     \ the image base the table serves, 0 for no table
variable SPACE-TABLE     \ the slots' address, as its distance from NULL-PTR
variable SPACE-SLOTS     \ a power of two
variable SPACE-FILLED
1024 constant FIRST-SLOTS

: IMAGE-BASE ( -- n ) data-base BOOT:IMAGE-BASE-CELL + @ ;
: SPACE? ( -- bool ) IMAGE-BASE 0 <> ;

: CELL-AT ( n n -- ptr n )
   {: base:n k:n :}
   NULL-PTR BYTE-VIEW base + k cells + CELL-VIEW ;

: SLOT-CELL ( n -- ptr n ) SPACE-TABLE @ swap CELL-AT ;

\ A slot holds a body's address, 0 when empty; the length is the cell before it.
: BODY$ ( n -- ptr u8 n )
   {: body:n :}
   NULL-PTR BYTE-VIEW body +
   body CELL - 0 CELL-AT @ ;

: SLOT-ENDS? ( ptr u8 n n -- bool )
   {: a:ptr u:n k:n :}
   k SLOT-CELL @ {: body:n :}
   body 0= if 0 0= exit then
   body BODY$ a u CORE-STR= ;

\ The slot holding the body equal to a u, else the empty slot it goes in: one
\ exists, since the table is never more than half full (SPACE-ADD).
: SPACE-SLOT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u FNV SPACE-SLOTS @ 1- and
   begin dup a u rot SLOT-ENDS? 0= while 1+ SPACE-SLOTS @ 1- and repeat ;

: SLOT-PUT ( n -- )
   {: body:n :}
   body  body BODY$ SPACE-SLOT SLOT-CELL ! ;

: TABLE-MAP ( n -- n )
   cells map-anon 0 <> if drop s" intern: table mmap failed" 76 die then
   NULL-PTR - ;

: TABLE-RELEASE ( n n -- )
   {: base:n slots:n :}
   base 0 CELL-AT slots cells munmap 0 <> if
      s" intern: table munmap failed" 76 die
   then ;

public

\ Frees the table of the build whose space is closing (LINKER:WITH's close).
: CLEANUP ( -- )
   SPACE-IMAGE @ 0= if exit then
   SPACE-TABLE @ SPACE-SLOTS @ TABLE-RELEASE
   0 SPACE-TABLE !  0 SPACE-IMAGE ! ;

\ Hands visit each body the build's table holds, as its distance from NULL-PTR. Bodies go
\ stale only once DP moves back, and the build then writes nothing (linker.f GOING).
: EACH-STRING ( [ n -- ] -- )
   {: visit :}
   SPACE-IMAGE @ 0=  SPACE-IMAGE @ IMAGE-BASE <>  or if exit then
   SPACE-SLOTS @ 0 ?do
      i SLOT-CELL @ {: body:n :}
      body 0 <> if body visit execute then
   loop ;

private

: SPACE-OPEN ( -- )
   CLEANUP
   FIRST-SLOTS TABLE-MAP SPACE-TABLE !
   FIRST-SLOTS SPACE-SLOTS !  0 SPACE-FILLED !
   IMAGE-BASE SPACE-IMAGE ! ;

: SPACE-GROW ( -- )
   SPACE-TABLE @ {: old:n :}
   SPACE-SLOTS @ {: slots:n :}
   slots 2 * TABLE-MAP SPACE-TABLE !
   slots 2 * SPACE-SLOTS !
   slots 0 ?do
      old i CELL-AT @ dup 0 <> if SLOT-PUT else drop then
   loop
   old slots TABLE-RELEASE ;

\ One `allot` checks the bounds and zeroes before the copy; no `,`, so the
\ length cell never joins a created body's cell grid (BCOMMA).
: SPACE-ADD ( ptr u8 n -- n )
   {: a:ptr u:n :}
   SPACE-FILLED @ 1+ 2 * SPACE-SLOTS @ > if SPACE-GROW then
   align here NULL-PTR - {: head:n :}
   CELL u + allot
   u head 0 CELL-AT !
   head CELL + {: address:n :}
   NULL-PTR BYTE-VIEW address + {: dst:ptr :}
   u 0 ?do a i + c@ dst i + c! loop
   address SLOT-PUT
   1 SPACE-FILLED +!
   address ;

\ The refusal comes before any lookup: a found address may sit above a lowered
\ DP.
: SPACE-STRING ( ptr u8 n -- n )
   {: a:ptr u:n :}
   data-base DPHW-OFF + @ 0 <> if E-INTERN-REWIND throw then
   SPACE-IMAGE @ IMAGE-BASE <> if SPACE-OPEN then
   a u SPACE-SLOT SLOT-CELL @ {: body:n :}
   body 0 <> if body exit then
   a u SPACE-ADD ;

;package
