\ generated-declaration-dictionary.f - reversible native dictionary owner.
\
\ Each generated-declaration nesting level retains the native dictionary record,
\ code, and data high-waters until the outermost transaction succeeds.  A nested
\ success therefore remains provisional: an outer rollback truncates every word,
\ package record, code byte, and data allocation created below its savepoint.
\ WIDN deliberately follows layout.f's process-wide monotonic allocation contract:
\ rollback leaves an unreferenced hole instead of reusing an identity.  Truncating
\ the dictionary records removes every lookup path to each consumed WID, while
\ monotonic non-reuse prevents stale WID-bearing state from aliasing a later list.

package GENERATED-DECL-DICTIONARY

7174 constant E-DICTIONARY-TX
7175 constant E-DICTIONARY-CAP

0 cells constant ROW.NDICT-OFF
1 cells constant ROW.CP-OFF
2 cells constant ROW.DP-OFF
3 cells constant ROW-REC

4 constant CAP-INIT
$7FFFFFFFFFFFFFFF constant FRAME-BYTE-MAX
FRAME-BYTE-MAX ROW-REC / constant FRAME-ROW-MAX
create FRAME-BOOT CAP-INIT ROW-REC * allot
PTR-VARIABLE FRAME-P FRAME-BOOT FRAME-P !
variable FRAME-CAP CAP-INIT FRAME-CAP !

: FRAME-BASE ( -- ptr a ) FRAME-P @ ;
: FRAME-ROW ( n -- ptr a )
   dup 0 < over FRAME-CAP @ >= or IF E-DICTIONARY-CAP throw THEN
   ROW-REC * FRAME-BASE + ;
: ROW.NDICT ( ptr a -- ptr a ) ROW.NDICT-OFF + ;
: ROW.CP ( ptr a -- ptr a ) ROW.CP-OFF + ;
: ROW.DP ( ptr a -- ptr ptr a ) ROW.DP-OFF CELL / ptr-field ;

TRUSTED: FRAME-GROW ( ptr a n n -- ptr a ) ARENA-BYTES-GROW ;
TRUSTED: DICTIONARY-DP! ( ptr a -- ) data-base DP-CELL + ! ;

: CHECK-DEPTH ( n -- )
   dup 0 <= IF drop E-DICTIONARY-TX throw THEN
   FRAME-ROW-MAX > IF E-DICTIONARY-CAP throw THEN ;

: ENSURE-ROOM ( n -- ) {: need:n :}
   need FRAME-CAP @ <= IF EXIT THEN
   FRAME-CAP @ 0 <= FRAME-CAP @ FRAME-ROW-MAX > or
      IF E-DICTIONARY-CAP throw THEN
   FRAME-CAP @ FRAME-ROW-MAX 2 / <=
      IF FRAME-CAP @ 2 * need max ELSE need THEN {: cap:n :}
   FRAME-P @ FRAME-CAP @ ROW-REC *
      cap ROW-REC * FRAME-GROW FRAME-P !
   cap FRAME-CAP ! ;

: SNAPSHOT ( n -- n ) {: depth:n :}
   depth CHECK-DEPTH
   depth ENSURE-ROOM
   depth 1 - FRAME-ROW {: r:ptr :}
   ndict@ r ROW.NDICT !
   cp@ r ROW.CP !
   here r ROW.DP !
   depth ;

: PREPARE ( n -- n ) ;
: COMMIT ( n -- n ) ;

: ROLLBACK ( n -- n ) {: depth:n :}
   depth CHECK-DEPTH
   depth 1 - FRAME-ROW {: r:ptr :}
   r ROW.NDICT @ ndict!
   r ROW.CP @ cp!
   r ROW.DP @ DICTIONARY-DP!
   depth ;

\ Nothing to discard: this owner's savepoint is three watermark cells in its own
\ frame row, which the next SNAPSHOT at this depth overwrites.
: RELEASE ( -- ) ;

4 constant PARTICIPANT

: INSTALL ( -- )
   PARTICIPANT GENERATED-DECL:ORDER-DICTIONARY
   [: SNAPSHOT ;]
   [: PREPARE ;]
   [: COMMIT ;]
   [: ROLLBACK ;]
   [: RELEASE ;]
   GENERATED-DECL-OWNER:REGISTER ;

public

: PREFLIGHT ( n -- )
   dup 0 < IF E-DICTIONARY-CAP throw THEN
   DICT-CAP ndict@ - > IF E-DICTIONARY-CAP throw THEN ;

: SNAPSHOT-RESET ( -- )
   GENERATED-DECL:DEPTH 0 <> IF E-DICTIONARY-TX throw THEN
   FRAME-BOOT FRAME-P !
   CAP-INIT FRAME-CAP ! ;

private

INSTALL
get-current prot-wid-add

;package
