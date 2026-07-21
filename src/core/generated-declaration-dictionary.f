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

0 cells constant ROW.NDICT-OFF
1 cells constant ROW.CP-OFF
2 cells constant ROW.DP-OFF
3 cells constant ROW-REC

4 constant CAP-INIT
create FRAME-BOOT CAP-INIT ROW-REC * allot
PTR-VARIABLE FRAME-P FRAME-BOOT FRAME-P !
variable FRAME-CAP CAP-INIT FRAME-CAP !
variable FRAME-N

: FRAME-BASE ( -- ptr a ) FRAME-P @ ;
: FRAME-ROW ( n -- ptr a ) ROW-REC * FRAME-BASE + ;
: ROW.NDICT ( ptr a -- ptr a ) ROW.NDICT-OFF + ;
: ROW.CP ( ptr a -- ptr a ) ROW.CP-OFF + ;
: ROW.DP ( ptr a -- ptr ptr a ) ROW.DP-OFF CELL / ptr-field ;

TRUSTED: FRAME-GROW ( ptr a n n -- ptr a ) ARENA-BYTES-GROW ;
TRUSTED: DICTIONARY-DP! ( ptr a -- ) data-base DP-CELL + ! ;

: ENSURE-ROOM ( -- )
   FRAME-N @ FRAME-CAP @ < IF EXIT THEN
   FRAME-P @ FRAME-CAP @ ROW-REC *
      FRAME-CAP @ 2 * ROW-REC * FRAME-GROW FRAME-P !
   FRAME-CAP @ 2 * FRAME-CAP ! ;

: REQUIRE-DEPTH ( n -- ) FRAME-N @ <> IF E-DICTIONARY-TX throw THEN ;

: SNAPSHOT ( n -- n ) {: depth:n :}
   depth FRAME-N @ 1 + <> IF E-DICTIONARY-TX throw THEN
   ENSURE-ROOM
   FRAME-N @ FRAME-ROW {: r:ptr :}
   ndict@ r ROW.NDICT !
   cp@ r ROW.CP !
   here r ROW.DP !
   FRAME-N @ 1 + FRAME-N !
   depth ;

: PREPARE ( n -- n ) ;
: COMMIT ( n -- n ) ;

: ROLLBACK ( n -- n ) {: depth:n :}
   depth REQUIRE-DEPTH
   FRAME-N @ 1 - FRAME-ROW {: r:ptr :}
   r ROW.NDICT @ ndict!
   r ROW.CP @ cp!
   r ROW.DP @ DICTIONARY-DP!
   FRAME-N @ 1 - FRAME-N !
   depth ;

: FINALIZE ( n -- n ) {: depth:n :}
   depth REQUIRE-DEPTH
   FRAME-N @ 1 - FRAME-N !
   depth ;

4 constant PARTICIPANT

: INSTALL ( -- )
   0 FRAME-N !
   PARTICIPANT GENERATED-DECL:ORDER-DICTIONARY
   [: SNAPSHOT ;]
   [: PREPARE ;]
   [: COMMIT ;]
   [: ROLLBACK ;]
   [: FINALIZE ;]
   GENERATED-DECL-OWNER:REGISTER ;

public

: DEPTH ( -- n ) FRAME-N @ ;

private

INSTALL
get-current prot-wid-add

;package
