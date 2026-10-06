\ checker-decl-locs-lib.f - arm a declaration location as the verifier does,
\ and read back the location a row took, for the files that test the
\ checker's declaration locations (src/core/checker.f DECL-LOCS):
\ test/checker-decl-locs.f and test/checker-decl-locs-capture-subject.f; and
\ arm a completion cursor as the verifier does, for test/checker-completion.f. It
\ reads checker internals, so only the unsealed engine
\ (test/whitebox-engine.f) loads it.

require lib/test.f
require src/habu/verify-source.f

package NAVL

private

\ The owner record's slots, called as the verifier calls them
\ (src/habu/verify-source.f OWNER-XT).
: SLOT ( n -- n )
   {: off:n :}
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   off CELL + CHECKER-OWNER-GUARD:VALIDATE
   off + CELL-VIEW @ dup 0= IF E-NCOMP-OWNER throw THEN ;

CAST: ARM-ACTION ( n -- [ ptr u8 n n n n -- ] )
CAST: OFF-ACTION ( n -- [ -- ] )
CAST: USES-ACTION ( n -- [ [ n n n n n -- ] [ -- ] -- ] )
CAST: CURSOR-ACTION ( n -- [ n [ ptr u8 n n n n -- ] -- ] )
CAST: VISIBLE-ACTION ( n -- [ ptr u8 n n [ ptr u8 n n n n -- ] -- ] )

public

: ARM ( ptr u8 n n n n -- )
   NCOMP-DISPATCH:DECL-VERIFY-DECL-ARM-OFF SLOT ARM-ACTION execute ;
: DISARM ( -- )
   NCOMP-DISPATCH:DECL-VERIFY-DECL-DISARM-OFF SLOT OFF-ACTION execute ;
: WITH-USES ( [ n n n n n -- ] [ -- ] -- )
   NCOMP-DISPATCH:DECL-VERIFY-USES-OFF SLOT USES-ACTION execute ;
\ Completion: the cursor the next body check fires at (-1 disarms), and the
\ spellings that bind at a position of a kind (CHECKER-OWNER-ABI:VISIBLE-*),
\ each with its declaration's visit, start and end.
: CURSOR! ( n [ ptr u8 n n n n -- ] -- )
   CHECKER-OWNER-ABI:VERIFY-CURSOR-OFF SLOT CURSOR-ACTION execute ;
: EACH-VISIBLE ( ptr u8 n n [ ptr u8 n n n n -- ] -- )
   NCOMP-DISPATCH:DECL-VERIFY-EACH-VISIBLE-OFF SLOT VISIBLE-ACTION execute ;

\ The armed location: a visit and byte range no source on this path holds.
7 constant VISIT
100 constant AT-START
110 constant AT-END

\ Replay TEXT with NAME armed at visit V, bytes S to E, as the verifier arms a
\ registrar call, and run QUERIES in the replay's own scope once it is through
\ (VERIFY:SOURCE-BUF-THEN-IN-SCOPE). A replay compiles nothing: a name it
\ declares binds only while its scope is open, so the queries read the rows
\ there, as the verifier reads uses and locations during its own check.
: PLACED-REPLAY ( ptr u8 n ptr u8 n n n n [ -- ] -- )
   {: na:ptr nu:n a:ptr u:n v:n s:n e:n queries :}
   na nu v s e ARM
   a u queries VERIFY:SOURCE-BUF-THEN-IN-SCOPE
   DISARM ;

\ Replay TEXT with NAME and the location armed, then QUERIES.
: ARMED-REPLAY ( ptr u8 n ptr u8 n [ -- ] -- )
   {: na:ptr nu:n a:ptr u:n queries :}
   na nu a u VISIT AT-START AT-END queries PLACED-REPLAY ;

\ Run XT in a neutral checker scope, the rollback frame a verifier child checks
\ its whole composition in (tools/check-verify-child.f SCOPED): its rewind
\ takes back every row XT's checks retained.
: IN-NEUTRAL ( [ -- ] -- )
   CHECKER-SCOPE-START-NEUTRAL
   catch
   CHECKER-SCOPE-DONE
   dup 0= IF drop EXIT THEN
   throw ;

\ NAME's newest row, offset+1; 0 when the name or its row is missing. Ask it
\ where the replay that declared NAME binds it: in that replay's queries, or
\ in a neutral checker scope the replay ran in.
: ROW ( ptr u8 n -- n )
   CHECKER-FIND-ACTIVE-SYM {: sym:n :}
   sym 0= IF 0 EXIT THEN
   sym USIG-NEWEST ;

\ The row, offset+1, is there and has no location.
: ROW-UNLOCATED ( n -- )
   {: rec1:n :}
   rec1 0 T<>
   rec1 CHECKER-REC-DECL-AT {: v:n s:n e:n found:bool :}
   found TFALSE ;

\ NAME has a row, and the row has no location.
: UNLOCATED ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u T-LABEL
   a u ROW ROW-UNLOCATED ;

\ NAME has a row, and the row carries the armed location.
: LOCATED ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u T-LABEL
   a u ROW {: rec1:n :}
   rec1 0 T<>
   rec1 CHECKER-REC-DECL-AT {: v:n s:n e:n found:bool :}
   found TTRUE
   v VISIT T=
   s AT-START T=
   e AT-END T= ;

;package
