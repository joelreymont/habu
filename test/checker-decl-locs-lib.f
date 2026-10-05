\ checker-decl-locs-lib.f - arm a declaration location as the verifier does,
\ and read back the location a row took, for the files that test the
\ checker's declaration locations (src/core/checker.f DECL-LOCS):
\ test/checker-decl-locs.f and test/checker-decl-locs-capture-subject.f. It
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

CAST: ARM-ACTION ( n -- [ n n n -- ] )
CAST: OFF-ACTION ( n -- [ -- ] )
CAST: USES-ACTION ( n -- [ [ n n n n n -- ] [ -- ] -- ] )

PTR-VARIABLE ASK-A   variable ASK-U   variable ASK-ROW
: ASK ( -- )
   ASK-A @ ASK-U @ CHECKER-FIND-ACTIVE-SYM {: sym:n :}
   sym 0= IF 0 ASK-ROW ! EXIT THEN
   sym USIG-NEWEST ASK-ROW ! ;

public

: ARM ( n n n -- )
   NCOMP-DISPATCH:DECL-VERIFY-DECL-ARM-OFF SLOT ARM-ACTION execute ;
: DISARM ( -- )
   NCOMP-DISPATCH:DECL-VERIFY-DECL-DISARM-OFF SLOT OFF-ACTION execute ;
: WITH-USES ( [ n n n n n -- ] [ -- ] -- )
   NCOMP-DISPATCH:DECL-VERIFY-USES-OFF SLOT USES-ACTION execute ;

\ The armed location: a visit and byte range no source on this path holds.
7 constant VISIT
100 constant AT-START
110 constant AT-END

\ Replay TEXT with the location armed, as the verifier arms a registrar call.
: ARMED-REPLAY ( ptr u8 n -- )
   {: a:ptr u:n :}
   VISIT AT-START AT-END ARM
   a u VERIFY:SOURCE-BUF-IN-SCOPE
   DISARM ;

\ A replay compiles nothing, so the engine's dictionary holds none of these
\ names: they bind inside the verifier's package scope, over the checker's own
\ records, as src/habu/verify-source.f RUN-IN-SCOPE binds them.
: IN-SCOPE ( [ -- ] -- )
   NCOMP-DISPATCH:DECL-VERIFY-START-OFF SLOT OFF-ACTION execute
   catch
   NCOMP-DISPATCH:DECL-VERIFY-DONE-OFF SLOT OFF-ACTION execute
   dup 0= IF drop EXIT THEN
   throw ;

\ NAME's newest row, offset+1; 0 when the name or its row is missing.
: ROW ( ptr u8 n -- n )
   ASK-U !  ASK-A !
   [: ASK ;] IN-SCOPE
   ASK-ROW @ ;

\ NAME has a row, and the row has no location.
: UNLOCATED ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u T-LABEL
   a u ROW {: rec1:n :}
   rec1 0 T<>
   rec1 CHECKER-REC-DECL-AT {: v:n s:n e:n found:bool :}
   found TFALSE ;

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
