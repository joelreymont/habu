\ A fresh native compiler publishes into the retained build's open shadow.
\ Its emission handles belong to its own scratch region: borrow its readers,
\ then copy through the ordinary NEMIT/NART representation before returning.
require src/compiler/native/compiler.f
require src/compiler/native/backend.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/compiler/ir/context.f
require src/compiler/session/emission.f
require src/compiler/session/lease.f
require src/habu/layout.f
require src/habu/xref.f

package NSHADOW-TAIL
private

variable SOURCE
variable ENTRY
TYPED-VARIABLE LEASE NLEASE:lease
\ Retained driver cells are outside the captured target. Keep foreign entries
\ as integers here: storing an xt would declare retired host-heap address rows.
variable DOES
variable DOES-FUN
variable BODY
variable SCOPE

CAST: READ-N ( n -- [ n -- n ] )
CAST: READ-ROW ( n -- [ n n -- n ] )
CAST: READ-BYTES ( n -- [ n -- ptr u8 ] )
CAST: READ-ARCH ( n -- [ n -- CTARGET:arch ] )
CAST: READ-CELL ( n -- [ -- ptr n ] )
CAST: SHADOW-SCOPE ( n -- [ [ n n n n -- ] [ n -- ] [ -- ] -- ] )
CAST: FIXED>N ( [ n n n -- ] -- n )
CAST: PATCH>N ( [ n n -- ] -- n )
CAST: BODY>N ( [ -- ] -- n )
CAST: BODY-XT ( n -- [ -- ] )

: ENTRY-OF ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if E-NCOMP-STATE throw then
   XREF-START ;

: PRIVATE-ENTRY ( ptr u8 n -- n )
   s" NCOMP" XREF-NAMESPACE-WL XREF-FIND-WL
   XREF-PKG-PRIVATE XREF-FIND-WL
   dup XREF-FOUND? 0= if E-NCOMP-STATE throw then
   XREF-START ;

: N@ ( ptr u8 n -- n )
   ENTRY-OF READ-N SOURCE @ swap execute ;

: ROW@ ( n ptr u8 n -- n )
   ENTRY-OF READ-ROW {: q :}
   SOURCE @ swap q execute ;

: REPLAY ( -- )
   SOURCE @ s" NART:BYTES" ENTRY-OF READ-BYTES execute
   s" NART:SIZE" N@ s" NART:RET-BYTES" N@
   SOURCE @ s" NART:ARCH" ENTRY-OF READ-ARCH execute NEMIT:OPEN
   ENTRY @ NEMIT:PLACE
   s" NART:PATCH-SLOT" N@ dup 0 >= if NEMIT:PATCH-SLOT! else drop then
   s" NART:FUNCTIONS" N@ 0 ?do
      i s" NART:FUNCTION-OFFSET@" ROW@ NEMIT:FUNCTION+
   loop
   s" NART:CALL-SITES" N@ 0 ?do
      i s" NART:CALL-SITE@" ROW@
      i s" NART:CALL-KIND@" ROW@
      i s" NART:CALL-TARGET@" ROW@
      i s" NART:CALL-IMPL@" ROW@
      i s" NART:CALL-LOC@" ROW@ NEMIT:CALL-SITE+
   loop
   s" NART:ADDR-SITES" N@ 0 ?do
      i s" NART:ADDR-SITE@" ROW@
      i s" NART:ADDR-SITE-KIND@" ROW@
      i s" NART:ADDR-SITE-ORIGIN@" ROW@ NEMIT:ADDR-SITE+
   loop
   NEMIT:SEAL ;

: COPY ( NSESSION:session -- )
   NART:COPY {: e:NART:emission :}
   DOES @ READ-CELL execute @ 0<> if
      e DOES-FUN @ READ-CELL execute @ NSHADOW:TAKE-DOES
   else e NSHADOW:TAKE then
   e ENTRY @ NSHADOW:SOURCE-FUNS
   e NART:RELEASE ;

: IN-CONTEXT ( IR-CTX:ctx -- )
   LEASE @ NSESSION:NEW [: COPY ;] NSESSION:WITH-WORK ;

: COPY-LEASED ( NLEASE:lease -- )
   LEASE !
   NSHADOW:BINDING [: IN-CONTEXT ;] IR-CTX:WITH-CONTEXT ;

\ The source owns its bytes and handle only through this precommit callback.
\ TAKE reserves every row, so COMMIT below cannot allocate or read the source.
: TAKE ( n n n n -- )
   {: source:n idx:n entry:n size:n :}
   idx drop size drop
   source SOURCE ! entry ENTRY !
   [: REPLAY [: COPY-LEASED ;] NLEASE:WITH ;] [: NEMIT:CLEAR ;] finally ;

: COMMIT ( n -- ) NSHADOW:PUBLISH ;

: FIXED ( n n n -- ) NCOMP:SHADOW-FIXED ;
: PATCH ( n n -- ) NCOMP:SHADOW-DOES ;

: CELL@ ( n -- n ) data-base + CELL-VIEW @ ;
: CELL! ( n n -- ) data-base + CELL-VIEW ! ;

: RUN ( -- ) ['] TAKE ['] COMMIT BODY @ BODY-XT SCOPE @ SHADOW-SCOPE execute ;

public

: WITH ( [ -- ] -- )
   {: q :}
   s" M-DOES" PRIVATE-ENTRY DOES !
   s" M-DOES-FUN" PRIVATE-ENTRY DOES-FUN !
   s" NPUB:WITH-SHADOW" ENTRY-OF SCOPE !
   q BODY>N BODY !
   NCOMP-DISPATCH:FIXED-SHADOW-CELL CELL@ {: fixed:n :}
   NCOMP-DISPATCH:DOES-SHADOW-CELL CELL@ {: patch:n :}
   ['] FIXED FIXED>N NCOMP-DISPATCH:FIXED-SHADOW-CELL CELL!
   ['] PATCH PATCH>N NCOMP-DISPATCH:DOES-SHADOW-CELL CELL!
   [: RUN ;] catch {: rc:n :}
   fixed NCOMP-DISPATCH:FIXED-SHADOW-CELL CELL!
   patch NCOMP-DISPATCH:DOES-SHADOW-CELL CELL!
   NSHADOW:ABANDON
   0 SOURCE ! 0 ENTRY !
   rc 0<> if rc throw then ;

;package
