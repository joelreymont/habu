\ Child of test/arena.f: a package's own `field-project` is refused at its
\ definition.
require src/lib/string.f

1024 constant CAP-ROOM
CAP-ROOM BUFFER: CAP-SRC
variable CAP-U
: CAP, ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ CAP-SRC CAP-U @ + c! CAP-U @ 1 + CAP-U ! loop ;


TYPED-VARIABLE ROW-A ptr u8
variable ROW-U
: ROW-RUN ( -- ) ROW-A @ ROW-U @ INCLUDE-EVALUATE ;
: OWNER-ROW ( ptr u8 n ptr u8 n -- ) ROW-U ! ROW-A ! type [: ROW-RUN ;] catch . ;

\ STRUCTURE's accessors spell `field-project` unqualified, so a sealed engine
\ refuses it as a definition name. Were STRUCT-CAPTURE's admitted, S2-Y's store
\ would land in V, printing 77 twice. The row prints its refusal code. The
\ package opens in this file, so the refused row leaves none open.
: LINE, ( ptr u8 n -- ) CAP, s"  " CAP, ;


: STRUCT-CAPTURE-SRC$ ( -- ptr u8 n )
   0 CAP-U !
   s" variable V" LINE,
   s" : field-project ( a n -- ptr n ) drop drop V ;" LINE,
   s" STRUCTURE s2 DERIVE addr FIELD x n FIELD y n ;STRUCTURE" LINE,
   s" TYPED-VARIABLE R s2" LINE,
   s" : RUN ( -- ) 77 R S2-Y !  V @ .  R S2-Y @ . ;  RUN" LINE,
   CAP-SRC CAP-U @ ;


: STRUCT-CAPTURE-ROW ( -- ) s" struct-capture: " STRUCT-CAPTURE-SRC$ OWNER-ROW ;
package STRUCT-CAPTURE
STRUCT-CAPTURE-ROW
;package
