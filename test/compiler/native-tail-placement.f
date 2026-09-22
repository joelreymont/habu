\ Tail transfers keep the callee's stack position even when another is cheaper.
\ Tier 1 first: a tail transfer is lowered only by the optimizing compiler, so
\ the placement asserted below is a tier-1 fact (1 row fails at the default tier).
1 set-tier

require lib/test.f
require tools/codegen-tail-probe.f

package TAIL-PLACEMENT
public

: ID ( n -- n ) ;

: TWICE ( n -- n )
   dup ID swap ID + ID ;

: TWICE-PLUS-ONE ( n -- n )
   dup ID swap ID + ID 1+ ;

private

: RUN ( -- )
   T-RESET
   s" TAIL-PLACEMENT:TWICE" NTAILPROBE:TAIL-BRANCH? TTRUE
   s" TAIL-PLACEMENT:TWICE-PLUS-ONE" NTAILPROBE:TAIL-BRANCH? TFALSE
   123 21 TWICE 42 T= 123 T=
   0 TWICE 0 T=
   -21 TWICE -42 T=
   21 TWICE-PLUS-ONE 43 T=
   T-REPORT ;

RUN
;package
