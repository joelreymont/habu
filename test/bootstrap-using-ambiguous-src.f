\ bootstrap-using-ambiguous-src.f - stage0 rejects an ambiguous used-public name.

\ Recovery-emitter fixture: when two imported packages both export the same tail,
\ a bare use of that tail must be a hard error (USING-AMBIGUOUS = 94), never a
\ silent pick of whichever record the dictionary scan met first. The armed marker
\ proves the engine reached the ambiguous token.

package BUS-A
public
: BUS-BOTH ( -- n ) 7 ;
;package

package BUS-B
public
: BUS-BOTH ( -- n ) 9 ;
;package

s" BOOTSTRAP-USING-ARMED" type cr
using BUS-A
using BUS-B
BUS-BOTH .
