\ bootstrap-using-ambiguous-src.f - stage0 rejects an ambiguous used-public name.
\ schedule-lint: allow-unscheduled - habu-rehome-or-retire-65f56d69 owns the
\ decision. This stage0 fixture's wired siblings have rows in
\ test/candidate-validation.f or tools/package-diff-lint-core.f; this one has none.

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
