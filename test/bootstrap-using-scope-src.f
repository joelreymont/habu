\ bootstrap-using-scope-src.f - stage0 ends a `using` scope at its boundary.

\ Recovery-emitter fixture for the two implicit scope ends: `;package` closes any
\ import opened inside the package block, and leaving an `evaluate` frame closes
\ any import that frame opened. Neither one may leak into the caller, so the bare
\ name is undefined again at the end and the engine exits 70. If either scope end
\ leaks, the last line resolves and the fixture exits 0 instead.

package BUS-A
public
: BUS-VALUE ( -- n ) 7 ;
;package

package BUS-C
public
using BUS-A
: BUS-INNER ( -- n ) BUS-VALUE ;
;package

s" using BUS-A BUS-VALUE drop" evaluate

s" BOOTSTRAP-USING-ARMED" type cr
BUS-VALUE .
