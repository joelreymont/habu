\ bootstrap-using-unknown-src.f - stage0 rejects `using` of an unknown package.
\ schedule-lint: allow-unscheduled - habu-rehome-or-retire-65f56d69 owns the
\ decision. This stage0 fixture's wired siblings have rows in
\ test/candidate-validation.f or tools/package-diff-lint-core.f; this one has none.

\ Recovery-emitter fixture: `using NAME` may only name a package that exists, and
\ the failure must be the named diagnostic plus the engine-error exit status the
\ native engine uses (USING-UNKNOWN = 91), never a silent no-op import. The armed
\ marker proves the engine reached the `using` line.

package BUS-A
public
: BUS-VALUE ( -- n ) 7 ;
;package

s" BOOTSTRAP-USING-ARMED" type cr
using NOSUCH-PACKAGE
BUS-VALUE .
