\ enum-ctor-collide-bad.f - KNOWN-FAIL-HARD: a generated constructor name that
\ collides with an existing word kills the process instead of rejecting the
\ declaration.
\
\ WHAT THIS PINS, AND WHY IT IS A FIXTURE RATHER THAN AN ASSERTION.
\ The ORDER 820 constructor participant renders one `PKG:VARIANT` definition per
\ variant and hands the whole plan to the evaluator. Before a definition enters
\ the plan, sumtype.f's TDPLAN-NAME+ asks CHECKER-DEFINED? whether that exact
\ name already exists, and answers a collision with
\   s" sumtype: generated declaration already defined" 76 die
\ `die` is a process exit, not a throw: nothing unwinds, the declaration
\ transaction never reaches rollback, and no `catch` in the repository can
\ observe it. So this behaviour cannot be asserted from inside a suite — the
\ suite would die with it. It is pinned here as a negative gate case with its
\ exact exit code and diagnostic instead.
\
\ This is NOT the participant's own defect and NOT new. The legacy sumtype.f
\ definers reach the identical die through the identical guard, which is why the
\ behaviour is recorded as-is rather than repaired in the constructor-participant
\ lane. The repair — turning that die into a catchable declaration-time throw so
\ the transaction rolls back to a byte-identical registry, exactly like every
\ other generation failure already does — is a sumtype.f change tracked by its
\ own dot. When it lands, this fixture becomes an ordinary in-suite reject
\ assertion next to the other §20f rollback anchors, and the negative case below
\ is removed.
\
\ Until then the guard is doing the right thing for the wrong reason: it refuses
\ to publish a second definition of a live word, which is the corruption that
\ matters, and it fails closed. It just fails closed far too hard.
\
\ Registered as a negative gate case in test/candidate-validation.f. The ARMED
\ marker is the only stdout line, so a build that silently accepts the collision
\ and completes the declaration prints LEAKED-PAST-COLLIDE and fails the
\ exact-marker check even if it also ends up at exit 76 for some other reason.

package ENUM-CTOR-COLLIDE-BAD

TRUSTED: EV ( ptr u8 n -- ) evaluate ;

\ The declaration below is `ecol` with variants `red` and `green`, so the family
\ derives constructor package ECOL and the participant will render `ECOL:RED`.
\ Seeding that exact name first is what the guard has to notice. The seeded word
\ is an ordinary checked definition, written with the same qualified spelling the
\ generator emits, and its body is never reached.
: ECOL:RED ( -- n ) 0 ;

public

\ MAIN is public and runs AFTER the package closes. A declaration evaluated while
\ this package is still open would inherit its private visibility, and a private
\ family generates no constructors at all — the fixture would then report a leak
\ for the wrong reason.
: MAIN ( -- )
   s" ENUM-CTOR-COLLIDE-ARMED" type cr
   s" ENUM-DECL:ED-RUN ecol red green ;ENUM" EV
   s" LEAKED-PAST-COLLIDE" type cr ;

private

;package

ENUM-CTOR-COLLIDE-BAD:MAIN
