\ checker-dead-path-suite.f - which calls end a path, and where that fact lives.
\
\     bin/hb --load test/checker-dead-path-suite.f
\
\ WHAT IS UNDER TEST. The checker kills the current path after a call that has no
\ normal continuation, and it decides that from the CONTROL FLAGS recorded for the
\ word the token resolved to (src/core/checker.f, CTL-DEAD in the NORETS store,
\ read by DEAD-CUR? / THROW-CUR?). `throw` and `die` carry those flags like any
\ other word: NORET-AXIOMS records them beside the flags a checked definition
\ ending in one earns for itself. There is no second answer anywhere and no
\ spelling is compared.
\
\ WHY THAT MATTERS ENOUGH TO HAVE A SUITE. The two predicates used to recognise
\ the SPELLINGS `throw` and `die` before they looked at any record, and a spelling
\ is not a word: a package that defines its own `throw` binds that word for every
\ bare mention inside it, and the path died anyway. The reproducer certified and
\ then underflowed -
\
\     package P public : throw ( n -- ) drop ;
\     : T2 ( n n -- n ) 0 = if drop 5 throw then ; ;package
\     7 0 P:T2      \ certified ( n n -- n ), left the stack EMPTY
\
\ - so the fact had to move into the store, where it is keyed by the symbol the
\ token really names. Section 3 is that reproducer, held as a refusal.
\
\ HOW EACH CASE ASKS. CHECK-QUIET-CANDIDATE! (test/checker-assert.f) hands one
\ candidate definition to the real checker and answers -1 accepted / 0 refused,
\ which is the same certification path a loaded file takes. Every candidate here
\ is the SAME shape - an `if` arm that leaves one value fewer than the
\ fall-through - so the whole verdict is whether the arm's last call ended the
\ path. A case that accepts and a case that refuses differ only in which word the
\ arm ends with, never in the arm's arithmetic.
\
\ THE HOSTILE FIXTURES ARE IN SECTION 4. Each writes `throw` where no call does -
\ inside a longer name, inside a string literal, inside a comment - so a scan that
\ matched text instead of resolving tokens would kill the arm and accept. Each
\ must be refused. The last of them asks the opposite question: after the real
\ axiom, ordinary code on that path must be refused, which is how the case knows
\ the flag fired rather than the arm passing for some other reason.

require lib/prelude.f
require lib/test.f
require test/checker-assert.f

\ The checker's control-flag reader is a sig-less global that the seal strips, so
\ checked code reaches it behind one declared boundary. It decides nothing: every
\ assertion below is ordinary checked Habu over the number it answers.
package DEADPATH-SHIM
public

TRUSTED: CTL ( ptr u8 n -- n )
   CTL-FLAGS ;

;package

\ ---- fixtures ----------------------------------------------------------------
\ A word that ends a path by its own body: it calls `throw`, so the checker
\ records CTL-DEAD for it exactly as it records the two axioms. Its name is not
\ `throw`, which is the whole point - a reader keying on spelling cannot see it.
package DEADPATH-FIX
public

: BOOM ( n -- )
   throw ;

\ Same body, one name a text scan would match. The checker resolves the token, so
\ the flags it reads are BOOM's.
: THROWN ( n -- )
   drop ;

\ The word this suite exists for: an arm that ends in `throw` leaves nothing where
\ the fall-through leaves a cell, and the definition still certifies.
: JT ( n n -- n )
   0 = if drop E-A-EMPTY throw then ;

;package

\ A package that defines its own `throw` and its own `die`. Inside it a bare
\ mention of either binds THIS word - `( n -- )` and `( n -- )`, both of which
\ return - so an arm ending in one goes on to the join like any other call.
package DEADPATH-SHADOW
public

: throw ( n -- )
   drop ;

: die ( n -- )
   drop ;

;package

\ ---- 1. the two axioms are in the store --------------------------------------
T-RESET

s" the throw axiom is recorded, dead and catchable" T-LABEL
s" throw" DEADPATH-SHIM:CTL CTL-DEAD and CTL-DEAD T=
s" throw" DEADPATH-SHIM:CTL CTL-THROW and CTL-THROW T=

s" the die axiom is recorded, dead and not catchable" T-LABEL
s" die" DEADPATH-SHIM:CTL CTL-DEAD and CTL-DEAD T=
s" die" DEADPATH-SHIM:CTL CTL-THROW and 0 T=

s" an ordinary word carries neither" T-LABEL
s" DEADPATH-FIX:THROWN" DEADPATH-SHIM:CTL 0 T=

s" a word whose own body ends a path earns the same flags" T-LABEL
s" DEADPATH-FIX:BOOM" DEADPATH-SHIM:CTL CTL-DEAD and CTL-DEAD T=

\ ---- 2. an arm that ends a path need not reach the join -----------------------
s" an arm ending in throw certifies against a wider fall-through" T-LABEL
s" DP-A ( n n -- n ) 0 = if drop 5 throw then" CHECK-QUIET-CANDIDATE! -1 T=

s" an arm ending in die certifies the same way" T-LABEL
s\" DP-B ( n n -- n ) 0 = if drop s\q x\q 3 die then" CHECK-QUIET-CANDIDATE! -1 T=

s" so does an arm ending in a word the checker recorded as dead" T-LABEL
s" DP-C ( n n -- n ) 0 = if drop 5 DEADPATH-FIX:BOOM then" CHECK-QUIET-CANDIDATE! -1 T=

s" an arm ending in an ordinary call still has to reach the join" T-LABEL
s" DP-D ( n n -- n ) 0 = if drop 5 DEADPATH-FIX:THROWN then" CHECK-QUIET-CANDIDATE! 0 T=

\ ---- 3. the reproducer: a package word named throw is not the axiom -----------
\ The candidates below are checked with DEADPATH-SHADOW OPEN, so a bare `throw`
\ and a bare `die` bind that package's own words. Both return, so both arms must
\ reach the join - and both leave one value too few.
package DEADPATH-SHADOW

s" inside the package the bare tail is not the axiom" T-LABEL
s" throw" DEADPATH-SHIM:CTL 0 T=
s" die" DEADPATH-SHIM:CTL 0 T=

s" an arm ending in a package word named throw is refused" T-LABEL
s" DP-E ( n n -- n ) 0 = if drop 5 throw then" CHECK-QUIET-CANDIDATE! 0 T=

s" an arm ending in a package word named die is refused" T-LABEL
s" DP-F ( n n -- n ) 0 = if drop 5 die then" CHECK-QUIET-CANDIDATE! 0 T=

s" the qualified axiom still ends the path from inside that package" T-LABEL
s" DP-G ( n n -- n ) 0 = if drop 5 DEADPATH-FIX:BOOM then" CHECK-QUIET-CANDIDATE! -1 T=

;package

\ ---- 4. text that says throw where no call does -------------------------------
s" a word whose NAME contains throw is not the axiom" T-LABEL
s" DP-H ( n n -- n ) 0 = if drop 5 DEADPATH-FIX:THROWN then" CHECK-QUIET-CANDIDATE! 0 T=

s" the spelling inside a string literal ends nothing" T-LABEL
s\" DP-I ( n n -- n ) 0 = if drop s\q throw\q 2drop then" CHECK-QUIET-CANDIDATE! 0 T=

s" the spelling inside a bracket comment ends nothing" T-LABEL
s" DP-J ( n n -- n ) 0 = if drop ( throw ) then" CHECK-QUIET-CANDIDATE! 0 T=

\ A candidate is ONE line of source, and the candidate reader has no `\` comment
\ (a bare backslash in a candidate answers unresolvable, measured), so the line
\ form of the case above cannot be written through this harness. The bracket form
\ is the comment case.

s" the axiom really fired: nothing may follow it on that path" T-LABEL
s" DP-L ( n n -- n ) 0 = if drop E-A-EMPTY throw drop 5 then" CHECK-QUIET-CANDIDATE! 0 T=

\ ---- 5. the certified word does what its arms say -----------------------------
\ A verdict is not a program. JT above certified `( n n -- n )` with a dead arm,
\ so the live path must really leave the cell and the dead one must really throw
\ the code it names - which is what the reproducer's certified-then-underflowed
\ definition could not do.
s" the live path leaves the declared cell" T-LABEL
1 2 DEADPATH-FIX:JT 1 T=

s" the dead path throws the code the arm named" T-LABEL
7 0 ' DEADPATH-FIX:JT catch E-A-EMPTY T=
2drop

T-REPORT