\ checker-dead-path-suite.f - which calls end a path.
\
\ The checker ends a path after a call by the control flags of the word the
\ token resolves to (src/core/checker.f: CTL-DEAD in the NORETS store, read by
\ DEAD-CUR? and THROW-CUR?; NORET-AXIOMS records `throw` and `die`), never by
\ its spelling: a package's own `throw` returns.

require src/lib/prelude.f
require src/lib/test.f

\ ---- fixtures ----------------------------------------------------------------
package DEADPATH-FIX
public

: BOOM ( n -- )
   throw ;

: THROWN ( n -- )
   drop ;

: JT ( n n -- n )
   0 = if drop E-A-EMPTY throw then ;

;package

package DEADPATH-SHADOW
public

: throw ( n -- )
   drop ;

: die ( n -- )
   drop ;

;package

\ Each body calls the engine's word of the same spelling, which its bare token
\ binds while the definition is unpublished. The flags a definition earns belong
\ to the symbol it is recorded under, not to the engine word its name shadows.
package DEADPATH-OWN
public

: DIE ( ptr u8 n -- )
   3 die ;

: THROW ( n -- )
   throw ;

;package

\ ---- 1. an arm that ends a path need not reach the join -----------------------
T-RESET

\ Every candidate is an `if` arm one value short of the fall-through: the
\ verdict is whether its last call ends it.
s" an arm ending in throw certifies against a wider fall-through" T-LABEL
s" DP-A ( n n -- n ) 0 = if drop 5 throw then" CHECK-QUIET-CANDIDATE! -1 T=

s" an arm ending in die certifies the same way" T-LABEL
s\" DP-B ( n n -- n ) 0 = if drop s\q x\q 3 die then" CHECK-QUIET-CANDIDATE! -1 T=

s" so does an arm ending in a word the checker recorded as dead" T-LABEL
s" DP-C ( n n -- n ) 0 = if drop 5 DEADPATH-FIX:BOOM then" CHECK-QUIET-CANDIDATE! -1 T=

s" an arm ending in an ordinary call still has to reach the join" T-LABEL
s" DP-D ( n n -- n ) 0 = if drop 5 DEADPATH-FIX:THROWN then" CHECK-QUIET-CANDIDATE! 0 T=

\ ---- 2. the reproducer: a package word named throw is not the axiom -----------
\ Checked with DEADPATH-SHADOW open: a bare `throw` and `die` bind its words,
\ which return, so both arms must reach the join one value short.
package DEADPATH-SHADOW

s" an arm ending in a package word named throw is refused" T-LABEL
s" DP-E ( n n -- n ) 0 = if drop 5 throw then" CHECK-QUIET-CANDIDATE! 0 T=

s" an arm ending in a package word named die is refused" T-LABEL
s" DP-F ( n n -- n ) 0 = if drop 5 die then" CHECK-QUIET-CANDIDATE! 0 T=

s" the qualified axiom still ends the path from inside that package" T-LABEL
s" DP-G ( n n -- n ) 0 = if drop 5 DEADPATH-FIX:BOOM then" CHECK-QUIET-CANDIDATE! -1 T=

;package

\ ---- 2b. a package word under an engine spelling that does end the path -------
\ The converse of section 2, checked with DEADPATH-OWN open: the bare tails bind
\ that package's words, whose bodies end in the axioms.
package DEADPATH-OWN

s" an arm ending in it certifies against a wider fall-through" T-LABEL
s\" DP-M ( n n -- n ) 0 = if drop s\q x\q DIE then" CHECK-QUIET-CANDIDATE! -1 T=

s" nothing may follow it on that path" T-LABEL
s\" DP-N ( n n -- n ) 0 = if drop s\q x\q DIE 5 then" CHECK-QUIET-CANDIDATE! 0 T=

;package

\ ---- 3. text that says throw where no call does -------------------------------
s" a word whose NAME contains throw is not the axiom" T-LABEL
s" DP-H ( n n -- n ) 0 = if drop 5 DEADPATH-FIX:THROWN then" CHECK-QUIET-CANDIDATE! 0 T=

s" the spelling inside a string literal ends nothing" T-LABEL
s\" DP-I ( n n -- n ) 0 = if drop s\q throw\q 2drop then" CHECK-QUIET-CANDIDATE! 0 T=

s" the spelling inside a bracket comment ends nothing" T-LABEL
s" DP-J ( n n -- n ) 0 = if drop ( throw ) then" CHECK-QUIET-CANDIDATE! 0 T=

\ A candidate is one line and the candidate reader has no `\` comment (a bare
\ backslash is refused as an undefined word): the bracket form is the comment
\ case.

s" the axiom really fired: nothing may follow it on that path" T-LABEL
s" DP-L ( n n -- n ) 0 = if drop E-A-EMPTY throw drop 5 then" CHECK-QUIET-CANDIDATE! 0 T=

\ ---- 4. the certified word does what its arms say -----------------------------
s" the live path leaves the declared cell" T-LABEL
1 2 DEADPATH-FIX:JT 1 T=

s" the dead path throws the code the arm named" T-LABEL
: JT-DEAD ( -- ) 7 0 DEADPATH-FIX:JT drop ;
' JT-DEAD catch E-A-EMPTY T=

T-REPORT
