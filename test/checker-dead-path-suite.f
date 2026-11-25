\ checker-dead-path-suite.f - which calls end a path.
\
\ A call ends its path when the word its token resolves to never returns,
\ whatever its spelling: a package's own `throw` returns. `execute`,
\ `finally` and a word whose result row only its quotations' outputs name end
\ it when the quotation cells they take hold `[: … ;]` literals that never
\ return; `catch` only when its literal never throws either, as a throw
\ returns to it. The fact rides on the cell, so a word's result, a join of
\ two different literals and a cell carried into a loop do not carry it.

require src/lib/prelude.f
require src/lib/memory.f
require src/lib/test.f
require src/lib/adt/option.f

\ ---- fixtures ----------------------------------------------------------------
package DEADPATH-FIX
public

: BOOM ( n -- )
   throw ;

: THROWN ( n -- )
   drop ;

: JT ( n n -- n )
   0 = if drop E-A-EMPTY throw then ;

: GONE ( -- )
   s" gone" 3 die ;

: CHOOSE ( a a bool -- a )
   if drop else nip then ;

: APP ( R [ R -- S ] -- S )
   execute ;

: APPR ( R [ R -- R ] -- R )
   execute ;

: APPRN ( R [ R -- R ] -- R n )
   execute 0 ;

: APPRET ( R [ R -- S ] | U -- S | U )
   execute ;

: SCOPE ( n ^u8 NUM:alloc-byte-len -- )
   drop drop drop 5 throw ;

: KEEP ( n ^u8 NUM:alloc-byte-len -- n )
   drop drop ;

: NEVER ( n -- )
   8 MEM:BYTES-ALLOC-LEN [: SCOPE ;] MEM:WITH-BYTES ;

: AROUND ( [ -- ] -- )
   catch drop ;

: APP2 ( R [ R -- S ] [ R -- S ] bool -- S )
   if drop execute else nip execute then ;

: APPO ( R [ R -- S ] option<n> -- S )
   drop execute ;

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

: DIE ( str -- )
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

\ ---- 3b. a combinator given only quotations that never return -------------
s" a combinator running a quotation that never returns ends the arm" T-LABEL
s" DP-O ( n n -- n ) 0 = if 8 MEM:BYTES-ALLOC-LEN [: DEADPATH-FIX:SCOPE ;] MEM:WITH-BYTES then" CHECK-QUIET-CANDIDATE! -1 T=

s" nothing may follow that call on its path" T-LABEL
s" DP-P ( n n -- n ) 0 = if 8 MEM:BYTES-ALLOC-LEN [: DEADPATH-FIX:SCOPE ;] MEM:WITH-BYTES 5 then" CHECK-QUIET-CANDIDATE! 0 T=

s" a combinator running a quotation that returns still returns" T-LABEL
s" DP-Q ( n n -- n ) 0 = if 8 MEM:BYTES-ALLOC-LEN [: DEADPATH-FIX:KEEP ;] MEM:WITH-BYTES 1 + then" CHECK-QUIET-CANDIDATE! -1 T=

s" a combinator whose result row is its input row still returns" T-LABEL
s" DP-R ( n n -- n ) 0 = if drop [: 5 throw ;] DEADPATH-FIX:AROUND then" CHECK-QUIET-CANDIDATE! 0 T=

s" a word ending in such a call ends the arm too" T-LABEL
s" DP-S ( n n -- n ) 0 = if drop 5 DEADPATH-FIX:NEVER then" CHECK-QUIET-CANDIDATE! -1 T=

s" a quotation joined from one that never returns and one that returns still returns" T-LABEL
s" DP-T ( n n -- n ) 0 = if 0 = if [: 5 throw ;] else [: ;] then DEADPATH-FIX:APP then" CHECK-QUIET-CANDIDATE! 0 T=

s" a combinator choosing between one that never returns and one that returns still returns" T-LABEL
s" DP-U ( n n -- n ) 0 = if drop [: 5 throw ;] [: ;] true DEADPATH-FIX:APP2 then" CHECK-QUIET-CANDIDATE! 0 T=

s" a combinator whose result row is its quotation's input row still returns" T-LABEL
s" DP-V ( n n -- n ) 0 = if drop [: 5 throw ;] DEADPATH-FIX:APPRN 1 + then" CHECK-QUIET-CANDIDATE! -1 T=

s" a case arm keeps the cells its selector code swapped" T-LABEL
s" DP-W ( n -- ) [: ;] [: 5 throw ;] rot case >r swap r> 1 of 2drop endof drop nip execute endcase" CHECK-QUIET-CANDIDATE! 0 T=

\ The callee holds option<n> as two cells, the caller OPTION:NONE's open-width
\ option as one: the quotation below it pairs with the live argument.
s" a quotation below a multi-cell value is paired with its own argument" T-LABEL
s" DP-X ( n n -- n ) 0 = if drop [: 5 throw ;] [: drop ;] OPTION:NONE DEADPATH-FIX:APPO 5 then" CHECK-QUIET-CANDIDATE! -1 T=

s" a combinator with a return-stack clause still returns" T-LABEL
s" DP-Z ( n n -- n ) 0 = if drop [: 5 throw ;] DEADPATH-FIX:APPRET then" CHECK-QUIET-CANDIDATE! 0 T=

\ ---- 4. the certified word does what its arms say -----------------------------
s" the live path leaves the declared cell" T-LABEL
1 2 DEADPATH-FIX:JT 1 T=

s" the dead path throws the code the arm named" T-LABEL
: JT-DEAD ( -- ) 7 0 DEADPATH-FIX:JT drop ;
' JT-DEAD catch E-A-EMPTY T=

\ ---- 5. a cell holding a never-returning literal ends the path ---------------
\ The fact is on the cell `[: ;]` pushed, not its type: a value sharing only a
\ term with it returns. Each candidate declares one more value than it leaves.
s" a literal straight to execute ends the path" T-LABEL
s" DN-A ( -- n ) [: 5 throw ;] execute" CHECK-QUIET-CANDIDATE! -1 T=

s" a dying literal straight to catch ends the path" T-LABEL
s\" DN-B ( -- n n ) [: s\q x\q 3 die ;] catch" CHECK-QUIET-CANDIDATE! -1 T=

s" a literal straight to finally as the body ends the path" T-LABEL
s" DN-C ( -- n ) [: 5 throw ;] [: ;] finally" CHECK-QUIET-CANDIDATE! -1 T=

s" a literal straight to finally as the cleanup ends the path" T-LABEL
s" DN-D ( -- n ) [: ;] [: 5 throw ;] finally" CHECK-QUIET-CANDIDATE! -1 T=

s" the fact follows dup" T-LABEL
s" DN-E ( -- n ) [: 5 throw ;] dup drop execute" CHECK-QUIET-CANDIDATE! -1 T=

s" the fact follows swap" T-LABEL
s" DN-F ( n -- n n ) [: 5 throw ;] swap drop execute" CHECK-QUIET-CANDIDATE! -1 T=

s" the fact follows rot" T-LABEL
s" DN-G ( n n -- n ) [: 5 throw ;] rot rot 2drop execute" CHECK-QUIET-CANDIDATE! -1 T=

s" the fact follows over" T-LABEL
s" DN-H ( -- n ) [: 5 throw ;] 1 over execute" CHECK-QUIET-CANDIDATE! -1 T=

s" the fact follows a local" T-LABEL
s" DN-I ( -- n ) [: 5 throw ;] {: q :} q execute" CHECK-QUIET-CANDIDATE! -1 T=

s" a local read inside a nested quotation keeps the fact" T-LABEL
s" DN-J ( -- n ) [: 5 throw ;] {: q :} [: q execute ;] execute" CHECK-QUIET-CANDIDATE! -1 T=

s" a nested literal that ends dead makes its outer literal dead" T-LABEL
s" DN-K ( -- n ) [: [: 5 throw ;] execute ;] execute" CHECK-QUIET-CANDIDATE! -1 T=

s" the same literal in both arms keeps the fact" T-LABEL
s" DN-L ( bool -- n ) [: 5 throw ;] swap if dup drop else 0 drop then execute" CHECK-QUIET-CANDIDATE! -1 T=

s" catch around a throwing literal returns" T-LABEL
s" DN-M ( -- n ) [: 5 throw ;] catch 1 +" CHECK-QUIET-CANDIDATE! -1 T=

s" a tick of a never-returning word returns" T-LABEL
s" DN-N ( -- n ) ['] DEADPATH-FIX:GONE execute 1" CHECK-QUIET-CANDIDATE! -1 T=

\ ---- 6. a value that only shares a term with a dead literal returns -----------
s" a word's result is not the literal: CHOOSE then execute" T-LABEL
s" DN-O ( -- n ) [: ;] [: 5 throw ;] true DEADPATH-FIX:CHOOSE execute" CHECK-QUIET-CANDIDATE! 0 T=

s" a quotation that returns either literal" T-LABEL
s" DN-P ( -- n ) [: ;] false [: if drop [: 5 throw ;] then ;] execute execute" CHECK-QUIET-CANDIDATE! 0 T=

s" a cell a quotation argument moved" T-LABEL
s" DN-Q ( -- n ) [: ;] [: 5 throw ;] [: swap ;] DEADPATH-FIX:APPR nip execute" CHECK-QUIET-CANDIDATE! 0 T=

s" a chosen cleanup to finally" T-LABEL
s" DN-R ( -- n ) [: ;] [: ;] [: 5 throw ;] true DEADPATH-FIX:CHOOSE finally" CHECK-QUIET-CANDIDATE! 0 T=

s" a join of a dying and a throwing literal to catch" T-LABEL
s\" DN-S ( bool -- ) if [: s\q x\q 1 die ;] else [: 5 throw ;] then catch" CHECK-QUIET-CANDIDATE! 0 T=

s" a chosen body to catch" T-LABEL
s" DN-T ( -- n ) [: 1 2 ;] [: 1 2 5 throw ;] true DEADPATH-FIX:CHOOSE catch" CHECK-QUIET-CANDIDATE! 0 T=

s" a cell carried into a loop" T-LABEL
s" DN-U ( n -- ) [: 5 throw ;] swap 0 ?do i 0 > if execute then drop [: ;] loop drop" CHECK-QUIET-CANDIDATE! 0 T=

s" CHOOSE then APP" T-LABEL
s" DN-V ( -- n ) [: ;] [: 5 throw ;] true DEADPATH-FIX:CHOOSE DEADPATH-FIX:APP" CHECK-QUIET-CANDIDATE! 0 T=

s" a quotation that returns either literal, then APP" T-LABEL
s" DN-W ( -- n ) [: ;] false [: if drop [: 5 throw ;] then ;] execute DEADPATH-FIX:APP" CHECK-QUIET-CANDIDATE! 0 T=

s" CHOOSE then APP inside an executed literal" T-LABEL
s" DN-X ( -- n ) [: ;] [: [: 5 throw ;] true DEADPATH-FIX:CHOOSE DEADPATH-FIX:APP ;] execute" CHECK-QUIET-CANDIDATE! 0 T=

\ ---- 7. the rejection names the call that ended the path ----------------------
: DP-NAMES-EXECUTE ( -- )
   s" code after execute of a never-returning literal names the execute" T-LABEL
   REJECTION:CAPTURE
   [: s" : DP-Y ( -- n ) [: 5 throw ;] execute 7 ;" evaluate-closed ;] 70 TTHROWSQ
   REJECTION:BUFFER$
   s" error[E-DEAD-CODE]: `7` never runs: `execute` before it never returns"
   STR:STARTS-WITH? TTRUE  REJECTION:BUFFER-OFF ;
DP-NAMES-EXECUTE

T-REPORT
