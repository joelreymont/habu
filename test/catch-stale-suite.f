\ catch-stale-suite.f - the cells a caught throw leaves stale
\ (dot habu-preserve-exceptional-stack-89902bde).
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f test/checker-assert.f
\   test/catch-stale-suite.f
\
\ `catch` restores the DEPTH of both stacks and never their CONTENTS, so a cell
\ in the caught quotation's window - its declared fixed input prefix - is only
\ still worth its input type when EVERY throw path of that body provably left it
\ where it was. Every other window cell is `stale<t>` afterwards: it may be
\ moved, dropped or bound to an untyped local, and any READ of it is
\ E-STALE-READ, named on the token that reads it.
\
\ Every refused row below was ACCEPTED by the engine before this rule
\ (3da80b23) except R4 and R5, which it refused as an ordinary type mismatch
\ without naming the reason; each row says which. Measured: this file fails 16
\ assertions on 3da80b23 and passes on the engine this rule is built into.
\
\ Boundary this rule does NOT cover: the exceptional edge is not part of a
\ quotation's TYPE, so it survives only on a quotation literal. `['] W catch`,
\ `catch` of a quotation parameter, of a typed `xt<effect>` cell and of a
\ `defer` all keep the window typed even when W throws - measured, and the
\ reason R1's twin `['] SWAP-THROW catch` still certifies.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/adt/option.f
require test/checker-assert.f

package CATCH-STALE-TEST

7311 constant E-CS-BOOM
public

\ ---- fixtures the candidate sources below call ------------------------------
variable CS-TICK
: WBOOM ( ptr u8 -- ) drop E-CS-BOOM throw ;                     \ overwrites its input, then throws
: WMAYBE ( ptr u8 -- ptr u8 ) dup c@ 0= IF E-CS-BOOM throw THEN ; \ reads the cell, never writes it
: WKEEP ( ptr u8 -- ptr u8 ) dup c@ drop ;                       \ no throw edge at all
: CLEANMAYBE ( -- ) CS-TICK @ 0= IF E-CS-BOOM throw THEN ;       \ a cleanup that may throw
: FINBODY ( ptr u8 -- ptr u8 ) dup c@ drop ;
: FINW ( ptr u8 -- ptr u8 ) [: FINBODY ;] [: CLEANMAYBE ;] finally ;

\ ---- the refusal shape: a candidate with JSON diagnostics captured ----------
create CS-DBUF 8192 allot
: CS-CODE< ( ptr u8 n -- )   \ check a candidate with JSON diags captured; assert reject
   CS-DBUF 8192 DIAG-BUFFER!  0 0= DIAG-JSON!
   CHECK-CANDIDATE! 0 T= ;
: CS-CODE? ( ptr u8 n -- )   \ assert the captured diagnostic names this text
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;
: CS-CODE-END ( -- ) 0 0= 0= DIAG-JSON! DIAG-BUFFER-OFF ;
: CS-STALE? ( -- ) s\" \"code\":\"E-STALE-READ\"" CS-CODE? ;

\ ---- the two reproducers from the dot ---------------------------------------
\ SWAP-THROW returned (address, 17, code) where its signature said (n, ptr u8,
\ n): the throw path had swapped the two cells and `catch` put back only the
\ depth. RETURN-THROW is the same defect on the return stack.
: CS-SECTION-REPRODUCERS ( -- )
   s" R1 ( n ptr u8 -- n ptr u8 n ) [: swap -99 throw ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   s" R2 ( n | ptr u8 -- n n | ptr u8 ) [: r> swap >r -99 throw ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END ;

\ ---- reading a stale cell ----------------------------------------------------
\ The refusal is named on the token that reads the cell, not on the binding: an
\ untyped local may hold a stale cell.
: CS-SECTION-READS ( -- )
   s" R3 ( ptr u8 -- n ) [: WBOOM ;] catch {: v code:n :} v c@" CS-CODE<
   CS-STALE?  s\" \"token\":\"c@\"" CS-CODE?  CS-CODE-END
   \ arithmetic on the same cell was already refused as a ptr u8 / n mismatch;
   \ now it is named for the reason it is one, on the same token
   s" R4 ( ptr u8 -- n ) [: WBOOM ;] catch {: v code:n :} v 1 +" CS-CODE<
   CS-STALE?  s\" \"token\":\"+\"" CS-CODE?  CS-CODE-END
   \ a TYPED local is itself the read (also refused before this rule, as the
   \ same ordinary mismatch)
   s" R5 ( ptr u8 -- n ) [: WBOOM ;] catch {: v:n code:n :} code" CS-CODE<
   CS-STALE?  CS-CODE-END ;

\ ---- where a throw edge comes from ------------------------------------------
\ A callee that throws counts as overwriting every input it declared: it may
\ rewrite them all before it throws and can reach nothing below them. WMAYBE
\ only READS its input, and the rule still refuses - that conservative edge is
\ this lane's cost, and the evidence a callee could retain is the next one.
: CS-SECTION-EDGES ( -- )
   s" R6 ( ptr u8 -- ptr u8 n ) [: WMAYBE ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ a `finally` cleanup that throws is a throw edge of the word that runs it
   s" R7 ( ptr u8 -- ptr u8 n ) [: FINW ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ a join: the loop needs the cell's type on the back edge, and no refinement
   \ ever un-stales a cell
   s" R8 ( ptr u8 -- ptr u8 ) begin [: WMAYBE ;] catch 0= until" CS-CODE<
   CS-CODE-END
   \ A quotation literal infers its window on a fresh row, so at the edge the
   \ window cell is still an unbound variable and a rewrite with a value of the
   \ same type is not evidence that the cell survived: only identity of the term
   \ is, and identity neither binds the quotation (which would make it
   \ monomorphic where it is merely executed) nor keeps a transient row id.
   \ Migration: bind the cell to a local before the catch, or push the literal
   \ after it.
   s" R9 ( n -- n n ) [: drop 5 -99 throw ;] catch" CS-CODE<
   CS-STALE?  CS-CODE-END ;

\ ---- what the rule still admits ---------------------------------------------
: CS-SECTION-ADMITS ( -- )
   \ window 0: a body that takes nothing has nothing to lose
   s" A1 ( -- n ) [: -99 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" A2 ( -- n ) [: 1 2 3 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" A3 ( -- n ) [: 1 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ the throw path left the window cell exactly where it was
   s" A4 ( ptr u8 -- ptr u8 n ) [: dup c@ drop -99 throw ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ a body with no throw edge records nothing and stales nothing
   s" A5 ( ptr u8 -- ptr u8 n ) [: WKEEP ;] catch" CHECK-QUIET-CANDIDATE! -1 T=
   \ a stale cell may be bound to an untyped local and never read
   s" A6 ( ptr u8 -- ) [: WBOOM ;] catch {: v code:n :}" CHECK-QUIET-CANDIDATE! -1 T=
   \ and it may be moved and dropped
   s" A7 ( ptr u8 ptr u8 -- ) [: WBOOM ;] catch drop swap 2drop" CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- a multicell bundle is ONE stale value ----------------------------------
\ A layout value occupies W hidden physical cells on the row, and `catch` wraps
\ each of them one at a time. Staleness is a property of the VALUE: every
\ question that finds the group sees through the wrapper, so an `option<cspt>`
\ window comes back as one `stale<option<cspt<>>>` that one `drop` removes and
\ one `nip` moves, and every typed use of it - a word input, a typed local, a
\ `MATCH` - is E-STALE-READ. Read as three lone cells instead, two `drop`s left
\ one behind and the third was E-NELAB-UNDER in the native elaborator, which
\ holds the bundle as one value.
PRODUCT cspt 0
   FIELD x n
   FIELD y n
;PRODUCT

: CS-PT ( -- cspt ) 17 25 CATCH--STALE--TEST-CSPT:MAKE ;
: CS-SOME ( -- option<cspt> ) CS-PT OPTION:SOME ;
: CS-BUNDLE-THROW ( option<cspt> -- option<cspt> )   \ overwrites the bundle, then throws
   drop CS-SOME E-CS-BOOM throw ;
: CS-READ-BUNDLE ( option<cspt> -- n )               \ a declared logical input: a read
   MATCH option
      none OF 0 ENDOF
      some OF CATCH--STALE--TEST-CSPT:UNMAKE + ENDOF
   ;MATCH ;
: CS-BUNDLE-RT ( cspt option<cspt> -- n )            \ one drop for the stale bundle, one for the cspt
   [: CS-BUNDLE-THROW ;] catch {: rc:n :}
   drop drop rc ;

: CS-SECTION-BUNDLES ( -- )
   \ the stale bundle is dropped by ONE drop, the cspt below it by the other
   s" B1 ( cspt option<cspt> -- ) [: CS-BUNDLE-THROW ;] catch {: rc:n :} drop drop"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ passing it to a word that declares the logical type reads it, and the
   \ diagnostic names the value, not a hidden field
   s" B2 ( option<cspt> -- n ) [: CS-BUNDLE-THROW ;] catch {: rc:n :} CS-READ-BUNDLE" CS-CODE<
   CS-STALE?  s" stale<option<cspt<>>>" CS-CODE?  CS-CODE-END
   \ a typed local is the same read
   s" B3 ( option<cspt> -- ) [: CS-BUNDLE-THROW ;] catch {: o:option<cspt> rc:n :}" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ and so is a MATCH on the bundle
   s" B4 ( option<cspt> -- n ) [: CS-BUNDLE-THROW ;] catch {: rc:n :} MATCH option none OF 0 ENDOF some OF CATCH--STALE--TEST-CSPT:UNMAKE + ENDOF ;MATCH"
      CS-CODE<  CS-STALE?  CS-CODE-END
   \ an untyped local holds the whole bundle and gives it back stale in every
   \ cell: it still drops in one, and it still refuses to be read
   s" B5 ( option<cspt> -- ) [: CS-BUNDLE-THROW ;] catch {: o rc:n :} o drop"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B6 ( option<cspt> -- n ) [: CS-BUNDLE-THROW ;] catch {: o rc:n :} o CS-READ-BUNDLE" CS-CODE<
   CS-STALE?  CS-CODE-END
   \ transport moves the group whole: `nip` takes the scalar out from under it
   s" B7 ( n option<cspt> -- ) [: CS-BUNDLE-THROW ;] catch {: rc:n :} nip drop"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ B1's shape, run: the drops agree with the checker on the bundle's width
   CS-PT CS-SOME CS-BUNDLE-RT E-CS-BOOM T= ;

\ ---- the intact cell is honest at run time ----------------------------------
\ A4's shape, run: the caught body reads the address and throws, and the catch
\ hands back the SAME address with the throw code on top.
16 BUFFER: CS-BUF
: CS-RT ( ptr u8 -- ptr u8 n ) [: dup c@ drop E-CS-BOOM throw ;] catch ;

: CS-SECTION-RUNTIME ( -- )
   CS-BUF CS-RT {: p code:n :}
   code E-CS-BOOM T=
   p CS-BUF = TTRUE ;

: RUN ( -- )
   T-RESET
   CS-SECTION-REPRODUCERS
   CS-SECTION-READS
   CS-SECTION-EDGES
   CS-SECTION-ADMITS
   CS-SECTION-BUNDLES
   CS-SECTION-RUNTIME
   T-REPORT ;

RUN

;package
