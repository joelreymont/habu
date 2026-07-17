\ rep.f — phantom-preserving register-emitter application (package PTXREP).
\
\ A kernel-token newtype (span / tile / gridctx / uniform / mmracc / …) is a
\ single-cell nominal over one PTX register number. A checked EMIT-* word
\ transforms register numbers ( n … -- n ). These combinators apply such an
\ emitter to kernel tokens while PRESERVING their phantom type: the token's n
\ register flows THROUGH the emitter quotation and the SAME phantom is returned —
\ no strip-and-re-mint, so a wrapper built from them certifies as CHECKED code
\ instead of a TRUSTED: boundary. This is the phantom-preserving-effects
\ capability (dot habu-ptx-phantom-preserving) for the type-PRESERVING op class.
\
\ Forge-safety is by the effect TYPE, enforced by the checker's own unification,
\ not by a body scan: the preserved output var `a` must UNIFY with the preserved
\ input(s), so a wrapper cannot relabel one family as another — `( mmaslice
\ -- mmbslice )` REJECTS because both operands must be one `a`. A wide
\ (multi-cell layout) family cannot bind the single-cell var `a` (kind mismatch
\ REJECTS), and the emitter quotation's arity is checked structurally (arity
\ drift REJECTS). Negatives are pinned in lib/ptx/rep-neg-test.f.
\
\ These combinators are the ONLY trusted boundary they concentrate: the `a<->n`
\ from-register identity inside `q execute` is exactly the coercion the checker
\ cannot express, the codegen analogue of the cg.f *-REG mints. Every pointwise
\ kernel op that PRESERVES its operand's phantom type — and every phantom-CONSUMING
\ store that returns nothing (the SINK* family) — is now a checked caller of one of
\ these combinators, so per-op TRUSTED: wrappers collapse to this core.
\ Load before the tile / collective op vocabularies (lib/ptx/cg.f supplies the
\ EMIT-* leaves the callers quote).

package PTXREP
public

\ The emitter quotation is the top input, so `execute` runs it directly on the
\ operand registers below — no local, no strip-and-re-mint. The `a<->n` coercion
\ this executes across is the concentrated trusted boundary.

\ REP1 ( a [ n -- n ] -- a ) : unary op — a's register through a ( n -- n )
\ emitter; the phantom type is preserved (RELU, EXP., NEG).
TRUSTED: REP1 ( a [ n -- n ] -- a )
   execute ;

\ REP2 ( a a [ n n -- n ] -- a ) : binary op over a SHARED operand type — both
\ operands and the result are the same phantom `a` (element-wise +. -. *. /. ,
\ uniform U/).
TRUSTED: REP2 ( a a [ n n -- n ] -- a )
   execute ;

\ REPMIX2 ( a b [ n n -- n ] -- a ) : binary op preserving the FIRST operand's
\ phantom `a` while consuming a second operand of an independent single-cell type
\ `b` (SCALE: tile * uniform -> tile; B- / B/: tile ∘ uniform -> tile).
TRUSTED: REPMIX2 ( a b [ n n -- n ] -- a )
   execute ;

\ REPMIX3 ( a b c [ n n n -- n ] -- a ) : ternary op preserving the FIRST
\ operand's phantom `a` while consuming two independent single-cell operands
\ `b c` (ACC-FMA: acc ∘ tile ∘ tile -> acc). The output must equal `a`, so it
\ cannot forge a different family; a wide operand cannot bind single-cell `a`
\ (kind); the [ n n n -- n ] quotation pins arity.
TRUSTED: REPMIX3 ( a b c [ n n n -- n ] -- a )
   execute ;

\ REPMIX3B ( a b c [ n n n -- n ] -- b ) : ternary op preserving the SECOND
\ operand's phantom `b` — the FMA-shaped ops whose result phantom is the MIDDLE
\ operand, not the first (FMA.: uniform ∘ tile ∘ tile -> tile; BLOCK-MAX-SELECT:
\ uniform ∘ tile ∘ uniform -> tile). The emitter's needed register order IS the
\ declared operand order, so the wrapper reshuffles nothing and the byte-identity
\ is direct; the output must equal `b`, so it cannot forge the first family.
TRUSTED: REPMIX3B ( a b c [ n n n -- n ] -- b )
   execute ;

\ SINK3 ( a b c [ n n n -- ] -- ) : a ternary phantom-CONSUMING store — three
\ independent single-cell operands `a b c` push their registers through a
\ ( n n n -- ) sink emitter that returns nothing (STORE / STORE-ONCE /
\ SCATTER-ADD / FANIN-SCATTER-ADD / INDEX-DENSE-STORE / ROW-STORE* / SSTORE /
\ STORE-V4 / STORE.V4). The sink returns NO phantom, so it can neither mint nor
\ forge one (an output declaration rejects); a wide operand cannot bind
\ single-cell `a` (kind); the [ n n n -- ] quotation pins arity.
TRUSTED: SINK3 ( a b c [ n n n -- ] -- )
   execute ;

\ SINK4 ( a b c d [ n n n n -- ] -- ) : the 4-operand indexed sink
\ (INDEX-SCATTER-ADD / INDEX-STORE) — the same sink discipline with one more
\ single-cell operand; the [ n n n n -- ] quotation pins the wider arity.
TRUSTED: SINK4 ( a b c d [ n n n n -- ] -- )
   execute ;

\ --- MINTING combinators (dot habu-ptx-phantom-preserving, leg 2b): a checked
\ wrapper REPACKAGES register operands into a NEW register-phantom family whose
\ every type argument is PROJECTED from the operands — no fresh var is minted
\ here, so the fresh-mask CONTEXT mints (GRID-CTX / ROW-CTX / …) stay TRUSTED.
\ Two soundness layers hold forging no easier than a per-op TRUSTED: row:
\   (1) the combinator's declared types PIN the projection, so a wrapper cannot
\       reroute an operand argument — an element<->block relabel REJECTS by
\       unification, exactly as REP2's shared `a` rejects a cross-family relabel;
\   (2) the checked-mint output-provenance seal (src/core/checker.f NP-MINT-CHECK)
\       REJECTS a `:` wrapper that declares an input-unbound output type variable,
\       so the register<->phantom mint cannot leak a free-typed phantom into
\       checked code. The a<->n from-register coercion inside `execute` is the
\       one concentrated trusted boundary, as with REP*/SINK*.
\ MINT-LOAD ( span<s,t,e> gridctx<b,e,m> [ n n -- n ] -- tile<t,b,m> ) : the
\ masked coalesced grid load — element from the span, block+mask from the ctx
\ (LOAD / LOAD-ONCE, space-generic over `s`).
TRUSTED: MINT-LOAD ( span<s,t,e> gridctx<b,e,m> [ n n -- n ] -- tile<t,b,m> )
   execute ;

\ MINT-ROW-SPAN ( matrix<s,t,e,k> rowidx<e> [ n n -- n ] -- span<s,t,k> ) : the
\ one-row slice of a matrix — element from the matrix, column extent `k` carried,
\ row extent `e` consumed by the row index (ROW-SPAN / ROW-SPAN-ONCE).
TRUSTED: MINT-ROW-SPAN ( matrix<s,t,e,k> rowidx<e> [ n n -- n ] -- span<s,t,k> )
   execute ;

\ MINT-ROW-LOAD ( span<s,t,k> rowctx<b,k,m> [ n n -- n ] -- tile<t,b,m> ) : the
\ one-block-per-row load — element from the span, block+mask from the row ctx
\ (ROW-LOAD / ROW-LOAD-ONCE).
TRUSTED: MINT-ROW-LOAD ( span<s,t,k> rowctx<b,k,m> [ n n -- n ] -- tile<t,b,m> )
   execute ;

;package
