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
\ kernel op that PRESERVES its operand's phantom type is now a checked caller of
\ one of these three, so ~20 per-op TRUSTED: wrappers collapse to this core.
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

;package
