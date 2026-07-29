\ checker-model-proof.f - the checker model parity gate.
\
\ `formal/Common/Effects.v` and `formal/Common/Control.v` are, together, the
\ largest body of proof in this tree and the only machine-checked description of
\ `src/core/checker.f`: the type vocabulary, row-polymorphic stack effects, the
\ widening lattice, quotation effects and arity-n type families in the first,
\ and branches, both loop shapes, `case`, the `MATCH` eliminator, early return,
\ the throw edge, dead paths, recursion, quotation application, branch-scoped
\ locals and the linear-once conservation pass in the second. Every other model
\ in the tree is bound to its source by a gate. Until this file existed these two
\ were not, so they could drift away from the checker with nothing saying so -
\ which is exactly the failure the parity gates exist to prevent.
\
\ One artifact, two readers. `test/compiler/checker-model-schema.f` holds the
\ frozen tables: the concrete type vocabulary, the class and sign vocabularies,
\ the term tags, the control-flow dispatch table, the control frame kinds, and
\ the shared program vectors. `test/compiler/checker-model-cases.f` asks the
\ shipped checker about them and
\ `test/compiler/checker-model-obligations.f` asks Rocq about the same rows.
\ Neither side carries a copy.
\
\ What the gate refuses:
\
\   - a type ADDED to the checker's concrete vocabulary and not to the model,
\     or removed from it, or given a different code, class, width or sign on
\     either side, through the token-by-token walk over `CT-INIT`'s body and
\     the generated obligations over `con_code` / `con_cls` / `con_width` /
\     `con_sgn`. The walk demands the body is exhausted when the frozen rows
\     are, so an extra row is caught as surely as a missing one, and the type
\     NAMES are read out of the string literals themselves;
\   - a type added to the MODEL and not to the checker, through the generated
\     exhaustive match over `Effects.con`: a constructor nobody wrote a row for
\     leaves that match non-exhaustive and Rocq refuses the file;
\   - a term tag added on either side, through the same pair of devices over
\     `T-CON` .. `T-PARAM`, `S-ROW` / `S-PUSH` and `Effects.ty` /
\     `Effects.stack`;
\   - a control opener or closer added to, removed from, reordered in, or
\     rewired inside `CF-TOK?`, through the token-by-token walk over its body,
\     and a `Control.tok` constructor added or removed, through the generated
\     exhaustive match over it;
\   - a control frame kind that moves, changes number, or stops being written by
\     the construct that owns it, through the frozen source run read out of that
\     construct's own body and the generated obligation that runs the model to
\     the same frame;
\   - a program the two machines answer differently, through the shared vectors:
\     each is handed to the real `CHECK-QUIET-CANDIDATE!` and turned into a
\     `check_ctl` obligation, and the row's ONE verdict is what both must give;
\   - a rewritten statement, through the statement each manifest row pins. The
\     row's type is ascribed to a generated definition whose body is the result,
\     so Rocq itself has to accept the proved statement as the one the committed
\     manifest wrote down;
\   - an unbound result, a manifest row for a result that no longer exists, or an
\     `Admitted`, through the structural inventory of what the two model files
\     declare;
\   - ANY assumption at all. Both models rest on nothing, so this gate refuses an
\     assumption twice over: the manifest is read with assumption rows forbidden,
\     so one cannot be written down, and every statement must additionally report
\     "Closed under the global context" with no `Axioms:` header anywhere in what
\     Rocq printed.
\
\ What it does NOT refuse, stated plainly so nobody reads more into a green run
\ than is there. The models cover a FRAGMENT, and both name their omissions in
\ their own headers - `T-ATOM`'s rigid host identities, VALUE-RECORD field
\ coercion, the transport ops and the generated-accessor window, field
\ projection, block-uniform branches, and `MATCH`'s scrutinee pop. This
\ gate holds the modelled fragment to the checker; it cannot notice a change
\ inside an unmodelled one. It also compares BEHAVIOUR only on the frozen
\ vectors: two machines that agree on those twenty-six programs may still
\ disagree on the twenty-seventh, and only a soundness proof - which neither
\ model states yet - would close that. That is not a small caveat. Both models
\ publish more than a hundred concrete results, and only the decisions a vector
\ reaches are actually held to the checker; the rest are held to the reader's
\ care in keeping the model faithful. Measured: halving `CF-PUSH`'s frame
\ ceiling, letting `INT-WIDENS?` pass any same-class pair, lowering or deleting
\ `MATCH`'s depth guard, turning the per-step linear conservation count into a
\ no-op, and dropping the unterminated-`construct` test from `CHECK`'s open-form
\ check each left this gate green before the vectors that reach those five
\ decisions were added. Growing the vector table is what closes that, one
\ decision at a time.
\
\ Focused command: `bin/hb --load test/compiler/checker-model-proof.f`. The gate
\ compiles the models itself, so nothing has to be built first. It shells out to
\ `rocq` with no capability probe, exactly as the identity, interning, structure
\ and storage gates do, which is why it runs in the standalone stdlib gate and is
\ documented a manual gate rather than mirrored into the resident fast tier.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test.f
require lib/test/outcome.f
require test/compiler/proof-manifest.f
require test/compiler/rocq-run.f
require test/compiler/checker-model-cases.f
require test/compiler/checker-model-obligations.f

package CHECKER-MODEL-GATE
using CHECKER-MODEL-PROOF
private

$100 constant PATH-CAP
$2E constant DOT

create VPATH PATH-CAP allot

variable VPATH-U
variable DECL-N
variable ADMITTED-N
variable MODEL-I

: MANIFEST-PATH$ ( -- ptr u8 n )
   s" test/compiler/checker-model-axioms.txt" ;

\ ---- the manifest, read with assumptions forbidden ---------------------------

\ A malformed manifest, or one that tries to claim an assumption, is refused when
\ it is read. That refusal is caught here only so the run says which rule the
\ committed file broke before it stops; the code is rethrown, because every later
\ phase reads the manifest and none of them can mean anything without it.
: READ-MANIFEST ( -- )
   false PROOF-MANIFEST:AXIOMS-ALLOWED!
   [: MANIFEST-PATH$ PROOF-MANIFEST:READ ;] catch {: rc:n :}
   s" the committed manifest is well formed and claims no assumption" T-LABEL
   rc 0 T=
   rc 0 <> if rc throw then ;

\ The grammar's shape rules belong to `package PROOF-MANIFEST` and are proved by
\ the identity gate. What this gate owns is the strength of its assumption
\ claim, so these fixtures are about that and nothing else.

: HOSTILE-AXIOM-ROW ( -- )
   s" an assumption row cannot be written into a manifest held at empty" T-LABEL
   [: false PROOF-MANIFEST:AXIOMS-ALLOWED!
      S\" theorem Habu.Common.Effects.arity_mismatch_rejects\ntype True\naxiom host_atomic_cas : cas_impl"
      PROOF-MANIFEST:TEXT ;]
      E-CID-AXIOM TTHROWSQ ;

: HOSTILE-AXIOM-AFTER-CLOSED ( -- )
   s" an assumption row after a closed row is refused just the same" T-LABEL
   [: false PROOF-MANIFEST:AXIOMS-ALLOWED!
      S\" theorem Habu.Common.Control.branch_arms_join\ntype True\nclosed\naxiom whatever : Prop"
      PROOF-MANIFEST:TEXT ;]
      E-CID-AXIOM TTHROWSQ ;

: MANIFEST-CLOSED-PAIR ( -- )
   false PROOF-MANIFEST:AXIOMS-ALLOWED!
   S\" theorem Habu.Common.Control.branch_arms_join\n# a note\ntype True\nclosed"
   PROOF-MANIFEST:TEXT
   s" a manifest held at empty still reads a result and its statement" T-LABEL
   PROOF-MANIFEST:THEOREMS 1 T=
   s" the pinned statement is exactly the type row's text" T-LABEL
   0 PROOF-MANIFEST:TYPE$ s" True" T$= ;

public

: PHASE-MANIFEST ( -- )
   HOSTILE-AXIOM-ROW
   HOSTILE-AXIOM-AFTER-CLOSED
   MANIFEST-CLOSED-PAIR ;

private

\ ---- what the model files actually declare -----------------------------------
\ Read structurally through the shared source lexer. A Rocq name carries the
\ trailing period of the command it ends when the command is written on one
\ line, so it is stripped here rather than assumed away. The two files are walked
\ in the order `formal/_CoqProject` builds them, against one manifest, because
\ Control.v imports Effects.v and the two are one model.

: STRIP-DOT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0= if a u exit then
   a u 1- + c@ DOT = if a u 1- exit then
   a u ;

: TOK$ ( n -- ptr u8 n )
   COMPILER-ID-SRC:TOKEN$ ;

: NAME-AT$ ( n -- ptr u8 n )
   TOK$ STRIP-DOT$ ;

: TOK-IS? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k TOK$ a u STR= ;

\ `Example` is `Definition` with a proof script, so a published `Example` closed
\ with `Qed.` is a constant of exactly its stated type and is pinned the same way
\ a `Theorem` is. Both spellings are recognised, so moving a result from one to
\ the other is not a way out of the manifest.
: DECL-HEAD? ( n -- bool ) {: k:n :}
   k s" Theorem" TOK-IS? if true exit then
   k s" Corollary" TOK-IS? if true exit then
   k s" Example" TOK-IS? ;

: ADMITTED-TOKEN? ( n -- bool ) {: k:n :}
   k s" Admitted" TOK-IS? if true exit then
   k s" Admitted." TOK-IS? if true exit then
   k s" admit" TOK-IS? ;

: QUALIFIED$ ( n -- ptr u8 n ) {: k:n :}
   SB-RESET
   MODEL-I @ MODEL-PREFIX$ SB-APPEND
   k NAME-AT$ SB-APPEND
   SB$ ;

: DECL-ROW ( n -- ) {: k:n :}
   s" every published result is bound by the committed manifest" T-LABEL
   DECL-N @ PROOF-MANIFEST:THEOREMS < TTRUE
   DECL-N @ PROOF-MANIFEST:THEOREMS < if
      s" the manifest binds that result, at that position" T-LABEL
      k 1+ QUALIFIED$ DECL-N @ PROOF-MANIFEST:THEOREM$ T$=
   then
   DECL-N @ 1+ DECL-N ! ;

: WALK-TOKEN ( n -- ) {: k:n :}
   k ADMITTED-TOKEN? if ADMITTED-N @ 1+ ADMITTED-N ! then
   k DECL-HEAD? if k DECL-ROW then ;

: WALK-MODEL ( n -- ) {: m:n :}
   m MODEL-I !
   m MODEL-FILE$ COMPILER-ID-SRC:SCAN-FILE
   COMPILER-ID-SRC:TOKENS 0 ?do i WALK-TOKEN loop ;

public

: PHASE-DECLARATIONS ( -- )
   READ-MANIFEST
   0 DECL-N !
   0 ADMITTED-N !
   MODELS 0 ?do i WALK-MODEL loop
   s" the manifest names every published result and no others" T-LABEL
   DECL-N @ PROOF-MANIFEST:THEOREMS T=
   s" every named result also has its statement pinned" T-LABEL
   PROOF-MANIFEST:TYPES PROOF-MANIFEST:THEOREMS T=
   s" no statement in the checker models is admitted" T-LABEL
   ADMITTED-N @ 0 T= ;

private

\ ---- making Rocq answer ------------------------------------------------------

: SCRATCH-DIR ( -- ptr u8 n )
   s" habu-checker-model-parity" TMPDIR-MKDIR ;

: OBLIGATION-ROW ( n -- ) {: k:n :}
   k PROOF-MANIFEST:THEOREM$ k PROOF-MANIFEST:TYPE$ CHECKER-MODEL-ROCQ:STATEMENT+
   k PROOF-MANIFEST:THEOREM$ CHECKER-MODEL-ROCQ:ASSUMPTION+ ;

: EMIT-OBLIGATIONS ( -- )
   CHECKER-MODEL-ROCQ:START
   PROOF-MANIFEST:THEOREMS 0 ?do i OBLIGATION-ROW loop ;

: WRITE-OBLIGATIONS ( ptr u8 n -- ) {: dir:ptr diru:n :}
   dir diru s" CheckerModelParity.v" VPATH JOIN-PATH VPATH-U !
   VPATH VPATH-U @ CHECKER-MODEL-ROCQ:TEXT$ ATOMIC-WRITE-FILE ;

: COMPILE-MODEL ( n -- ) {: m:n :}
   s" the checker model compiles" T-LABEL
   m MODEL-FILE$ ROCQ-CMD:COMPILE ;

public

: PHASE-ROCQ ( -- )
   MODELS 0 ?do i COMPILE-MODEL loop
   EMIT-OBLIGATIONS
   CLEANUP-RESET
   SCRATCH-DIR 2dup CLEANUP-TREE+ WRITE-OBLIGATIONS
   s" every generated checker-model obligation is proved" T-LABEL
   VPATH VPATH-U @ ROCQ-CMD:COMPILE
   ROCQ-CMD:OUT$ PROOF-MANIFEST:RENDER
   CLEANUP-RUN ;

\ ---- the assumption set, held at empty ---------------------------------------

: PHASE-ASSUMPTIONS ( -- )
   s" the reported assumption set is the committed manifest, entire" T-LABEL
   PROOF-MANIFEST:GOT$ PROOF-MANIFEST:WANT$ T$=
   s" every published result reports closed under the global context" T-LABEL
   PROOF-MANIFEST:CLOSED PROOF-MANIFEST:THEOREMS T=
   s" no result rests on an external assumption" T-LABEL
   PROOF-MANIFEST:BEARING 0 T=
   s" the whole external assumption set is empty" T-LABEL
   PROOF-MANIFEST:AXIOM-COUNT 0 T= ;

: RUN ( -- )
   T-RESET
   CHECKER-MODEL-CASES:HABU-SIDE
   PHASE-MANIFEST
   PHASE-DECLARATIONS
   PHASE-ROCQ
   PHASE-ASSUMPTIONS
   T-REPORT ;

;using
;package

\ The gate runs INSIDE the package that declares the vectors' sum families.
\ `construct` resolves its family in the active package only
\ (`TFAM-CONSTRUCT-FAM`, src/core/type-family.f), so a construct vector can only
\ be asked where the family is owned: asked from top level the very same text is
\ refused for a reason that has nothing to do with the rule under test. No other
\ row here depends on the package, and every one of them answers the same either
\ way; this is the one question that has a place to be asked from.
package CHECKER-MODEL-CASES
CHECKER-MODEL-GATE:RUN
;package
