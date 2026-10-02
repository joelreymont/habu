\ backend.f - the pass rows a backend installs beside its registry row.
\
\ WHAT IT IS FOR. src/compiler/target.f answers WHETHER a loaded backend serves a
\ contract; this table holds WHAT it runs. The native driver
\ (src/compiler/native/compiler.f) therefore names no backend package: it asks
\ here for the row the compilation's own target contract resolves to, so an
\ engine carries the backends its sources required and no others.
\
\ THE KEY IS THE REGISTRY'S. A row is `<arch> CTARGET:ROW`, the index the
\ registry's own predicate rows use, so there is no second key and no second
\ answer to "is this backend loaded".
\
\ EVERY ROW ANSWERS, INCLUDING ONE NOBODY FILLED. The rows are given defaults as
\ this file loads. A per-definition stage refuses with E-CTGT-UNLOADED - the
\ refusal an architecture with no backend already gets - so a module that took a
\ registry row and installed no passes is refused by name instead of reaching an
\ empty cell. The lifecycle rows do nothing by default: a backend that interned
\ no dialect and reserved no scratch has nothing to give back.
\
\ WHICH ROWS TAKE A CONTEXT. A definition's stages resolve through the context's
\ own binding, so each reaches the backend for the machine that definition
\ compiles to. The session and capture stages have no context to ask - a
\ stand-down runs as its context dies and an image capture runs outside every
\ context - so they run over the whole table and each loaded backend gives back
\ what it holds.
\
\ ONE MODULE FEEDS EVERY SELECTOR. FREEZE is the stage before selection and the
\ one stage that is no backend's: it freezes the definition's HIR module, folds
\ its loops (which reads no machine) and hands back the module every selector
\ binds, so a second target's emission selects from the very module the first
\ one did. The driver that froze it retires it after its last selector. The
\ fold's binding and scratch are therefore this file's to give back as well:
\ RELEASE and PREPARE do it before they dispatch, because FREEZE takes the
\ binding before any row runs and a row's own RELEASE need not know the pass.
\
\ AN EMISSION MEASURED FROM NO SLOT. EMIT writes the routine for the code slot
\ the driver names; EMIT-UNPLACED writes it for none, so every site that leaves
\ it is a row and its field is a linker's to write. It is the emission a shadow
\ target records (src/compiler/native/shadow.f), and a backend with no such
\ emission refuses it the way an unfilled row does.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/arena.f
require src/compiler/ir/context.f
require src/compiler/ir/build.f
require src/compiler/native/hir.f
require src/compiler/native/loop.f

package NBACK
public

\ How control reaches and leaves the routine a definition compiles to. The
\ backend composes its own machine contract from this and from what the
\ definition takes and leaves; which registers or frame that means is its
\ answer, not this file's. It is a one-field nominal over a bit set, so a bare
\ integer cannot be passed where a linkage is wanted and the value stays one
\ cell: a multi-field value cannot be bound to a local, and the dispatch below
\ has to bind one.
STRUCTURE linkage 0
   FIELD bits n
;STRUCTURE

private

$1 constant BIT-DEAD       \ control never comes back
$2 constant BIT-CALLED     \ a call site reaches it
$4 constant BIT-TAIL       \ tail-called from inside the emitted region
$8 constant BIT-BACK       \ it calls back out
$10 constant BIT-PATCH     \ its return is the slot `does>` patches

: MK ( n -- NBACK:linkage )    NBACK-LINKAGE:MAKE ;
: BITS ( NBACK:linkage -- n )  NBACK-LINKAGE:UNMAKE ;

public

: L-NONE ( -- NBACK:linkage )    0 MK ;
: L-DEAD ( -- NBACK:linkage )    BIT-DEAD MK ;
: L-CALLED ( -- NBACK:linkage )  BIT-CALLED MK ;
: L-TAIL ( -- NBACK:linkage )    BIT-TAIL MK ;
: L-BACK ( -- NBACK:linkage )    BIT-BACK MK ;

\ A created word's body (NCOMP:COMPILE-FIXED): `does-patch` turns its return
\ into a branch to the clause. A backend whose return is a patchable slot as
\ it stands ignores it; one whose return is too short for a branch writes the
\ slot in front of it.
: L-PATCH ( -- NBACK:linkage )   BIT-PATCH MK ;

: WITH ( NBACK:linkage NBACK:linkage -- NBACK:linkage )
   BITS swap BITS or MK ;

: HAS? ( NBACK:linkage NBACK:linkage -- bool )
   {: set:linkage probe:linkage :}
   probe BITS {: want:n :}
   set BITS want and want = ;

private

\ ---- the rows ----------------------------------------------------------------
\ Every table is sized and indexed exactly like the registry's own rows.
CTARGET:BACKEND-ROWS TYPED-BUFFER P-DECLARE [ n n NBACK:linkage -- ]
CTARGET:BACKEND-ROWS TYPED-BUFFER P-SELECT [ IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ]
CTARGET:BACKEND-ROWS TYPED-BUFFER P-PRUNE [ IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ]
CTARGET:BACKEND-ROWS TYPED-BUFFER P-FIXPOINT [ IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ]
CTARGET:BACKEND-ROWS TYPED-BUFFER P-EMIT [ IR-CTX:ctx IR-BUILD:module n -- ]
CTARGET:BACKEND-ROWS TYPED-BUFFER P-UNPLACED [ IR-CTX:ctx IR-BUILD:module -- ]
CTARGET:BACKEND-ROWS TYPED-BUFFER P-RELEASE [ -- ]
CTARGET:BACKEND-ROWS TYPED-BUFFER P-RETIRE [ -- ]
CTARGET:BACKEND-ROWS TYPED-BUFFER P-PROTOTYPE [ IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- ]
CTARGET:BACKEND-ROWS TYPED-BUFFER P-FORGET [ -- ]
CTARGET:BACKEND-ROWS TYPED-BUFFER P-PREPARE [ -- ]

\ ---- what an unfilled row answers --------------------------------------------
: NO-DECLARE ( n n NBACK:linkage -- )
   E-CTGT-UNLOADED throw ;

: NO-REWRITE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   E-CTGT-UNLOADED throw ;

: NO-EMIT ( IR-CTX:ctx IR-BUILD:module n -- )
   E-CTGT-UNLOADED throw ;

: NO-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- )
   E-CTGT-UNLOADED throw ;

: NO-STAGE ( -- )
   E-CTGT-UNLOADED throw ;

: NO-PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   2drop 2drop ;

: NO-LIFECYCLE ( -- ) ;

: DEFAULTS ( -- )
   CTARGET:BACKEND-ROWS 0 ?do
      [: NO-DECLARE ;] i P-DECLARE !
      [: NO-REWRITE ;] i P-SELECT !
      [: NO-REWRITE ;] i P-PRUNE !
      [: NO-REWRITE ;] i P-FIXPOINT !
      [: NO-EMIT ;] i P-EMIT !
      [: NO-UNPLACED ;] i P-UNPLACED !
      [: NO-STAGE ;] i P-RELEASE !
      [: NO-STAGE ;] i P-RETIRE !
      [: NO-PROTOTYPE ;] i P-PROTOTYPE !
      [: NO-LIFECYCLE ;] i P-FORGET !
      [: NO-LIFECYCLE ;] i P-PREPARE !
   loop ;

DEFAULTS

\ The machine this compilation is for, resolved to its backend's row. The
\ contract is revalidated on the way, so a stage is dispatched only for a
\ declarable target.
: ROW@ ( IR-CTX:ctx -- n )
   IR-CTX:BINDING@ CBIND:VALIDATE CBIND:TARGET@ CTARGET:ARCH@ CTARGET:ROW ;

\ ---- the module every selector reads ----------------------------------------
: HIR-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER ;

\ A module with no such loop is handed back UNTOUCHED: rebuilding renumbers
\ values, so a routine that gained nothing could still come out with other bytes.
: CLOSED ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   m NLOOP:FOLDS {: n:n :}
   n 0= if NLOOP:RELEASE m exit then
   c HIR-BUILDER {: nb:IR-BUILD:builder :}
   c nb HIR:ENSURE-VOCABULARY
   c m nb NLOOP:REWRITE {: m1:IR-BUILD:module :}
   NLOOP:FOLDED n <> if E-NLOOP-PLAN throw then
   m IR-BUILD:RETIRE
   m1 ;

public

\ ---- the stages of one definition --------------------------------------------
\ The HIR module every selector of this definition binds, frozen interim with
\ its loops folded. IT IS FROZEN INTERIM because it is never the module a
\ compilation emits: selection reads it and writes a machine module, and that
\ one is verified whole, so this freeze derives the edge table selection reads
\ and leaves the checking to the freeze of the module that becomes the routine.
: FREEZE ( IR-CTX:ctx IR-BUILD:builder -- IR-BUILD:module )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b NLOOP:BIND-DIALECT
   c b HIR:ENSURE-VOCABULARY
   c b IR-BUILD:FREEZE-INTERIM {: m:IR-BUILD:module :}
   c m CLOSED ;

\ What the definition takes and leaves, and how control reaches and leaves it.
\ Stated once, before the stages that compile it.
: DECLARE ( IR-CTX:ctx n n NBACK:linkage -- )
   {: c:IR-CTX:ctx in:n out:n l:linkage :}
   in out l  c ROW@ P-DECLARE @ execute ;

: SELECT ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   c m  c ROW@ P-SELECT @ execute ;

: PRUNE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   c m  c ROW@ P-PRUNE @ execute ;

: FIXPOINT ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   c m  c ROW@ P-FIXPOINT @ execute ;

\ The slot is the code region's answer, so the backend is never asked where the
\ routine goes.
: EMIT ( IR-CTX:ctx IR-BUILD:module n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module at:n :}
   c m at  c ROW@ P-EMIT @ execute ;

: EMIT-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   c m  c ROW@ P-UNPLACED @ execute ;

: RELEASE ( IR-CTX:ctx -- )
   NLOOP:BOUND? if NLOOP:RELEASE then
   ROW@ P-RELEASE @ execute ;

: RETIRE ( IR-CTX:ctx -- )
   ROW@ P-RETIRE @ execute ;

\ ---- what every loaded backend holds -----------------------------------------
: PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena k:IR-ID:ir-module-key :}
   CTARGET:BACKEND-ROWS 0 ?do
      c a r k  i P-PROTOTYPE @ execute
   loop ;

: FORGET ( -- )
   CTARGET:BACKEND-ROWS 0 ?do i P-FORGET @ execute loop ;

: PREPARE ( -- )
   NLOOP:RESET-SCRATCH
   CTARGET:BACKEND-ROWS 0 ?do i P-PREPARE @ execute loop ;

\ ---- what a backend installs as it loads -------------------------------------
\ One act per row: a backend that installs some and not others refuses at the
\ stages it left, rather than reaching an empty cell at any of them.
: DECLARE! ( CTARGET:arch [ n n NBACK:linkage -- ] -- )
   {: a:CTARGET:arch q :}
   q a CTARGET:ROW P-DECLARE ! ;

: SELECT! ( CTARGET:arch [ IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ] -- )
   {: a:CTARGET:arch q :}
   q a CTARGET:ROW P-SELECT ! ;

: PRUNE! ( CTARGET:arch [ IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ] -- )
   {: a:CTARGET:arch q :}
   q a CTARGET:ROW P-PRUNE ! ;

: FIXPOINT! ( CTARGET:arch [ IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ] -- )
   {: a:CTARGET:arch q :}
   q a CTARGET:ROW P-FIXPOINT ! ;

: EMIT! ( CTARGET:arch [ IR-CTX:ctx IR-BUILD:module n -- ] -- )
   {: a:CTARGET:arch q :}
   q a CTARGET:ROW P-EMIT ! ;

: EMIT-UNPLACED! ( CTARGET:arch [ IR-CTX:ctx IR-BUILD:module -- ] -- )
   {: a:CTARGET:arch q :}
   q a CTARGET:ROW P-UNPLACED ! ;

: RELEASE! ( CTARGET:arch [ -- ] -- )
   {: a:CTARGET:arch q :}
   q a CTARGET:ROW P-RELEASE ! ;

: RETIRE! ( CTARGET:arch [ -- ] -- )
   {: a:CTARGET:arch q :}
   q a CTARGET:ROW P-RETIRE ! ;

: PROTOTYPE! ( CTARGET:arch [ IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- ] -- )
   {: a:CTARGET:arch q :}
   q a CTARGET:ROW P-PROTOTYPE ! ;

: FORGET! ( CTARGET:arch [ -- ] -- )
   {: a:CTARGET:arch q :}
   q a CTARGET:ROW P-FORGET ! ;

: PREPARE! ( CTARGET:arch [ -- ] -- )
   {: a:CTARGET:arch q :}
   q a CTARGET:ROW P-PREPARE ! ;

;package
