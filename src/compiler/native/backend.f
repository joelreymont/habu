\ backend.f - complete native pass descriptors and dispatch.
\
\ Each published CTARGET descriptor has one pass record at the same sorted
\ runtime row. Registration reserves both persistent tables and fills the pass
\ row before CTARGET publishes its count. The mode records the current legacy
\ provider's exclusive-session constraint for the session driver.
\
\ Definition stages resolve the context's binding. Lifecycle stages visit
\ every published provider; FREEZE folds shared HIR before any selector runs.

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
require src/compiler/session/backend.f

package NBACK
public

\ How control reaches and leaves the routine a definition compiles to. The
\ backend composes its own machine contract from this and from what the
\ definition takes and leaves; which registers or frame that means is its
\ answer, not this file's. It is a one-field nominal over a bit set, so a bare
\ integer cannot be passed where a linkage is wanted and the value stays one
\ cell, which keeps the dispatch linkage small.
STRUCTURE linkage 0
   FIELD bits n
;STRUCTURE

private

$1 constant BIT-DEAD       \ control never comes back
$2 constant BIT-CALLED     \ a call site reaches it
$4 constant BIT-TAIL       \ tail-called from inside the emitted region
$8 constant BIT-BACK       \ it calls back out

: MK ( n -- NBACK:linkage )    NBACK-LINKAGE:MAKE ;
: BITS ( NBACK:linkage -- n )  NBACK-LINKAGE:UNMAKE ;

public

: L-NONE ( -- NBACK:linkage )    0 MK ;
: L-DEAD ( -- NBACK:linkage )    BIT-DEAD MK ;
: L-CALLED ( -- NBACK:linkage )  BIT-CALLED MK ;
: L-TAIL ( -- NBACK:linkage )    BIT-TAIL MK ;
: L-BACK ( -- NBACK:linkage )    BIT-BACK MK ;

: WITH ( NBACK:linkage NBACK:linkage -- NBACK:linkage )
   BITS swap BITS or MK ;

: HAS? ( NBACK:linkage NBACK:linkage -- bool )
   {: set:linkage probe:linkage :}
   probe BITS {: want:n :}
   set BITS want and want = ;

ENUM mode DERIVE eq
   exclusive-session
;ENUM

STRUCTURE pass 0 DERIVE addr
   FIELD mode mode
   FIELD declare [ n n NBACK:linkage -- ]
   FIELD select [ IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ]
   FIELD prune [ IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ]
   FIELD fixpoint [ IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ]
   FIELD emit [ IR-CTX:ctx IR-BUILD:module n -- ]
   FIELD unplaced [ IR-CTX:ctx IR-BUILD:module -- ]
   FIELD release [ -- ]
   FIELD retire [ -- ]
   FIELD prototype [ IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- ]
   FIELD forget [ -- ]
   FIELD prepare [ -- ]
;STRUCTURE

private

DEFER-LAYOUT-BUFFER P-ROWS pass
TYPED-VARIABLE PENDING pass
1 P-ROWS-BIND

: SHIFT ( n -- )
   {: at:n :}
   CTARGET:COUNT at - 0 ?do
      CTARGET:COUNT i - 1- P-ROWS @ CTARGET:COUNT i - P-ROWS !
   loop ;

: INSTALL-AT ( n -- )
   {: at:n :}
   CTARGET:COUNT 1+ P-ROWS-GROW
   at SHIFT
   PENDING @ at P-ROWS ! ;

\ The machine this compilation is for, resolved to its backend's row. The
\ contract is revalidated on the way, so a stage is dispatched only for a
\ declarable target.
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
: FREEZE ( NSESSION:session IR-BUILD:builder -- IR-BUILD:module )
   {: s:NSESSION:session b:IR-BUILD:builder :}
   s NSESSION:RESOLVE drop {: c:IR-CTX:ctx :}
   c b NLOOP:BIND-DIALECT
   c b HIR:ENSURE-VOCABULARY
   c b IR-BUILD:FREEZE-INTERIM {: m:IR-BUILD:module :}
   c m CLOSED ;

\ What the definition takes and leaves, and how control reaches and leaves it.
\ Stated once, before the stages that compile it.
: DECLARE ( NSESSION:session n n NBACK:linkage -- )
   {: s:NSESSION:session in:n out:n l:linkage :}
   s NSESSION:RESOLVE nip {: row:n :}
   in out l row P-ROWS NBACK-PASS:DECLARE @ execute ;

: SELECT ( NSESSION:session IR-BUILD:module -- IR-BUILD:module )
   {: s:NSESSION:session m:IR-BUILD:module :}
   s NSESSION:RESOLVE {: c:IR-CTX:ctx row:n :}
   c m row P-ROWS NBACK-PASS:SELECT @ execute ;

: PRUNE ( NSESSION:session IR-BUILD:module -- IR-BUILD:module )
   {: s:NSESSION:session m:IR-BUILD:module :}
   s NSESSION:RESOLVE {: c:IR-CTX:ctx row:n :}
   c m row P-ROWS NBACK-PASS:PRUNE @ execute ;

: FIXPOINT ( NSESSION:session IR-BUILD:module -- IR-BUILD:module )
   {: s:NSESSION:session m:IR-BUILD:module :}
   s NSESSION:RESOLVE {: c:IR-CTX:ctx row:n :}
   c m row P-ROWS NBACK-PASS:FIXPOINT @ execute ;

\ The slot is the code region's answer, so the backend is never asked where the
\ routine goes.
: EMIT ( NSESSION:session IR-BUILD:module n -- )
   {: s:NSESSION:session m:IR-BUILD:module at:n :}
   s NSESSION:RESOLVE {: c:IR-CTX:ctx row:n :}
   c m at row P-ROWS NBACK-PASS:EMIT @ execute ;

: EMIT-UNPLACED ( NSESSION:session IR-BUILD:module -- )
   {: s:NSESSION:session m:IR-BUILD:module :}
   s NSESSION:RESOLVE {: c:IR-CTX:ctx row:n :}
   c m row P-ROWS NBACK-PASS:UNPLACED @ execute ;

: RELEASE ( NSESSION:session -- )
   NSESSION:RESOLVE nip {: row:n :}
   NLOOP:BOUND? if NLOOP:RELEASE then
   row P-ROWS NBACK-PASS:RELEASE @ execute ;

: RETIRE ( NSESSION:session -- )
   NSESSION:RESOLVE nip P-ROWS NBACK-PASS:RETIRE @ execute ;

\ ---- what every loaded backend holds -----------------------------------------
: PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key NLEASE:lease -- )
   {: l:NLEASE:lease :}
   l NLEASE:QUIET
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena k:IR-ID:ir-module-key :}
   CTARGET:COUNT 0 ?do
      c a r k  i P-ROWS NBACK-PASS:PROTOTYPE @ execute
   loop ;

: FORGET ( NLEASE:lease -- )
   NLEASE:QUIET
   CTARGET:COUNT 0 ?do i P-ROWS NBACK-PASS:FORGET @ execute loop ;

: PREPARE ( -- )
   NLEASE:IDLE-CK
   NLOOP:RESET-SCRATCH
   CTARGET:COUNT 0 ?do i P-ROWS NBACK-PASS:PREPARE @ execute loop ;

\ The whole pass record is installed before CTARGET publishes the provider.
: REGISTER ( CTARGET:backend NBACK:pass -- )
   {: d:CTARGET:backend p:pass :}
   NLEASE:IDLE-CK
   p PENDING !
   d [: INSTALL-AT ;] CTARGET:REGISTER ;

: MODE@ ( CTARGET:arch -- NBACK:mode )
   CTARGET:ROW P-ROWS NBACK-PASS:MODE @ ;

;package
