\ backend.f - complete native pass descriptors and dispatch.
\
\ Each published row contains its CTARGET descriptor and complete native pass.
\ Registration publishes the row count only after storing both together.
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
require src/compiler/session/lease.f

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

STRUCTURE pass 0 DERIVE addr
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

STRUCTURE row 0 DERIVE addr
   FIELD descriptor CTARGET:backend
   FIELD implementation pass
;STRUCTURE

DEFER-LAYOUT-BUFFER ROWS row
variable ROW-N
1 ROWS-BIND

: BACK-ID ( CTARGET:backend -- CTARGET:backend-id )
   CTARGET-BACKEND:UNMAKE 2drop drop ;

: BACK-ARCH ( CTARGET:backend -- CTARGET:arch )
   CTARGET-BACKEND:UNMAKE 2drop nip ;

: BACK-LOWER ( CTARGET:backend -- [ CTARGET:contract -- bool ] )
   CTARGET-BACKEND:UNMAKE {: id:CTARGET:backend-id a:CTARGET:arch lo em :} lo ;

: BACK-EMIT ( CTARGET:backend -- [ CTARGET:contract -- bool ] )
   CTARGET-BACKEND:UNMAKE {: id:CTARGET:backend-id a:CTARGET:arch lo em :} em ;

: DESCRIPTOR@ ( n -- CTARGET:backend )
   ROWS ROW-DESCRIPTOR @ ;

: PASS-PTR ( n -- ptr NBACK:pass )
   ROWS ROW-IMPLEMENTATION ;

: FIND-ROW ( CTARGET:arch -- n )
   {: a:CTARGET:arch :}
   ROW-N @ 0 ?do
      a i DESCRIPTOR@ BACK-ARCH CTARGET-ARCH:EQ if i unloop exit then
   loop
   -1 ;

: ID-CLAIMED? ( CTARGET:backend-id -- bool )
   {: id:CTARGET:backend-id :}
   ROW-N @ 0 ?do
      id i DESCRIPTOR@ BACK-ID CTARGET-BACKEND--ID:EQ if true unloop exit then
   loop
   false ;

: INSERT-AT ( CTARGET:backend-id -- n )
   CTARGET:ID-CODE {: code:n :}
   ROW-N @ 0 ?do
      code i DESCRIPTOR@ BACK-ID CTARGET:ID-CODE < if i unloop exit then
   loop
   ROW-N @ ;

: SHIFT ( n -- )
   {: at:n :}
   ROW-N @ at - 0 ?do
      ROW-N @ i - 1- ROWS @ ROW-N @ i - ROWS !
   loop ;

\ Whole-row stores leave callback cells undeclared; growth moves old rows.
: REGISTER-XTS ( -- )
   ROW-N @ 1+ 0 ?do
      i ROWS ROW-DESCRIPTOR CTARGET-BACKEND:LOWER dup @ swap xt!
      i ROWS ROW-DESCRIPTOR CTARGET-BACKEND:EMIT dup @ swap xt!
      i PASS-PTR NBACK-PASS:DECLARE dup @ swap xt!
      i PASS-PTR NBACK-PASS:SELECT dup @ swap xt!
      i PASS-PTR NBACK-PASS:PRUNE dup @ swap xt!
      i PASS-PTR NBACK-PASS:FIXPOINT dup @ swap xt!
      i PASS-PTR NBACK-PASS:EMIT dup @ swap xt!
      i PASS-PTR NBACK-PASS:UNPLACED dup @ swap xt!
      i PASS-PTR NBACK-PASS:RELEASE dup @ swap xt!
      i PASS-PTR NBACK-PASS:RETIRE dup @ swap xt!
      i PASS-PTR NBACK-PASS:PROTOTYPE dup @ swap xt!
      i PASS-PTR NBACK-PASS:FORGET dup @ swap xt!
      i PASS-PTR NBACK-PASS:PREPARE dup @ swap xt!
   loop ;

\ NLEASE:WITH keeps admission through validation, movement and publication.
: PUBLISH ( CTARGET:backend NBACK:pass NLEASE:lease -- )
   drop
   {: d:CTARGET:backend p:pass :}
   d BACK-ID CTARGET:ID-CODE 0 <= if E-CTGT-ID throw then
   d BACK-ID ID-CLAIMED? if E-CTGT-REGISTERED throw then
   d BACK-ARCH FIND-ROW 0 >= if E-CTGT-REGISTERED throw then
   d BACK-ID INSERT-AT {: at:n :}
   ROW-N @ 1+ ROWS-GROW
   at SHIFT
   d p ROW-MAKE at ROWS !
   REGISTER-XTS
   ROW-N @ 1+ ROW-N ! ;

public

: COUNT ( -- n ) ROW-N @ ;

: BACKEND@ ( n -- CTARGET:backend )
   dup 0 < over ROW-N @ >= or if E-CTGT-ROW throw then
   DESCRIPTOR@ ;

: REGISTERED? ( CTARGET:arch -- bool ) FIND-ROW 0 >= ;

: ROW ( CTARGET:arch -- n )
   FIND-ROW dup 0 < if E-CTGT-UNLOADED throw then ;

: ID-ROW ( CTARGET:backend-id -- n )
   {: id:CTARGET:backend-id :}
   ROW-N @ 0 ?do
      id i DESCRIPTOR@ BACK-ID CTARGET-BACKEND--ID:EQ if i unloop exit then
   loop
   E-CTGT-UNLOADED throw ;

: REGISTER ( CTARGET:backend NBACK:pass -- )
   [: PUBLISH ;] NLEASE:WITH ;

: LOWERS? ( CTARGET:contract -- bool )
   CTARGET:VALIDATE dup CTARGET:ARCH@ ROW DESCRIPTOR@ BACK-LOWER execute ;

: EMITS? ( CTARGET:contract -- bool )
   CTARGET:VALIDATE dup CTARGET:ARCH@ ROW DESCRIPTOR@ BACK-EMIT execute ;

;package

\ Session identity stays stable as rows move; work resolves the current row.
package NSESSION
public

STRUCTURE session 0
   FIELD ctx IR-CTX:ctx
   FIELD provider CTARGET:backend-id
   FIELD lease NLEASE:lease
;STRUCTURE

private

variable WORK-OWNER

: CTX ( NSESSION:session -- IR-CTX:ctx )
   NSESSION-SESSION:UNMAKE 2drop ;

: LEASE ( NSESSION:session -- NLEASE:lease )
   NSESSION-SESSION:UNMAKE {: c:IR-CTX:ctx id:CTARGET:backend-id l:NLEASE:lease :}
   l ;

: CLEAR ( -- )
   0 WORK-OWNER ! ;

: ENTER ( R NSESSION:session [ R NSESSION:session -- S ] -- S )
   {: body :}
   dup CTX IR-CTX:SERIAL WORK-OWNER !
   body [: CLEAR ;] finally ;

public

: NEW ( IR-CTX:ctx NLEASE:lease -- NSESSION:session )
   {: c:IR-CTX:ctx l:NLEASE:lease :}
   l NLEASE:CHECK
   c IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:ARCH@ NBACK:ROW NBACK:BACKEND@
   CTARGET-BACKEND:UNMAKE {: id:CTARGET:backend-id arch:CTARGET:arch lower emit :}
   c id l NSESSION-SESSION:MAKE ;

: RESOLVE ( NSESSION:session -- IR-CTX:ctx n )
   NSESSION-SESSION:UNMAKE {: c:IR-CTX:ctx id:CTARGET:backend-id l:NLEASE:lease :}
   l NLEASE:WORK-CK
   c IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:ARCH@ {: arch:CTARGET:arch :}
   c IR-CTX:SERIAL WORK-OWNER @ <> if NLEASE:E-STATE throw then
   id NBACK:ID-ROW {: row:n :}
   row NBACK:BACKEND@ CTARGET-BACKEND:UNMAKE
   {: found:CTARGET:backend-id target:CTARGET:arch lower emit :}
   arch target CTARGET-ARCH:EQ 0= if NLEASE:E-STATE throw then
   c row ;

: WITH-WORK ( R NSESSION:session [ R NSESSION:session -- S ] -- S )
   {: body :}
   dup CTX IR-CTX:BINDING@ drop
   dup LEASE {: l:NLEASE:lease :}
   body l [: ENTER ;] NLEASE:WORK ;

;package

package NBACK
private

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
   in out l row PASS-PTR NBACK-PASS:DECLARE @ execute ;

: SELECT ( NSESSION:session IR-BUILD:module -- IR-BUILD:module )
   {: s:NSESSION:session m:IR-BUILD:module :}
   s NSESSION:RESOLVE {: c:IR-CTX:ctx row:n :}
   c m row PASS-PTR NBACK-PASS:SELECT @ execute ;

: PRUNE ( NSESSION:session IR-BUILD:module -- IR-BUILD:module )
   {: s:NSESSION:session m:IR-BUILD:module :}
   s NSESSION:RESOLVE {: c:IR-CTX:ctx row:n :}
   c m row PASS-PTR NBACK-PASS:PRUNE @ execute ;

: FIXPOINT ( NSESSION:session IR-BUILD:module -- IR-BUILD:module )
   {: s:NSESSION:session m:IR-BUILD:module :}
   s NSESSION:RESOLVE {: c:IR-CTX:ctx row:n :}
   c m row PASS-PTR NBACK-PASS:FIXPOINT @ execute ;

\ The slot is the code region's answer, so the backend is never asked where the
\ routine goes.
: EMIT ( NSESSION:session IR-BUILD:module n -- )
   {: s:NSESSION:session m:IR-BUILD:module at:n :}
   s NSESSION:RESOLVE {: c:IR-CTX:ctx row:n :}
   c m at row PASS-PTR NBACK-PASS:EMIT @ execute ;

: EMIT-UNPLACED ( NSESSION:session IR-BUILD:module -- )
   {: s:NSESSION:session m:IR-BUILD:module :}
   s NSESSION:RESOLVE {: c:IR-CTX:ctx row:n :}
   c m row PASS-PTR NBACK-PASS:UNPLACED @ execute ;

: RELEASE ( NSESSION:session -- )
   NSESSION:RESOLVE nip {: row:n :}
   NLOOP:BOUND? if NLOOP:RELEASE then
   row PASS-PTR NBACK-PASS:RELEASE @ execute ;

: RETIRE ( NSESSION:session -- )
   NSESSION:RESOLVE nip PASS-PTR NBACK-PASS:RETIRE @ execute ;

\ ---- what every loaded backend holds -----------------------------------------
: PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key NLEASE:lease -- )
   {: l:NLEASE:lease :}
   l NLEASE:QUIET
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena k:IR-ID:ir-module-key :}
   COUNT 0 ?do
      c a r k  i PASS-PTR NBACK-PASS:PROTOTYPE @ execute
   loop ;

: FORGET ( NLEASE:lease -- )
   NLEASE:QUIET
   COUNT 0 ?do i PASS-PTR NBACK-PASS:FORGET @ execute loop ;

: PREPARE ( -- )
   NLEASE:IDLE-CK
   NLOOP:RESET-SCRATCH
   COUNT 0 ?do i PASS-PTR NBACK-PASS:PREPARE @ execute loop ;

;package
