\ passes.f - the ARM64 backend's pass rows in the compiler's pass table.
\
\ WHAT IT IS FOR. src/compiler/native/compiler.f drives a compilation without
\ naming a backend: each stage goes through the row that the definition's own
\ target contract resolves to. This file is that row for ARM64. Every stage is
\ the sequence the driver used to run by name, wrapping A64SEL, A64PRUNE, A64RA,
\ A64RAV, A64SPILL and A64EMIT; none of their state moved here.
\
\ WHAT IT OWNS. The routine contract this definition compiles to. Two of its
\ facts are this backend's own readings - how many functions share the contract
\ (IR-BUILD:FUNS, read at selection) and how many spill slots the allocator
\ settled on (A64RA:FRAME, read at each lowering turn) - and the other two are
\ declared by the driver. ROUTINE is the only reader of all four, which is why
\ they sit together here rather than in a driver that names no machine.
\
\ HIR LOOP FOLDING TRAVELS WITH SELECTION, not because it is ARM64's, but because
\ the fold rewrites the module the selector is bound to as its source: the
\ binding has to be taken and re-made around it. A second backend reuses
\ src/compiler/native/loop.f the same way.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/ir/id.f
require src/compiler/ir/arena.f
require src/compiler/ir/context.f
require src/compiler/ir/build.f
require src/compiler/native/backend.f
require src/compiler/native/abi.f
require src/compiler/native/frame.f
require src/compiler/native/a64ir.f
require src/compiler/native/hir.f
require src/compiler/native/loop.f
require src/compiler/native/select.f
require src/compiler/native/prune.f
require src/compiler/native/spill.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/compiler/native/emit.f
require src/compiler/native/prof.f
require src/arch/arm64/backend.f

package A64PASS
private

\ ---- the routine contract this definition compiles to ------------------------
variable D-IN                        \ cells the definition takes
variable D-OUT                       \ cells it leaves
variable D-DEAD                      \ control never comes back
variable D-CALLED                    \ a call site reaches it
variable D-TAIL                      \ tail-called from inside the emitted region
variable D-BACK                      \ it calls back out
variable D-FUNS                      \ functions sharing the emitted routine contract
variable D-SPILLS                    \ padded spill slots that define the cumulative frame

\ The driver's first stage: what this definition takes and leaves, and how
\ control reaches and leaves it. The two counts this backend reads for itself
\ start again here, so a stage can only see the definition it is compiling.
: DECLARE ( n n NBACK:linkage -- )
   {: in:n out:n l:NBACK:linkage :}
   in D-IN !  out D-OUT !
   l NBACK:L-DEAD NBACK:HAS? D-DEAD !
   l NBACK:L-CALLED NBACK:HAS? D-CALLED !
   l NBACK:L-TAIL NBACK:HAS? D-TAIL !
   l NBACK:L-BACK NBACK:HAS? D-BACK !
   0 D-FUNS !
   0 D-SPILLS ! ;

\ All functions share this ABI. No-return and tail-call control describe a
\ single function; quotation siblings must retain their ordinary returns.
: ROUTINE ( -- NEFF:routine )
   D-DEAD @ 0<> D-FUNS @ 1 = and if
      D-CALLED @ 0<> if
         NABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ NABI:NORET-FRAMED exit
      then
      NABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ NABI:NORET-LEAF-FRAMED exit
   then
   D-TAIL @ 0<> D-FUNS @ 1 = and if
      D-BACK @ 0<> if
         NABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ NABI:TAIL-CALLING-FRAMED exit
      then
      NABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ NABI:TAIL-FRAMED exit
   then
   D-CALLED @ 0<> if
      NABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ NABI:CALL-FRAMED exit
   then
   NABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ NABI:LEAF-FRAMED ;

: A64-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER ;

: HIR-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER ;

\ ---- selection ---------------------------------------------------------------
\ A module with no such loop is handed back UNTOUCHED: rebuilding renumbers
\ values, so a routine that gained nothing could still come out with other bytes.
: CLOSED ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   m NLOOP:FOLDS {: n:n :}
   n 0= if NLOOP:RELEASE m exit then
   A64SEL:RELEASE
   c HIR-BUILDER {: nb:IR-BUILD:builder :}
   c nb A64SEL:BIND-SOURCE
   c m nb NLOOP:REWRITE {: m1:IR-BUILD:module :}
   NLOOP:FOLDED n <> if E-NLOOP-PLAN throw then
   m IR-BUILD:RETIRE
   m1 ;

\ The lowering pass is bound here because a module's symbols are its own.
\
\ THE HIR MODULE IS FROZEN INTERIM. It is never the module this compilation
\ emits - selection reads it and writes the A64 module - and that one is
\ verified whole. So the HIR freeze derives the edge table selection reads and
\ leaves the checking to the freeze of the module that becomes the routine.
: SELECT ( IR-CTX:ctx IR-BUILD:builder -- IR-BUILD:module )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b NLOOP:BIND-DIALECT
   c b A64SEL:BIND-SOURCE
   b IR-BUILD:FUNS D-FUNS !
   c b IR-BUILD:FREEZE-INTERIM {: m0:IR-BUILD:module :}
   c m0 CLOSED {: m:IR-BUILD:module :}
   c A64-BUILDER {: ab:IR-BUILD:builder :}
   c ab A64IR:MACHINE A64RA:BIND-DIALECT
   c ab A64RAV:BIND-DIALECT
   c ab A64EMIT:BIND-DIALECT
   c ab A64SPILL:BIND-DIALECT
   c ab A64PRUNE:BIND-DIALECT
   c m ab ROUTINE A64SEL:SELECT {: selected:IR-BUILD:module :}
   m IR-BUILD:RETIRE
   selected ;

\ ---- pruning -----------------------------------------------------------------
\ A module with no such load is handed back UNTOUCHED: rebuilding renumbers
\ values and the allocator breaks ties on those numbers, so a routine that
\ gained nothing would still come out with other bytes. Nothing in the corpus
\ reaches the rebuild; the shapes that do are named in prune.f.
: PRUNE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   NPROF-PHASE:PRUNE NPROF:START
   m A64PRUNE:REWRITES {: n:n :}
   n 0= if
      A64PRUNE:RELEASE  NPROF-PHASE:PRUNE NPROF:STOP  m exit
   then
   A64RA:RELEASE
   A64EMIT:RELEASE
   A64SPILL:RELEASE
   c A64-BUILDER {: nb:IR-BUILD:builder :}
   c nb A64IR:MACHINE A64RA:BIND-DIALECT
   c nb A64RAV:BIND-DIALECT
   c nb A64EMIT:BIND-DIALECT
   c nb A64SPILL:BIND-DIALECT
   c m nb A64PRUNE:REWRITE {: m1:IR-BUILD:module :}
   A64PRUNE:REWRITTEN n <> if E-A64PRUNE-SHAPE throw then
   m IR-BUILD:RETIRE
   NPROF-PHASE:PRUNE NPROF:STOP
   m1 ;

\ ---- lowering ----------------------------------------------------------------
\ The reserve is sized from A64RA:FRAME, the same count ROUTINE declares from,
\ so the module and its contract agree by construction.
: LOWERED ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   A64EMIT:RELEASE
   c A64-BUILDER {: nb:IR-BUILD:builder :}
   c nb A64IR:MACHINE A64RA:BIND-DIALECT
   c nb A64RAV:BIND-DIALECT
   c nb A64EMIT:BIND-DIALECT
   NPROF-PHASE:SPILL NPROF:START
   c m nb A64SPILL:REWRITE
   NPROF-PHASE:SPILL NPROF:STOP ;

\ Turn the allocator's absolute frame high-water back into the ABI's slot count.
\ Alignment holes stay counted, so a later allocation starts after this frame
\ rather than reusing padding as though it were unowned.
: KEEP-FRAME ( NEFF:routine -- )
   {: r :}
   r NEFF:TRAITS@  r NEFF:LINK@  A64FRAME:SPILL-BASE {: base:n :}
   A64RA:FRAME base - {: bytes:n :}
   bytes 0 <  bytes A64IR:SLOT-WIDTH mod 0<> or if E-A64RA-FRAME throw then
   bytes A64IR:SLOT-WIDTH / D-SPILLS ! ;

\ Frame slots and DECISIONS are different counts: a value re-emitted where it is
\ read takes no slot, so a walk asked through the slot count looks like one that
\ decided nothing. A routine that calls still cannot spill; it is refused.
: NEEDS-LOWERING? ( IR-CTX:ctx IR-BUILD:module -- bool )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   ROUTINE {: r :}
   c m r A64RA:ALLOCATE
   A64RA:PLAN-N 0= if false exit then
   A64RA:SPILLS A64RA:REMATS + A64RA:MOVES +
   0= if E-A64SPILL-PLAN throw then
   r KEEP-FRAME
   true ;

\ Each turn consumes a non-empty sealed plan and rewrites all of its decisions.
\ The next allocation either seals an empty plan or contributes another class.
: FIXPOINT ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   m
   begin
      c over NEEDS-LOWERING?
   while
      c over LOWERED
      swap IR-BUILD:RETIRE
   repeat ;

\ ---- emission ----------------------------------------------------------------
\ Declared for every definition, not only one that calls, so the seam can place
\ it at the slot it really claims. The spill binding is given back first: the
\ fixpoint above leaves it standing when its last turn lowered.
: EMIT ( IR-CTX:ctx IR-BUILD:module n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module at:n :}
   A64SPILL:BOUND? if A64SPILL:RELEASE then
   m ROUTINE A64RAV:ACCEPT
   at A64EMIT:PLACE-AT
   c m A64EMIT:EMIT ;

\ ---- what this backend holds between definitions -----------------------------
\ Caught INSIDE the context so it always leaves the ordinary way and gives its
\ arenas back. Each pass is asked about ITSELF, so this cannot get out of step.
: RELEASE ( -- )
   NLOOP:BOUND? if NLOOP:RELEASE then
   A64SEL:BOUND? if A64SEL:RELEASE then
   A64RA:BOUND? if A64RA:RELEASE then
   A64SPILL:BOUND? if A64SPILL:RELEASE then
   A64PRUNE:BOUND? if A64PRUNE:RELEASE then
   A64EMIT:BOUND? if A64EMIT:RELEASE then ;

\ The registry releases buffers immediately before DATA copy. Reset the pass
\ reservations here so a restored compiler sizes them again on use.
: PREPARE ( -- )
   A64EMIT:CAPTURE-PREPARE
   A64EMIT:RESET-SCRATCH
   A64SPILL:RESET-SCRATCH
   A64RAV:RESET-SCRATCH
   A64RA:RESET-SCRATCH
   A64PRUNE:RESET-SCRATCH
   A64SEL:RESET-SCRATCH
   NLOOP:RESET-SCRATCH ;

: ARCH ( -- CTARGET:arch )
   CTARGET-ARCH:AARCH64 ;

public

\ The row this backend fills as it loads, stage by stage. src/arch/arm64/
\ backend.f has already claimed the registry row these are stored beside.
: INSTALL ( -- )
   ARCH [: DECLARE ;] NBACK:DECLARE!
   ARCH [: SELECT ;] NBACK:SELECT!
   ARCH [: PRUNE ;] NBACK:PRUNE!
   ARCH [: FIXPOINT ;] NBACK:FIXPOINT!
   ARCH [: EMIT ;] NBACK:EMIT!
   ARCH [: RELEASE ;] NBACK:RELEASE!
   ARCH [: A64EMIT:RETIRE ;] NBACK:RETIRE!
   ARCH [: A64IR:PROTOTYPE ;] NBACK:PROTOTYPE!
   ARCH [: A64IR:PROTOTYPE-CLEAR ;] NBACK:FORGET!
   ARCH [: PREPARE ;] NBACK:PREPARE! ;

;package

A64PASS:INSTALL
