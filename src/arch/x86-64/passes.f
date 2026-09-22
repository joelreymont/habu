\ passes.f - the x86-64 backend's pass rows in the compiler's pass table.
\
\ WHAT IT IS FOR. It is to src/arch/x86-64 what src/arch/arm64/passes.f is to
\ AArch64: the row src/compiler/native/compiler.f reaches when a definition's
\ own target contract resolves to this machine. Every stage is the sequence that
\ file runs for ARM64, wrapping X64SEL, A64RA, A64RAV and A64SPILL; none of their
\ state moved here.
\
\ THE SPILL PASS IS THE SHARED ONE. src/compiler/native/spill.f names no dialect:
\ it is told the dialect's lowering record and how that dialect materialises a
\ form by name, so this backend lowers its spills through the same pass the ARM64
\ one does, bound with X64IR:LOWERING and X64IR:ENSURE-NAMED. The allocator and
\ its validator are shared for the same reason, bound with X64M:MACHINE and
\ X64IR:VOCABULARY.
\
\ PRUNE IS A PASS-THROUGH ON THIS MACHINE. src/compiler/native/prune.f rewrites
\ nothing on the whole corpus (its own header says so) and this machine has no
\ prune pass of its own; the row hands the module back as it is, because
\ rebuilding renumbers values and a routine that gained nothing would still come
\ out with other bytes.
\
\ EMISSION IS THIS BACKEND'S LAST ROW AND IT IS FILLED. src/compiler/native/
\ emit-x64.f owns its byte sink, lays every block of the accepted module out in
\ BYTES, resolves every displacement against the slot this row is handed, and
\ seals a byte image with the list of the relocatable literals in it. RETIRE
\ gives that image and the placement back, and is nonthrowing because the driver
\ calls it on the refusing path too. WHAT NO ROW HERE DOES IS PUBLISH: this host
\ has no x86-64 code region to write into (src/compiler/native/publish.f names
\ A64EMIT throughout), so the sealed image is read back by a cross-build image
\ writer and not by NPUB.
\
\ So declare, select, prune, the lowering fixpoint, emit and retire all run in
\ the order src/compiler/native/compiler.f runs them;
\ test/compiler/x64-chain.f drives that chain through this table.
\
\ HIR LOOP FOLDING TRAVELS WITH SELECTION for the reason the ARM64 file gives:
\ the fold rewrites the module the selector is bound to as its source, so the
\ binding has to be taken and re-made around it. src/compiler/native/loop.f is
\ target-free and this is the second backend reusing it.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/ir/id.f
require src/compiler/ir/arena.f
require src/compiler/ir/context.f
require src/compiler/ir/build.f
require src/compiler/native/backend.f
require src/compiler/native/frame.f
require src/compiler/native/x64ir.f
require src/compiler/native/hir.f
require src/compiler/native/loop.f
require src/compiler/native/select-x64.f
require src/compiler/native/spill.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/compiler/native/emit-x64.f
require src/compiler/native/prof.f
require src/arch/x86-64/backend.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/machine.f

package X64PASS
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
         X64ABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ X64ABI:NORET-FRAMED exit
      then
      X64ABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ X64ABI:NORET-LEAF-FRAMED exit
   then
   D-TAIL @ 0<> D-FUNS @ 1 = and if
      D-BACK @ 0<> if
         X64ABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ X64ABI:TAIL-CALLING-FRAMED exit
      then
      X64ABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ X64ABI:TAIL-FRAMED exit
   then
   D-CALLED @ 0<> if
      X64ABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ X64ABI:CALL-FRAMED exit
   then
   X64ABI:SCRATCH D-IN @ D-OUT @ D-SPILLS @ X64ABI:LEAF-FRAMED ;

: X64-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c X64IR:NEW-BUILDER ;

: HIR-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER ;

\ The allocator, its validator, the emitter and the shared spill pass, bound to
\ the module a stage is about to write. Selection and each lowering turn both
\ mint a machine module, and every pass that reads or writes one names that
\ module's own symbols - which is why this is an act and not a one-time setup.
\ The emitter is bound here too: the module it writes is whichever one the
\ fixpoint stopped on, and only the turn that minted it can hand it its symbols.
: BIND-MACHINE ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b X64M:MACHINE  c b X64IR:VOCABULARY  A64RA:BIND-DIALECT
   c b  c b X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   c b X64EMIT:BIND-DIALECT ;

: BIND-SPILL ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b X64IR:LOWERING  [: X64IR:ENSURE-NAMED ;] A64SPILL:BIND-DIALECT ;

\ ---- selection ---------------------------------------------------------------
\ A module with no such loop is handed back UNTOUCHED: rebuilding renumbers
\ values, so a routine that gained nothing could still come out with other bytes.
: CLOSED ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   m NLOOP:FOLDS {: n:n :}
   n 0= if NLOOP:RELEASE m exit then
   X64SEL:RELEASE
   c HIR-BUILDER {: nb:IR-BUILD:builder :}
   c nb X64SEL:BIND-SOURCE
   c m nb NLOOP:REWRITE {: m1:IR-BUILD:module :}
   NLOOP:FOLDED n <> if E-NLOOP-PLAN throw then
   m IR-BUILD:RETIRE
   m1 ;

\ The lowering pass is bound here because a module's symbols are its own.
\
\ THE HIR MODULE IS FROZEN INTERIM, for the reason the ARM64 file gives: it is
\ never the module this compilation emits, so its freeze derives the edge table
\ selection reads and leaves the checking to the freeze of the module that
\ becomes the routine.
: SELECT ( IR-CTX:ctx IR-BUILD:builder -- IR-BUILD:module )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b NLOOP:BIND-DIALECT
   c b X64SEL:BIND-SOURCE
   b IR-BUILD:FUNS D-FUNS !
   c b IR-BUILD:FREEZE-INTERIM {: m0:IR-BUILD:module :}
   c m0 CLOSED {: m:IR-BUILD:module :}
   c X64-BUILDER {: xb:IR-BUILD:builder :}
   c xb BIND-MACHINE
   c xb BIND-SPILL
   c m xb ROUTINE X64SEL:SELECT {: selected:IR-BUILD:module :}
   m IR-BUILD:RETIRE
   selected ;

\ ---- pruning -----------------------------------------------------------------
\ This machine has no prune pass: src/compiler/native/prune.f rewrites nothing
\ on the whole corpus, and rebuilding a module renumbers its values, so a
\ routine that gained nothing would still come out with other bytes. The module
\ selection wrote is handed back UNTOUCHED - the ARM64 row's own answer when its
\ prune finds nothing, without the binding it has and this backend has not.
: PRUNE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   m ;

\ ---- lowering ----------------------------------------------------------------
\ The reserve is sized from A64RA:FRAME, the same count ROUTINE declares from,
\ so the module and its contract agree by construction.
: LOWERED ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   c X64-BUILDER {: nb:IR-BUILD:builder :}
   c nb BIND-MACHINE
   NPROF-PHASE:SPILL NPROF:START
   c m nb  c nb X64IR:LOWERING  A64SPILL:REWRITE
   NPROF-PHASE:SPILL NPROF:STOP ;

\ Turn the allocator's absolute frame high-water back into the ABI's slot count.
\ Alignment holes stay counted, so a later allocation starts after this frame
\ rather than reusing padding as though it were unowned. The base is zero on
\ this machine however the routine is entered: `call` pushes the return address
\ onto the machine stack, so no contract here keeps a slot for it.
: KEEP-FRAME ( NEFF:routine -- )
   {: r :}
   r NEFF:TRAITS@  r NEFF:LINK@  A64FRAME:SPILL-BASE {: base:n :}
   A64RA:FRAME base - {: bytes:n :}
   bytes 0 <  bytes X64IR:SLOT-WIDTH mod 0<> or if E-A64RA-FRAME throw then
   bytes X64IR:SLOT-WIDTH / D-SPILLS ! ;

\ Frame slots and DECISIONS are different counts: a value re-emitted where it is
\ read takes no slot, so a walk asked through the slot count looks like one that
\ decided nothing.
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
\ Declared for every definition and not only one that calls, so the slot this
\ routine really claims is the one its own displacements are measured from. The
\ spill binding is given back first: the fixpoint above leaves it standing when
\ its last turn lowered.
: EMIT ( IR-CTX:ctx IR-BUILD:module n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module at:n :}
   A64SPILL:BOUND? if A64SPILL:RELEASE then
   m ROUTINE A64RAV:ACCEPT
   at X64EMIT:PLACE-AT
   c m X64EMIT:EMIT ;

\ ---- what this backend holds between definitions -----------------------------
\ Caught INSIDE the context so it always leaves the ordinary way and gives its
\ arenas back. Each pass is asked about ITSELF, so this cannot get out of step.
: RELEASE ( -- )
   NLOOP:BOUND? if NLOOP:RELEASE then
   X64SEL:BOUND? if X64SEL:RELEASE then
   A64RA:BOUND? if A64RA:RELEASE then
   A64SPILL:BOUND? if A64SPILL:RELEASE then
   X64EMIT:BOUND? if X64EMIT:RELEASE then ;

\ The registry releases buffers immediately before DATA copy. Reset the pass
\ reservations here so a restored compiler sizes them again on use.
: PREPARE ( -- )
   X64EMIT:CAPTURE-PREPARE
   X64EMIT:RESET-SCRATCH
   A64SPILL:RESET-SCRATCH
   A64RAV:RESET-SCRATCH
   A64RA:RESET-SCRATCH
   X64SEL:RESET-SCRATCH
   NLOOP:RESET-SCRATCH ;

: ARCH ( -- CTARGET:arch )
   CTARGET-ARCH:X86-64 ;

public

\ The row this backend fills as it loads, stage by stage. src/arch/x86-64/
\ backend.f has already claimed the registry row these are stored beside.
: INSTALL ( -- )
   ARCH [: DECLARE ;] NBACK:DECLARE!
   ARCH [: SELECT ;] NBACK:SELECT!
   ARCH [: PRUNE ;] NBACK:PRUNE!
   ARCH [: FIXPOINT ;] NBACK:FIXPOINT!
   ARCH [: EMIT ;] NBACK:EMIT!
   ARCH [: RELEASE ;] NBACK:RELEASE!
   ARCH [: X64EMIT:RETIRE ;] NBACK:RETIRE!
   ARCH [: X64IR:PROTOTYPE ;] NBACK:PROTOTYPE!
   ARCH [: X64IR:PROTOTYPE-CLEAR ;] NBACK:FORGET!
   ARCH [: PREPARE ;] NBACK:PREPARE! ;

;package

X64PASS:INSTALL
