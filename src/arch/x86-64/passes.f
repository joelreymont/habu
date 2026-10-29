\ passes.f - X64PASS, the x86-64 backend's rows in the compiler's pass table, the
\ twin of src/arch/arm64/passes.f: src/compiler/native/compiler.f drives each
\ stage through the row the definition's target contract resolves to. The rows
\ wrap X64SEL, A64RA, A64RAV, A64SPILL (the shared src/compiler/native/spill.f)
\ and X64EMIT (src/compiler/native/emit-x64.f) and hold none of their state.
\ test/compiler/x64-chain.f drives the chain through this table.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/target.f
require src/compiler/ir/id.f
require src/compiler/ir/arena.f
require src/compiler/ir/context.f
require src/compiler/ir/build.f
require src/compiler/ir/fun.f
require src/compiler/native/backend.f
require src/compiler/native/frame.f
require src/compiler/native/x64ir.f
require src/compiler/native/select-x64.f
require src/compiler/native/spill.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/compiler/native/emit-x64.f
require src/compiler/native/emission.f
require src/compiler/native/prof.f
require src/arch/x86-64/backend.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/machine.f
require src/habu/address-carrier.f
require src/compiler/native/hir.f
require src/compiler/native/shadow.f

package X64PASS
private

: ARCH ( -- CTARGET:arch )
   CTARGET-ARCH:X86-64 ;

\ ---- the routine contract this definition compiles to ------------------------
variable D-IN                        \ cells the definition takes
variable D-OUT                       \ cells it leaves
variable D-DEAD                      \ control never comes back
variable D-CALLED                    \ a call site reaches it
variable D-TAIL                      \ tail-called from inside the emitted region
variable D-BACK                      \ it calls back out
variable D-FUNS                      \ functions sharing the emitted routine contract
variable D-SPILLS                    \ padded spill slots that define the cumulative frame

\ The two counts this backend reads for itself start again here, so a stage sees
\ only the definition it is compiling.
: DECLARE ( n n NBACK:linkage -- )
   {: in:n out:n l:NBACK:linkage :}
   in D-IN !  out D-OUT !
   l NBACK:L-DEAD NBACK:HAS? D-DEAD !
   l NBACK:L-CALLED NBACK:HAS? D-CALLED !
   l NBACK:L-TAIL NBACK:HAS? D-TAIL !
   l NBACK:L-BACK NBACK:HAS? D-BACK !
   l NBACK:L-PATCH NBACK:HAS? X64EMIT:PATCH-SLOT!
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

\ Selection and each lowering turn mint a machine module whose symbols are its
\ own, so every pass that reads or writes one, the emitter included, is bound
\ again for each module.
: BIND-MACHINE ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b X64M:MACHINE  c b X64IR:VOCABULARY  A64RA:BIND-DIALECT
   c b  c b X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   c b X64EMIT:BIND-DIALECT ;

: BIND-SPILL ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b X64IR:LOWERING  [: X64IR:ENSURE-NAMED ;] A64SPILL:BIND-DIALECT ;

\ ---- selection ---------------------------------------------------------------
\ The lowering pass is bound here because a module's symbols are its own. The
\ source module is NBACK:FREEZE's, shared by every target the definition
\ compiles to; the driver retires it after its last selector.
: SELECT ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   m X64SEL:BIND-SOURCE
   m IR-BUILD:FFUN-ROWS IR-ARENA:OPEN IR-FUN:RFUNS D-FUNS !
   c X64-BUILDER {: xb:IR-BUILD:builder :}
   c xb BIND-MACHINE
   c xb BIND-SPILL
   c m xb ROUTINE X64SEL:SELECT ;

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

\ ---- the sealed emission, as the rows publication reads ----------------------
\ Publication reads these rows in NEMIT before RETIRE clears them. The emitter
\ answers in bytes and each site names an instruction's first byte, so the rows
\ are copied across and nothing is decoded. An x86-64 span is exact: it is the
\ whole routine, with no trailing return split off.
: FUNCTION-ROWS ( -- )
   X64EMIT:FUNS 0 ?do
      i X64EMIT:FUNCTION-OFFSET@ NEMIT:FUNCTION+
   loop ;

: CALL-ROWS ( -- )
   X64EMIT:CALL-SITES 0 ?do
      i X64EMIT:CALL-SITE@  i X64EMIT:CALL-KIND@  i X64EMIT:CALL-TARGET@
      i X64EMIT:CALL-IMPL@  i X64EMIT:CALL-LOC@
      NEMIT:CALL-SITE+
   loop ;

: ADDR-ROWS ( -- )
   X64EMIT:ADDR-SITES 0 ?do
      i X64EMIT:ADDR-SITE@  i X64EMIT:ADDR-SITE-KIND@  -1 NEMIT:ADDR-SITE+
   loop ;

: OPEN-ROWS ( -- )
   X64EMIT:BYTES X64EMIT:SIZE 0 ARCH NEMIT:OPEN
   X64EMIT:PATCH-SLOT dup -1 <> if NEMIT:PATCH-SLOT! else drop then ;

: SEAL-ROWS ( -- )
   FUNCTION-ROWS
   CALL-ROWS
   ADDR-ROWS
   NEMIT:SEAL ;

: ROWS ( n -- ) {: at:n :}
   OPEN-ROWS
   at NEMIT:PLACE
   SEAL-ROWS ;

\ ---- emission ----------------------------------------------------------------
\ The spill binding is released first: FIXPOINT leaves it bound when its last
\ turn lowered.
: ACCEPTED ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   A64SPILL:BOUND? if A64SPILL:RELEASE then
   m ROUTINE A64RAV:ACCEPT ;

\ Declared for every definition and not only one that calls, so the slot this
\ routine really claims is the one its own displacements are measured from.
: EMIT ( IR-CTX:ctx IR-BUILD:module n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module at:n :}
   m ACCEPTED
   at X64EMIT:PLACE-AT
   c m X64EMIT:EMIT
   at ROWS ;

\ Nonthrowing: the driver calls it on the accepting and the refusing path, and
\ the rows answer until it runs.
: RETIRE ( -- )
   X64EMIT:RETIRE
   NEMIT:CLEAR ;

\ ---- what this backend holds between definitions -----------------------------
\ Caught INSIDE the context so it always leaves the ordinary way and gives its
\ arenas back. Each pass is asked about ITSELF, so this cannot get out of step.
: RELEASE ( -- )
   X64SEL:BOUND? if X64SEL:RELEASE then
   A64RA:BOUND? if A64RA:RELEASE then
   A64SPILL:BOUND? if A64SPILL:RELEASE then
   X64EMIT:BOUND? if X64EMIT:RELEASE then ;

\ The emitter releases its sinks before DATA copy; the workspace and
\ dynamic registry release the remaining pass storage centrally.
: PREPARE ( -- )
   X64EMIT:CAPTURE-PREPARE ;

\ ---- the native shadow's address sites ---------------------------------------
\ A native shadow (src/compiler/native/shadow.f OPEN-NATIVE) copies the placed
\ emission while it is sealed in NEMIT, so each MOVABS site holds a host address.
\ An address of one of the emission's own function starts goes back to that
\ function's offset, the unplaced form the shadow linker reads; any other stays.
: FUN-OFF ( n -- n ) {: v:n :}
   NEMIT:PLACEMENT {: at:n :}
   NEMIT:FUNCTIONS 0 ?do
      i NEMIT:FUNCTION-OFFSET@ {: off:n :}
      at off + v = if off unloop exit then
   loop
   -1 ;

: SHADOW-ADDRS ( ptr u8 -- ) {: base:ptr :}
   NEMIT:ADDR-SITES 0 ?do
      i NEMIT:ADDR-SITE@ {: off:n :}
      off 0 < off ADDRESS-CARRIER:MOVABS-BYTES + NEMIT:SIZE > or
         if E-NSHADOW-ROW throw then
      base off + {: p:ptr :}
      p ADDRESS-CARRIER:MOVABS-SITE? 0= if E-NSHADOW-ROW throw then
      i NEMIT:ADDR-SITE-KIND@ NEMIT:ADDR-CODE = if
         p ADDRESS-CARRIER:MOVABSV FUN-OFF {: fun:n :}
         fun 0 >= if p fun ADDRESS-CARRIER:SET-MOVABS then
      then
   loop ;

public

\ The emit row with no slot: a shadow emission (emit-x64.f) whose call fields are
\ zero and whose rows alone say where they go, for a writer that links the
\ routine into its own stream (src/compiler/native/shadow.f, and the sealed row
\ of test/compiler/shadow.f in src/habu/kernel-hir-x64.f). Publication commits
\ only a placed emission, so it never takes these rows. RETIRE releases this too.
: EMIT-UNPLACED ( IR-CTX:ctx IR-BUILD:module -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   m ACCEPTED
   c m X64EMIT:EMIT
   OPEN-ROWS
   SEAL-ROWS ;

\ Publish only after every callback is defined. The id names this provider,
\ independent of target wire codes and its sorted runtime row.
: INSTALL ( -- )
   X64BACK:ID ARCH [: X64BACK:SERVES? ;] [: X64BACK:SERVES? ;]
      CTARGET-BACKEND:MAKE
   [: DECLARE ;] [: SELECT ;] [: FIXPOINT ;]
   [: EMIT ;] [: EMIT-UNPLACED ;] [: RELEASE ;] [: RETIRE ;]
   [: X64IR:PROTOTYPE ;] [: X64IR:PROTOTYPE-CLEAR ;] [: PREPARE ;]
      NBACK-PASS:MAKE
   NBACK:REGISTER
   [: SHADOW-ADDRS ;] NSHADOW:NATIVE-ADDRS! ;

;package

X64PASS:INSTALL
