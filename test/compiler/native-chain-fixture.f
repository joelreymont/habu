\ native-chain-fixture.f - the back half of the native chain, for everything that
\ needs to run it. One concern: turning a straight-line source module that is
\ ready to be frozen into emitted ARM64 bytes.

require src/compiler/native/abi.f
require src/compiler/native/select.f
require src/arch/arm64/machine.f
require src/compiler/native/emit.f
require src/compiler/native/spill.f

package NFIX

private

\ ---- the routine contract ----------------------------------------------------
\ The register pool and the two Habu-word contracts belong to the convention
\ itself, not to this fixture: src/compiler/native/abi.f states them once so the
\ suites and the publication seam answer about one convention. What is left here
\ is the C ABI, which only this fixture's callers use.
: POOL ( n n -- NEFF:gprs )
   NABI:POOL ;

: LEAF-OF ( NEFF:gprs -- NEFF:routine )
   {: pool:NEFF:gprs :}
   NEFF-CONV:REGISTER NEFF:SEQ-NONE NEFF:SEQ-NONE pool
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 A64M:MACHINE NEFF:ROUTINE ;

\ A leaf routine that may use `n` registers starting at `base`.
: LEAF-FROM ( n n -- NEFF:routine )
   POOL LEAF-OF ;

\ ---- the C ABI these routines are called through -----------------------------
\ AAPCS64 hands the first integer arguments over in x0 upwards and takes an
\ integer result back out of x0, and that is the whole convention a leaf routine
\ of this fixture has. It is written here once because the fixture is what calls
\ the emitted code - through the engine's own C-ABI call - so this is the one
\ place that knows which registers the caller will really use. It belongs in the
\ target contract beside the ABI name that already selects it, and moving it
\ there is dot habu-publish-the-aapcs-b51c7e3f.
0 constant ABI-ARG0                  \ argument i arrives in x(ABI-ARG0 + i)
0 constant ABI-OUT0                  \ returned value j leaves in x(ABI-OUT0 + j)

: ABI-SEQ ( n n -- NEFF:placeseq )
   {: base:n n:n :}
   NEFF:SEQ-NONE
   n 0 ?do base i + NEFF:SEQ-WITH loop ;

\ The builder the machine module is written through.
: A64-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER ;

\ Bind the source dialect to the module being read and the machine dialect to the
\ module about to be written - to the allocator and to the emitter both, because
\ each keeps its own identities - then select. The source row is carried out of
\ the module being read, so no caller presents the text at all.
\ The routine contract reaches the selector as well as the allocator now, because
\ the selector is where a data-stack place becomes a load or a store. It is the
\ last argument for the same reason it is everywhere else: it is what the entry
\ points validate on the way in, and a contract binds whole to a typed local.
\
\ THE LOWERING PASS IS BOUND HERE TOO, because a module's symbols are its own
\ ordinals and this is the only moment the machine dialect can be asked them. A
\ run whose walk decides no spill gives that binding straight back, which is what
\ the RELEASE in each of the entry points below is.
: SELECTED ( IR-CTX:ctx IR-BUILD:builder NEFF:routine -- IR-BUILD:module )
   NEFF:VALIDATE
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:NEFF:routine :}
   c b A64SEL:BIND-SOURCE
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   c A64-BUILDER {: ab:IR-BUILD:builder :}
   c ab A64IR:MACHINE  c ab A64IR:VOCABULARY  A64RA:BIND-DIALECT
   c ab  c ab A64IR:VOCABULARY  A64RAV:BIND-DIALECT
   c ab A64EMIT:BIND-DIALECT
   c ab  c ab A64IR:LOWERING  [: A64IR:ENSURE-NAMED ;] A64SPILL:BIND-DIALECT
   c m ab r A64SEL:SELECT ;

public

\ The host AArch64 binding these chain runs are made under. Overflow wraps,
\ which is what ARM64's add, sub and mul do; a trapping unit is refused by the
\ selector and has its own case in the selection suite. It is the compiler's own
\ binding, stated in src/compiler/native/abi.f.
: BINDING ( -- CBIND:binding )
   NABI:BINDING ;

\ A leaf routine of `n` registers from the pool that starts at register zero.
: LEAF-N ( n -- NEFF:routine )
   0 swap LEAF-FROM ;

\ The same with a frame of its own, for a routine whose values do not all fit in
\ its registers: a spill needs somewhere to go, and how deep that is, is the
\ contract's declaration.
: LEAF-FRAMED ( n n -- NEFF:routine )
   {: n:n size:n :}
   0 n POOL {: pool:NEFF:gprs :}
   NEFF-CONV:REGISTER NEFF:SEQ-NONE NEFF:SEQ-NONE pool
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE size 0 A64M:MACHINE NEFF:ROUTINE ;

\ A leaf routine with the C ABI declared on it: `in` arguments arriving in x0
\ upwards and `out` returned values leaving in x0 upwards. The registers it may
\ use are `n` of them from `base` TOGETHER WITH the ones the convention names,
\ because a declared register outside the routine's own set is refused by name -
\ an argument could not be held there and a result could not be computed there.
\ The destroyed set is what is left after the result registers are taken out:
\ one register cannot be both a result and a register whose contents mean
\ nothing.
: LEAF-ABI ( n n n n -- NEFF:routine )
   {: base:n n:n in:n out:n :}
   ABI-ARG0 in ABI-SEQ {: args:NEFF:placeseq :}
   ABI-OUT0 out ABI-SEQ {: outs:NEFF:placeseq :}
   base n POOL
   args NEFF:SEQ-SET NEFF:GPR-WITH
   outs NEFF:SEQ-SET NEFF:GPR-WITH {: pool:NEFF:gprs :}
   NEFF-CONV:REGISTER args outs
   pool outs NEFF:SEQ-SET NEFF:GPR-WITHOUT
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 A64M:MACHINE NEFF:ROUTINE ;

\ The data-stack convention used by emitted Habu fixtures.
: LEAF-HABU ( n n n n -- NEFF:routine )
   {: base:n n:n in:n out:n :}
   base n NABI:POOL in out NABI:LEAF ;

\ Allocate registers for a frozen machine module, have the validator accept the
\ allocation, and emit. Nothing here emits from a claim the validator has not
\ agreed with, which is the whole reason the three stages are one word.
: FINISH ( IR-CTX:ctx IR-BUILD:module n n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module base:n n:n :}
   base n LEAF-FROM {: r:NEFF:routine :}
   c m r A64RA:ALLOCATE
   m r A64RAV:ACCEPT
   c m A64EMIT:EMIT ;

\ The same three stages under the data-stack convention a Habu word is entered
\ and left through. One contract, named, reaches both stages: the allocation and
\ the verdict on it cannot be about two separately built values.
: FINISH-HABU ( IR-CTX:ctx IR-BUILD:module n n n n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module base:n n:n in:n out:n :}
   base n in out LEAF-HABU {: r:NEFF:routine :}
   c m r A64RA:ALLOCATE
   m r A64RAV:ACCEPT
   c m A64EMIT:EMIT ;

\ Select and finish in one step, out of a pool of `n` registers from `base`.
: RUN-FROM ( IR-CTX:ctx IR-BUILD:builder n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder base:n n:n :}
   c b base n LEAF-FROM SELECTED {: m:IR-BUILD:module :}
   A64SPILL:RELEASE
   c m base n FINISH ;

\ The same, out of the pool that starts at register zero.
: RUN ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder n:n :}
   c b 0 n RUN-FROM ;

\ Select and finish under the data-stack convention: `in` arguments taken out of
\ slots 0.. of the caller's stack and `out` results left in slots 0.., with `n`
\ scratch registers from `base`. This is the whole of what makes an emitted
\ routine callable the way an interpreted word is.
: RUN-HABU ( IR-CTX:ctx IR-BUILD:builder n n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder base:n n:n in:n out:n :}
   c b base n in out LEAF-HABU SELECTED {: m:IR-BUILD:module :}
   A64SPILL:RELEASE
   c m base n in out FINISH-HABU ;

\ The register the returned value ended up in. The last value the module defines
\ is the one the return carries in every straight-line shape, and it is read
\ through the validator rather than off the allocator's raw claim.
: RESULT-REG ( -- n )
   A64RA:VALUES 1- A64RAV:REG@ ;

;package
