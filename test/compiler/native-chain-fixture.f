\ native-chain-fixture.f - the back half of the native chain, for everything that
\ needs to run it. One concern: turning a straight-line source module that is
\ ready to be frozen into emitted ARM64 bytes.

require src/compiler/native/abi.f
require src/compiler/native/select.f
require src/arch/arm64/machine.f
require src/compiler/native/emit.f
require src/compiler/native/spill.f
require src/arch/arm64/passes.f

package NFIX

private

\ ---- the routine contract ----------------------------------------------------
\ The register pool and the two Habu-word contracts live in
\ src/compiler/native/abi.f; only the C ABI, used by this fixture's callers, is
\ here.
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
\ AAPCS64: integer arguments arrive in x0 upwards and results leave in x0
\ upwards; this fixture calls emitted code through the engine's C-ABI call.
0 constant ABI-ARG0                  \ argument i arrives in x(ABI-ARG0 + i)
0 constant ABI-OUT0                  \ returned value j leaves in x(ABI-OUT0 + j)

: ABI-SEQ ( n n -- NEFF:placeseq )
   {: base:n n:n :}
   NEFF:SEQ-NONE
   n 0 ?do base i + NEFF:SEQ-WITH loop ;

: A64-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER ;

\ The source dialect binds to the module read and the machine dialect to the
\ module written, for the allocator and the emitter both: each keeps its own
\ identities. The contract reaches the selector, where a data-stack place
\ becomes a load or a store. The lowering pass binds here because a module's
\ symbols are its own ordinals; a walk that decides no spill gives the binding
\ back (the RELEASE in each entry point below).
: SELECTED ( IR-CTX:ctx IR-BUILD:builder NEFF:routine -- IR-BUILD:module )
   NEFF:VALIDATE
   {: c:IR-CTX:ctx b:IR-BUILD:builder r:NEFF:routine :}
   c b HIR:ENSURE-VOCABULARY
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m A64SEL:BIND-SOURCE
   c A64-BUILDER {: ab:IR-BUILD:builder :}
   c ab A64IR:MACHINE  c ab A64IR:VOCABULARY  A64RA:BIND-DIALECT
   c ab  c ab A64IR:VOCABULARY  A64RAV:BIND-DIALECT
   c ab A64EMIT:BIND-DIALECT
   c ab  c ab A64IR:LOWERING  [: A64IR:ENSURE-NAMED ;] A64SPILL:BIND-DIALECT
   c m ab r A64SEL:SELECT ;

public

\ Overflow wraps, as ARM64's add, sub and mul do; the selector refuses a
\ trapping unit (its own case in the selection suite). This is the compiler's
\ own profile on any host.
: BINDING ( -- CBIND:binding )
   HB-TARGET-MACOS? if
      s" aarch64-apple-darwin"
   else
      s" aarch64-unknown-linux-gnu"
   then RTARGET:RESOLVE RTARGET:NATIVE-BINDING ;

\ A leaf routine of `n` registers from the pool that starts at register zero.
: LEAF-N ( n -- NEFF:routine )
   0 swap LEAF-FROM ;

\ The same with a frame for spills, whose depth the contract declares.
: LEAF-FRAMED ( n n -- NEFF:routine )
   {: n:n size:n :}
   0 n POOL {: pool:NEFF:gprs :}
   NEFF-CONV:REGISTER NEFF:SEQ-NONE NEFF:SEQ-NONE pool
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE size 0 A64M:MACHINE NEFF:ROUTINE ;

\ `in` arguments arrive in x0 upwards and `out` results leave in x0 upwards. The
\ usable set adds the convention's registers to `n` from `base`: a declared
\ register outside the routine's set is refused by name. The destroyed set
\ excludes the result registers.
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

\ Nothing emits from an allocation the validator has not accepted, which is why
\ the three stages are one word.
: FINISH ( IR-CTX:ctx IR-BUILD:module n n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module base:n n:n :}
   base n LEAF-FROM {: r:NEFF:routine :}
   c m r A64RA:ALLOCATE
   m r A64RAV:ACCEPT
   c m A64EMIT:EMIT ;

\ The same under the Habu data-stack convention: one named contract reaches the
\ allocation and the verdict, so they cannot be about two separately built
\ values.
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

\ Select and finish under the data-stack convention: `in` arguments from slots
\ 0.. of the caller's stack, `out` results left in slots 0.., `n` scratch
\ registers from `base`.
: RUN-HABU ( IR-CTX:ctx IR-BUILD:builder n n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder base:n n:n in:n out:n :}
   c b base n in out LEAF-HABU SELECTED {: m:IR-BUILD:module :}
   A64SPILL:RELEASE
   c m base n in out FINISH-HABU ;

\ The last value the module defines is the one the return carries in every
\ straight-line shape; it is read through the validator, not the allocator's
\ claim.
: RESULT-REG ( -- n )
   A64RA:VALUES 1- A64RAV:REG@ ;

;package
