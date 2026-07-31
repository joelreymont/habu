\ native-chain-fixture.f - the back half of the native chain, for everything that
\ needs to run it. One concern: turning a straight-line source module that is
\ ready to be frozen into emitted ARM64 bytes.
\
\ WHY THIS IS ITS OWN FILE. Two callers drive the same four stages in the same
\ order - test/compiler/native-emit.f, which proves the bytes are the
\ instructions they claim to be, and tools/codegen-compare-new.f, which measures
\ them against the emitter bin/hb uses today. The stages have to be driven
\ identically or the two disagree for a reason that is about the harnesses rather
\ than about the compiler, so the order lives here once: bind the source dialect
\ to the module being read, freeze it, build the machine module's builder, bind
\ the allocator and the emitter to it, select, allocate, accept, emit.
\
\ WHAT IS NOT HERE. How the source module was built is the caller's own: the
\ emission suite builds its shapes into HIR by hand because it is proving what a
\ named shape encodes to, and the comparison harness elaborates real source text
\ because it is measuring what a real word compiles to. Both hand this file a
\ builder with one function in it and get bytes back.
\
\ THE ROUTINE CONTRACT IS A PARAMETER, NOT A CONSTANT. The allocator is told
\ which registers a routine may use, and the emission suite deliberately runs one
\ shape out of a pool that starts above register zero so every register field in
\ the encoding is reached. The pool is therefore a base and a count here rather
\ than a fixed set.
\
\ NOTHING IN THIS FILE ASSERTS. It defines no case and prints nothing: it is a
\ fixture, not a test, so it never names the harness verdict word and no gate
\ schedules it on its own.

require src/compiler/native/select.f
require src/compiler/native/emit.f

package NFIX

private

\ ---- the routine contract ----------------------------------------------------
: POOL ( n n -- A64EFF:gprs )
   {: base:n n:n :}
   A64EFF:GPR-NONE
   n 0 ?do base i + A64EFF:GPR-REG A64EFF:GPR-WITH loop ;

: LEAF-OF ( A64EFF:gprs -- A64EFF:routine )
   {: pool:A64EFF:gprs :}
   A64EFF:GPR-NONE A64EFF:GPR-NONE pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

public

\ The AArch64 Darwin binding these chain runs are made under. Overflow wraps,
\ which is what ARM64's add, sub and mul do; a trapping unit is refused by the
\ selector and has its own case in the selection suite.
: BINDING ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A leaf routine that may use `n` registers starting at `base`.
: LEAF-FROM ( n n -- A64EFF:routine )
   POOL LEAF-OF ;

\ The same, out of the pool that starts at register zero.
: LEAF-N ( n -- A64EFF:routine )
   0 swap LEAF-FROM ;

\ The builder the machine module is written through.
: A64-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER ;

\ Bind the source dialect to the module being read and the machine dialect to the
\ module about to be written - to the allocator and to the emitter both, because
\ each keeps its own identities - then select. The text is the source the module
\ was compiled from; the selector checks its digest against the one the module
\ recorded, so a caller cannot present other bytes.
: SELECTED ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-BUILD:module )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b A64SEL:BIND-SOURCE
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   c A64-BUILDER {: ab:IR-BUILD:builder :}
   c ab A64RA:BIND-DIALECT
   c ab A64EMIT:BIND-DIALECT
   c m ab a u A64SEL:SELECT ;

\ Allocate registers for a frozen machine module, have the validator accept the
\ allocation, and emit. Nothing here emits from a claim the validator has not
\ agreed with, which is the whole reason the three stages are one word.
: FINISH ( IR-CTX:ctx IR-BUILD:module n n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module base:n n:n :}
   c m base n LEAF-FROM A64RA:ALLOCATE
   m base n LEAF-FROM A64RAV:ACCEPT
   c m A64EMIT:EMIT ;

\ Select and finish in one step, out of a pool of `n` registers from `base`.
: RUN-FROM ( IR-CTX:ctx IR-BUILD:builder ptr u8 n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n base:n n:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b a u SELECTED {: m:IR-BUILD:module :}
   c m base n FINISH ;

\ The same, out of the pool that starts at register zero.
: RUN ( IR-CTX:ctx IR-BUILD:builder ptr u8 n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n n:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b a u 0 n RUN-FROM ;

\ The register the returned value ended up in. The last value the module defines
\ is the one the return carries in every straight-line shape, and it is read
\ through the validator rather than off the allocator's raw claim.
: RESULT-REG ( -- n )
   A64RA:VALUES 1- A64RAV:REG@ ;

;package
