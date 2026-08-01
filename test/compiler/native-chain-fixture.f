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
\ than a fixed set. It now reaches the SELECTOR too, because a convention that
\ names data-stack slots becomes operations at selection time.
\
\ TWO CONVENTIONS, BOTH BUILT HERE. LEAF-ABI declares the C ABI - arguments in x0
\ upwards, result out of x0 - which is what the emission suite's routines are
\ entered through, and LEAF-HABU declares the one design section 7.6 gives a
\ Habu word: argument i out of data-stack slot i of the caller's stack, result j
\ into slot j. A routine under the second is entered by the branch the
\ interpreter itself uses, which is what makes an emitted routine callable the
\ way an interpreted word is.
\
\ NOTHING IN THIS FILE ASSERTS. It defines no case and prints nothing: it is a
\ fixture, not a test, so it never names the harness verdict word and no gate
\ schedules it on its own.

require src/compiler/native/abi.f
require src/compiler/native/select.f
require src/compiler/native/emit.f
require src/compiler/native/spill.f

package NFIX

private

\ ---- the routine contract ----------------------------------------------------
\ The register pool and the two Habu-word contracts belong to the convention
\ itself, not to this fixture: src/compiler/native/abi.f states them once so the
\ suites and the publication seam answer about one convention. What is left here
\ is the C ABI, which only this fixture's callers use.
: POOL ( n n -- A64EFF:gprs )
   NABI:POOL ;

: LEAF-OF ( A64EFF:gprs -- A64EFF:routine )
   {: pool:A64EFF:gprs :}
   A64EFF:SEQ-NONE A64EFF:SEQ-NONE pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

\ A leaf routine that may use `n` registers starting at `base`.
: LEAF-FROM ( n n -- A64EFF:routine )
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

: ABI-SEQ ( n n -- A64EFF:placeseq )
   {: base:n n:n :}
   A64EFF:SEQ-NONE
   n 0 ?do base i + A64EFF:SEQ-WITH loop ;

\ The builder the machine module is written through.
: A64-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER ;

\ Bind the source dialect to the module being read and the machine dialect to the
\ module about to be written - to the allocator and to the emitter both, because
\ each keeps its own identities - then select. The text is the source the module
\ was compiled from; the selector checks its digest against the one the module
\ recorded, so a caller cannot present other bytes.
\ The routine contract reaches the selector as well as the allocator now, because
\ the selector is where a data-stack place becomes a load or a store. It is the
\ last argument for the same reason it is everywhere else: twelve cells cannot be
\ bound to a typed local, so it is presented on top and taken apart there.
: SELECTED ( IR-CTX:ctx IR-BUILD:builder ptr u8 n A64EFF:routine -- IR-BUILD:module )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control
      t:A64EFF:traits size:n delta:n :}
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b A64SEL:BIND-SOURCE
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   c A64-BUILDER {: ab:IR-BUILD:builder :}
   c ab A64RA:BIND-DIALECT
   c ab A64RAV:BIND-DIALECT
   c ab A64EMIT:BIND-DIALECT
   c m ab a u
   gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64SEL:SELECT ;

public

\ The AArch64 Darwin binding these chain runs are made under. Overflow wraps,
\ which is what ARM64's add, sub and mul do; a trapping unit is refused by the
\ selector and has its own case in the selection suite. It is the compiler's own
\ binding, stated in src/compiler/native/abi.f.
: BINDING ( -- CBIND:binding )
   NABI:BINDING ;

\ A leaf routine of `n` registers from the pool that starts at register zero.
: LEAF-N ( n -- A64EFF:routine )
   0 swap LEAF-FROM ;

\ The same with a frame of its own, for a routine whose values do not all fit in
\ its registers: a spill needs somewhere to go, and how deep that is, is the
\ contract's declaration.
: LEAF-FRAMED ( n n -- A64EFF:routine )
   {: n:n size:n :}
   0 n POOL {: pool:A64EFF:gprs :}
   A64EFF:SEQ-NONE A64EFF:SEQ-NONE pool
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE size 0 A64EFF:ROUTINE ;

\ A leaf routine with the C ABI declared on it: `in` arguments arriving in x0
\ upwards and `out` returned values leaving in x0 upwards. The registers it may
\ use are `n` of them from `base` TOGETHER WITH the ones the convention names,
\ because a declared register outside the routine's own set is refused by name -
\ an argument could not be held there and a result could not be computed there.
\ The destroyed set is what is left after the result registers are taken out:
\ one register cannot be both a result and a register whose contents mean
\ nothing.
: LEAF-ABI ( n n n n -- A64EFF:routine )
   {: base:n n:n in:n out:n :}
   ABI-ARG0 in ABI-SEQ {: args:A64EFF:placeseq :}
   ABI-OUT0 out ABI-SEQ {: outs:A64EFF:placeseq :}
   base n POOL
   args A64EFF:SEQ-SET A64EFF:GPR-WITH
   outs A64EFF:SEQ-SET A64EFF:GPR-WITH {: pool:A64EFF:gprs :}
   args outs
   pool outs A64EFF:SEQ-SET A64EFF:GPR-WITHOUT
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

\ ---- the convention a Habu word is entered and left through ------------------
\ Design section 7.6: an externally callable Habu word takes argument i out of
\ data-stack slot i of the caller's stack and leaves result j in slot j. Both
\ contracts under it - the leaf and the one that calls - are stated in
\ src/compiler/native/abi.f, because the publication seam has to build the same
\ two and a second copy would be a second convention.
\
\ THE FRAME OF A CALLING ROUTINE IS ONE SLOT AND NOT MORE, so nothing but the
\ return address is in it. A routine of more than one block cannot spill at all,
\ so there is no second claimant on it; a single-block routine that both called
\ and spilled would have the allocator hand slot zero to a value, and the
\ validator refuses the second write to one slot by name. Dot
\ habu-give-the-routine-679de563 gives the frame one owner that reserves the link
\ slot before the allocator places anything.
: LEAF-HABU ( n n n n -- A64EFF:routine )
   NABI:LEAF ;

: CALL-HABU ( n n n n -- A64EFF:routine )
   NABI:CALL ;

\ Allocate registers for a frozen machine module, have the validator accept the
\ allocation, and emit. Nothing here emits from a claim the validator has not
\ agreed with, which is the whole reason the three stages are one word.
: FINISH ( IR-CTX:ctx IR-BUILD:module n n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module base:n n:n :}
   c m base n LEAF-FROM A64RA:ALLOCATE
   m base n LEAF-FROM A64RAV:ACCEPT
   c m A64EMIT:EMIT ;

\ The same three stages under the data-stack convention a Habu word is entered
\ and left through. The contract is built twice, from the same four numbers,
\ because a routine value cannot be held in a local.
: FINISH-HABU ( IR-CTX:ctx IR-BUILD:module n n n n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module base:n n:n in:n out:n :}
   c m base n in out LEAF-HABU A64RA:ALLOCATE
   m base n in out LEAF-HABU A64RAV:ACCEPT
   c m A64EMIT:EMIT ;

\ Select and finish in one step, out of a pool of `n` registers from `base`.
: RUN-FROM ( IR-CTX:ctx IR-BUILD:builder ptr u8 n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n base:n n:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b a u base n LEAF-FROM SELECTED {: m:IR-BUILD:module :}
   c m base n FINISH ;

\ The same, out of the pool that starts at register zero.
: RUN ( IR-CTX:ctx IR-BUILD:builder ptr u8 n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n n:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b a u 0 n RUN-FROM ;

\ Select alone, without allocating or emitting: the step a caller needs when it
\ is measuring what the selector produced rather than what the routine runs as.
: SELECT-HABU ( IR-CTX:ctx IR-BUILD:builder ptr u8 n n n n n -- IR-BUILD:module )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n base:n n:n in:n out:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b a u base n in out LEAF-HABU SELECTED ;

\ Select and finish under the data-stack convention: `in` arguments taken out of
\ slots 0.. of the caller's stack and `out` results left in slots 0.., with `n`
\ scratch registers from `base`. This is the whole of what makes an emitted
\ routine callable the way an interpreted word is.
: RUN-HABU ( IR-CTX:ctx IR-BUILD:builder ptr u8 n n n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n base:n n:n in:n out:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b a u base n in out LEAF-HABU SELECTED {: m:IR-BUILD:module :}
   c m base n in out FINISH-HABU ;

\ Select and finish for a routine that calls itself. The contract is built three
\ times from the same four numbers, because a routine value cannot be held in a
\ local; every one of the three is the same declaration, so the selector, the
\ allocator and the validator are answering about one routine.
: RUN-HABU-CALL ( IR-CTX:ctx IR-BUILD:builder ptr u8 n n n n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a u:n base:n n:n in:n out:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b a u  base n in out CALL-HABU  SELECTED {: m:IR-BUILD:module :}
   c m  base n in out CALL-HABU  A64RA:ALLOCATE
   m  base n in out CALL-HABU  A64RAV:ACCEPT
   c m A64EMIT:EMIT ;

\ The register the returned value ended up in. The last value the module defines
\ is the one the return carries in every straight-line shape, and it is read
\ through the validator rather than off the allocator's raw claim.
: RESULT-REG ( -- n )
   A64RA:VALUES 1- A64RAV:REG@ ;

;package
