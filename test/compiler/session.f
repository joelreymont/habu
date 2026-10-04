\ session.f - legacy compiler admission and cleanup through real tasks.

require lib/test.f
require lib/task.f
require src/compiler/session/lease.f
require src/compiler/native/backend.f
require src/compiler/session/emission.f
require test/compiler/native-source-fixture.f
require test/compiler/native-chain-fixture.f
require src/arch/x86-64/passes.f
require lib/fs.f

: COMPILER-SESSION-EDGE ( n -- n ) abs ;
: COMPILER-SESSION-PROOF ( n -- n [ -- n ] )
   dup 0< if negate else 3 + then COMPILER-SESSION-EDGE [: 7 ;] ;

package COMPILER-SESSION-TEST
private

TYPED-VARIABLE SAVED NLEASE:lease
TYPED-VARIABLE WRONG NSESSION:session
TYPED-VARIABLE LIVE NSESSION:session
TYPED-VARIABLE PARENT-HIR IR-BUILD:module
TYPED-VARIABLE CHILD-CTX IR-CTX:ctx
TYPED-VARIABLE CHILD-ART NART:emission
variable SLOT
variable FAR
variable ENTERED
TASK:MIN-STACK TASK:TASK WORKER

: ENTER ( -- )
   [: drop 1 ENTERED +! ;] NLEASE:WITH ;

: BAD-WORK ( -- )
   -73 throw ;

: WORK-CHILD ( -- )
   SAVED @ [: 1 ENTERED +! ;] NLEASE:WORK ;

: NEST-WORK ( -- )
   [: WORK-CHILD ;] NLEASE:E-BUSY TTHROWSQ
   [: NLEASE:IDLE-CK ;] NLEASE:E-BUSY TTHROWSQ
   SAVED @ NLEASE:WORK-CK ;

: NESTED ( NLEASE:lease -- )
   dup SAVED ! NLEASE:CHECK
   [: ENTER ;] NLEASE:E-BUSY TTHROWSQ
   SAVED @ NLEASE:CHECK
   SAVED @ [: NEST-WORK ;] NLEASE:WORK ;

: NESTED-REFUSAL ( -- )
   [: NESTED ;] NLEASE:WITH ;

: THROW-WORK ( NLEASE:lease -- )
   dup SAVED ! [: BAD-WORK ;] NLEASE:WORK ;

: THROW-ROOT ( -- )
   [: THROW-WORK ;] NLEASE:WITH ;

: CHECK-SAVED ( -- )
   SAVED @ NLEASE:CHECK ;

: STALE-IN-NEW ( NLEASE:lease -- )
   NLEASE:CHECK
   [: CHECK-SAVED ;] NLEASE:E-STATE TTHROWSQ ;

: TASK-ENTRY ( -- )
   [: ENTER ;] catch TASK:RETURN ;

: JOIN-UNWRAP ( result<n,n> -- n n )
   MATCH result ok OF 0 ENDOF err OF 1 ENDOF ;MATCH ;

: TASK-REFUSAL ( NLEASE:lease -- )
   SAVED !
   ['] TASK-ENTRY WORKER TASK:ACTIVATE
   WORKER TASK:JOIN JOIN-UNWRAP
   0 T= NLEASE:E-TASK T=
   SAVED @ NLEASE:CHECK ;

: WRONG-PROVIDER ( -- )
   WRONG @ NSESSION:RESOLVE drop drop ;

: PROVIDER-WORK ( NSESSION:session -- )
   NSESSION-SESSION:UNMAKE
   {: c:IR-CTX:ctx id:CTARGET:backend-id l:NLEASE:lease :}
   c X64BACK:ID l NSESSION-SESSION:MAKE WRONG !
   [: WRONG-PROVIDER ;] NLEASE:E-STATE TTHROWSQ
   c id l NSESSION-SESSION:MAKE NSESSION:RESOLVE drop drop ;

: PROVIDER-CTX ( IR-CTX:ctx -- )
   SAVED @ NSESSION:NEW [: PROVIDER-WORK ;] NSESSION:WITH-WORK ;

: PROVIDER-ROOT ( NLEASE:lease -- )
   SAVED !
   NFIX:BINDING [: PROVIDER-CTX ;] IR-CTX:WITH-CONTEXT ;

\ The same source carries arithmetic, both branches, a real callable and a
\ quotation. Each context elaborates its own HIR under its own binding.
: PROOF ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   s" COMPILER-SESSION-PROOF dup 0< if negate else 3 + then COMPILER-SESSION-EDGE [: 7 ;]" NSRC:TEXT!
   c NSRC:HIR-BUILDER {: b:IR-BUILD:builder :}
   c b NSRC:TAPE {: tape:IR-ARENA:arena :}
   c NSRC:LEX
   tape NTAPE:SEAL {: sealed:IR-ARENA:view :}
   c b sealed NTAPE:TOKENS NSRC:MODEL-ROOM
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   FAR @ if
      c b r c b s" COMPILER-SESSION-EDGE" IR-BUILD:INTERN-SYMBOL
      SLOT @ $100000000 + 1 1 0 false HIR-WORD:DECLARE-BOUND-CALLABLE
   then
   c b sealed p r 1 2 NELAB:COLON drop
   b ;

: BACKEND-CLOSE ( -- )
   LIVE @ NBACK:RELEASE
   LIVE @ NBACK:RETIRE ;

: CHILD-STAGE ( NSESSION:session -- )
   NSESSION:RESOLVE drop drop
   1 ENTERED +! ;

: NESTED-CTX ( IR-CTX:ctx -- )
   dup 8 IR-CTX:SCRATCH-TAKE drop drop
   SAVED @ NSESSION:NEW [: CHILD-STAGE ;] NSESSION:WITH-WORK ;

: NESTED-BACKEND ( -- )
   NFIX:BINDING [: NESTED-CTX ;] IR-CTX:WITH-CONTEXT ;

: OTHER-BINDING ( -- CBIND:binding )
   LIVE @ NSESSION:RESOLVE drop IR-CTX:BINDING@ CBIND:TARGET@
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:ASSUME-ORDERED CNUM:POLICY CBIND:BIND ;

\ A second live context can share the emitted machine but not its work owner.
: FOREIGN-COPY ( IR-CTX:ctx -- )
   {: other:IR-CTX:ctx :}
   other IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:ARCH@
   LIVE @ NSESSION:RESOLVE drop IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:ARCH@
   CTARGET-ARCH:EQ TTRUE
   other IR-CTX:BINDING@ CBIND:POLICY@
   LIVE @ NSESSION:RESOLVE drop IR-CTX:BINDING@ CBIND:POLICY@
   CNUM:SAME? TFALSE
   LIVE @ NSESSION-SESSION:UNMAKE
   {: active:IR-CTX:ctx id:CTARGET:backend-id l:NLEASE:lease :}
   active IR-CTX:SERIAL other IR-CTX:SERIAL <> TTRUE
   other id l NSESSION-SESSION:MAKE NART:COPY NART:RELEASE ;

: FOREIGN-CTX ( -- )
   OTHER-BINDING [: FOREIGN-COPY ;] IR-CTX:WITH-CONTEXT ;

: PROOF-EMIT ( NSESSION:session -- NART:emission )
   dup LIVE !
   {: s:NSESSION:session :}
   [: NESTED-BACKEND ;] NLEASE:E-BUSY TTHROWSQ
   ENTERED @ 0 T=
   s NSESSION:RESOLVE drop {: c:IR-CTX:ctx :}
   c PROOF {: b:IR-BUILD:builder :}
   s 1 2 NBACK:L-CALLED NBACK:DECLARE
   s b NBACK:FREEZE {: hm:IR-BUILD:module :}
   s hm NBACK:SELECT {: m0:IR-BUILD:module :}
   hm IR-BUILD:RETIRE
   s m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   s m1 NBACK:FIXPOINT {: ready:IR-BUILD:module :}
   s ready SLOT @ NBACK:EMIT
   [: FOREIGN-CTX ;] NLEASE:E-STATE TTHROWSQ
   s NART:COPY ;

: COMPILED ( NSESSION:session -- NART:emission )
   [: PROOF-EMIT ;] [: BACKEND-CLOSE ;] finally ;

: COPY-PROOF ( IR-CTX:ctx -- NART:emission )
   SAVED @ NSESSION:NEW [: COMPILED ;] NSESSION:WITH-WORK ;

: PARENT-FREEZE ( NSESSION:session -- )
   {: s:NSESSION:session :}
   s NSESSION:RESOLVE drop PROOF
   s swap NBACK:FREEZE PARENT-HIR ! ;

: RETAIN-HIR ( IR-CTX:ctx -- )
   SAVED @ NSESSION:NEW [: PARENT-FREEZE ;] NSESSION:WITH-WORK ;

: EMIT-RETAINED ( NSESSION:session -- NART:emission )
   dup LIVE !
   {: s:NSESSION:session :}
   s 1 2 NBACK:L-CALLED NBACK:DECLARE
   s PARENT-HIR @ NBACK:SELECT {: m0:IR-BUILD:module :}
   s m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   s m1 NBACK:FIXPOINT {: ready:IR-BUILD:module :}
   s ready SLOT @ NBACK:EMIT
   s NART:COPY ;

: COMPILED-RETAINED ( NSESSION:session -- NART:emission )
   [: EMIT-RETAINED ;] [: BACKEND-CLOSE ;] finally ;

: COPY-RETAINED ( IR-CTX:ctx -- NART:emission )
   SAVED @ NSESSION:NEW [: COMPILED-RETAINED ;] NSESSION:WITH-WORK ;

: FORGE-REFUSAL ( -- )
   s" FORGED ( IR-ARENA:view -- NART:emission ) NART-KEY:MAKE NART-EMISSION:MAKE"
      CHECK-QUIET-CANDIDATE! 1 T=
   s" FORGED ( IR-ARENA:view -- NART:emission ) NART:KEY-MAKE NART-EMISSION:MAKE"
      CHECK-QUIET-CANDIDATE! 1 T= ;

: ARTIFACT= ( NART:emission NART:emission -- )
   {: a:NART:emission b:NART:emission :}
   a NART:BYTES a NART:SIZE b NART:BYTES b NART:SIZE T$=
   a NART:BINDING b NART:BINDING CBIND:SAME? TTRUE
   a NART:PLACEMENT b NART:PLACEMENT T=
   a NART:RET-BYTES b NART:RET-BYTES T=
   a NART:FUNCTIONS b NART:FUNCTIONS T=
   a NART:CALL-SITES b NART:CALL-SITES T=
   a NART:ADDR-SITES b NART:ADDR-SITES T=
   a NART:FUNCTIONS 0 ?do
      a i NART:FUNCTION-OFFSET@ b i NART:FUNCTION-OFFSET@ T=
   loop
   a NART:CALL-SITES 0 ?do
      a i NART:CALL-SITE@ b i NART:CALL-SITE@ T=
      a i NART:CALL-KIND@ b i NART:CALL-KIND@ T=
      a i NART:CALL-TARGET@ b i NART:CALL-TARGET@ T=
   loop
   a NART:ADDR-SITES 0 ?do
      a i NART:ADDR-SITE@ b i NART:ADDR-SITE@ T=
      a i NART:ADDR-SITE-KIND@ b i NART:ADDR-SITE-KIND@ T=
   loop ;

: X64-PROOF ( IR-CTX:ctx -- )
   dup CHILD-CTX !
   COPY-PROOF dup CHILD-ART !
   {: e:NART:emission :}
   e NART:ARCH CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ TTRUE
   e NART:FUNCTIONS 2 T=
   e NART:CALL-SITES 0 > TTRUE
   e NART:ADDR-SITES 1 T=
   e 0 NART:ADDR-SITE-KIND@ X64IR:ADDR-CODE T= ;

: FAIL-X64 ( IR-CTX:ctx -- )
   dup CHILD-CTX !
   dup COPY-PROOF CHILD-ART !
   -1 FAR !
   COPY-PROOF drop ;

: CHILD-REFUSAL ( -- )
   X64ABI:BINDING [: FAIL-X64 ;] IR-CTX:WITH-CONTEXT ;

: STALE-ARTIFACT ( -- )
   CHILD-ART @ NART:BYTES drop ;

: X64-BINDING ( -- CBIND:binding )
   X64ABI:BINDING CBIND:TARGET@
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:TOTAL-ORDER CNUM:POLICY CBIND:BIND ;

: PARENT-PROOF ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c RETAIN-HIR
   c COPY-PROOF {: first:NART:emission :}
   first NART:FUNCTIONS 2 T=
   first NART:CALL-SITES 0 > TTRUE
   first NART:ARCH CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ TTRUE
   X64-BINDING CBIND:POLICY@ c IR-CTX:BINDING@ CBIND:POLICY@ CNUM:SAME? TFALSE
   X64-BINDING [: X64-PROOF ;] IR-CTX:WITH-CONTEXT
   CHILD-CTX @ IR-CTX:LIVE? TFALSE
   [: STALE-ARTIFACT ;] E-IR-ARENA-STALE TTHROWSQ
   c COPY-PROOF first ARTIFACT=
   [: CHILD-REFUSAL ;] E-X64EMIT-REACH TTHROWSQ
   0 FAR !
   CHILD-CTX @ IR-CTX:LIVE? TFALSE
   [: STALE-ARTIFACT ;] E-IR-ARENA-STALE TTHROWSQ
   c COPY-RETAINED first ARTIFACT=
   c COPY-PROOF first ARTIFACT=
   s" compiler-session-a64.bin" TMP-PATH first NART:BYTES first NART:SIZE WRITE-ALL
   first CHILD-ART !
   first NART:RELEASE
   [: STALE-ARTIFACT ;] E-IR-ARENA-STALE TTHROWSQ ;

: CROSS-ROOT ( NLEASE:lease -- )
   SAVED !
   0 ENTERED !
   0 FAR !
   NPUB:NEXT-SLOT X64IR:SP-ALIGN 1- + X64IR:SP-ALIGN /
   X64IR:SP-ALIGN * SLOT !
   NFIX:BINDING [: PARENT-PROOF ;] IR-CTX:WITH-CONTEXT ;

public

: RUN ( -- )
   T-RESET
   s" nested root/work admission refuses before running the child" T-LABEL
   0 ENTERED !
   NESTED-REFUSAL
   ENTERED @ 0 T=
   NLEASE:IDLE-CK

   s" throws release work and root; saved leases cannot authorize later work" T-LABEL
   [: THROW-ROOT ;] -73 TTHROWSQ
   [: CHECK-SAVED ;] NLEASE:E-STATE TTHROWSQ
   [: STALE-IN-NEW ;] NLEASE:WITH
   ENTER
   ENTERED @ 1 T=

   s" a real task cannot enter the legacy compiler realm" T-LABEL
   [: TASK-REFUSAL ;] NLEASE:WITH
   ENTER
   ENTERED @ 2 T=
   NLEASE:IDLE-CK

   s" a forged provider cannot select another architecture's live passes" T-LABEL
   [: PROVIDER-ROOT ;] NLEASE:WITH

   s" AArch64, x86-64 and AArch64 retain exact bytes and rows after a failed child" T-LABEL
   [: CROSS-ROOT ;] NLEASE:WITH

   s" checked callers cannot assemble an emission from independent fields" T-LABEL
   FORGE-REFUSAL ;

;package

COMPILER-SESSION-TEST:RUN
T-REPORT
s" test:ok" type cr
