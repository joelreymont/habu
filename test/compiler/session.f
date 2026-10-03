\ session.f - legacy compiler admission and cleanup through real tasks.

require lib/test.f
require lib/task.f
require src/compiler/session/lease.f
require src/compiler/session/backend.f
require test/compiler/native-chain-fixture.f
require src/arch/x86-64/passes.f

package COMPILER-SESSION-TEST
private

TYPED-VARIABLE SAVED NLEASE:lease
TYPED-VARIABLE WRONG NSESSION:session
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

public

: RUN ( -- )
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
   [: PROVIDER-ROOT ;] NLEASE:WITH ;

;package

COMPILER-SESSION-TEST:RUN
s" test:ok" type cr
