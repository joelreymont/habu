\ ending.f - checked assertions over the process ending enum.
\ A deadline is no verdict on the child: on an expired one an exit or signal
\ assert prints the program and its capture under the case label and throws
\ E-PROC-TIMEOUT; T-ENDING-TIMEOUT expects the deadline.
\ STORAGE CLASS. CALLER-OWNED: no module state.

require src/lib/error.f
require src/lib/fmt.f
require src/lib/test/assert.f
require src/lib/process.f
require src/lib/process-command.f
require src/lib/test/subject.f

: T-TIMED-OUT ( ptr u8 n ptr u8 n ptr u8 n -- )
   T-LABEL. SUBJECT:TIMED-OUT ;

\ The failed case for a child that completed by another variant than the one
\ asserted, as `expected exit got signal 9`.
: T-ENDING-MISS ( n ptr u8 n ptr u8 n -- )
   {: code:n want:ptr wantu:n got:ptr gotu:n :}
   T-NEXT
   T-FAIL
   s" assert: expected " type want wantu type
   s"  got " type got gotu type space code FMT:.INT cr ;

: T-CODE-MISS ( n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: code:n want:n src:ptr srcu:n out:ptr outu:n err:ptr erru:n :}
   code want T=
   code want <> if src srcu out outu err erru SUBJECT:SHOW then ;

: T-ENDING-EXITED= ( ptr u8 n ptr u8 n ptr u8 n PROCESS:ending n -- )
   {: src:ptr srcu:n out:ptr outu:n err:ptr erru:n oc want:n :}
   oc MATCH PROCESS:ending
     exited OF want src srcu out outu err erru T-CODE-MISS ENDOF
     signaled OF
       s" exit" s" signal" T-ENDING-MISS src srcu out outu err erru SUBJECT:SHOW
     ENDOF
     timeout OF src srcu out outu err erru T-TIMED-OUT ENDOF
   ;MATCH ;

: T-RUN-EXITED= ( CMD:command ptr u8 n ms n -- CMD:command )
   {: exe:ptr xu:n deadline:ms status:n :}
   exe xu deadline CMD:RUN-OUTCOME {: o :}
   CMD:OUT$ {: out:ptr ou:n :}  CMD:ERR$ {: err:ptr eu:n :}
   exe xu out ou err eu o status T-ENDING-EXITED= ;

: T-ENDING-SIGNALED= ( ptr u8 n ptr u8 n ptr u8 n PROCESS:ending n -- )
   {: src:ptr srcu:n out:ptr outu:n err:ptr erru:n oc want:n :}
   oc MATCH PROCESS:ending
     exited OF
       s" signal" s" exit" T-ENDING-MISS src srcu out outu err erru SUBJECT:SHOW
     ENDOF
     signaled OF want src srcu out outu err erru T-CODE-MISS ENDOF
     timeout OF src srcu out outu err erru T-TIMED-OUT ENDOF
   ;MATCH ;

: T-ENDING-TIMEOUT ( PROCESS:ending -- )
   MATCH PROCESS:ending
     exited OF s" timeout" s" exit" T-ENDING-MISS ENDOF
     signaled OF s" timeout" s" signal" T-ENDING-MISS ENDOF
     timeout OF 0 0= TTRUE ENDOF
   ;MATCH ;
