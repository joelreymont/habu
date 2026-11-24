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

: T-TIMED-OUT ( str str str -- )
   T-LABEL. SUBJECT:TIMED-OUT ;

\ The failed case for a child that completed by another variant than the one
\ asserted, as `expected exit got signal 9`.
: T-ENDING-MISS ( n str str -- )
   {: code:n want:str got:str :}
   T-NEXT
   T-FAIL
   s" assert: expected " type want type
   s"  got " type got type space code FMT:.INT cr ;

: T-CODE-MISS ( n n str str str -- )
   {: code:n want:n src:str out:str err:str :}
   code want T=
   code want <> if src out err SUBJECT:SHOW then ;

: T-ENDING-EXITED= ( str str str PROCESS:ending n -- )
   {: src:str out:str err:str oc want:n :}
   oc MATCH PROCESS:ending
     exited OF want src out err T-CODE-MISS ENDOF
     signaled OF
       s" exit" s" signal" T-ENDING-MISS src out err SUBJECT:SHOW
     ENDOF
     timeout OF src out err T-TIMED-OUT ENDOF
   ;MATCH ;

: T-RUN-EXITED= ( CMD:command str ms n -- CMD:command )
   {: exe:str deadline:ms status:n :}
   exe deadline CMD:RUN-OUTCOME {: o :}
   CMD:OUT$ {: out:str :}  CMD:ERR$ {: err:str :}
   exe out err o status T-ENDING-EXITED= ;

: T-ENDING-SIGNALED= ( str str str PROCESS:ending n -- )
   {: src:str out:str err:str oc want:n :}
   oc MATCH PROCESS:ending
     exited OF
       s" signal" s" exit" T-ENDING-MISS src out err SUBJECT:SHOW
     ENDOF
     signaled OF want src out err T-CODE-MISS ENDOF
     timeout OF src out err T-TIMED-OUT ENDOF
   ;MATCH ;

: T-ENDING-TIMEOUT ( PROCESS:ending -- )
   MATCH PROCESS:ending
     exited OF s" timeout" s" exit" T-ENDING-MISS ENDOF
     signaled OF s" timeout" s" signal" T-ENDING-MISS ENDOF
     timeout OF 0 0= TTRUE ENDOF
   ;MATCH ;
