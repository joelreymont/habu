\ outcome.f - checked assertions over the process outcome sum.
\ One MATCH per assert so every consumer of the -OUTCOME capture API asserts
\ completion by variant (exit code / signal / timeout) without kind ints, and a
\ child that completed by another variant fails the case under that variant's
\ name.
\ A deadline is no verdict on the child: an assert that wants an exit or a
\ signal takes the program the child ran and the stdout and stderr its capture
\ drained, and on an expired deadline prints them under the case label and
\ throws E-PROC-TIMEOUT instead of failing, so the row reports a timeout
\ (src/lib/process.f, the outcome sum; T-TIMED-OUT below).
\ An exit by another code prints the same program and capture after the failed
\ assertion.
\ T-OUTCOME-TIMEOUT is the assert for a test that expects the deadline.

require src/lib/errors.f
require src/lib/fmt.f
require src/lib/test/assert.f
require src/lib/process.f
require src/lib/test/subject.f

\ SUBJECT:TIMED-OUT with the case label first, for a caller that keeps one.
: T-TIMED-OUT ( ptr u8 n ptr u8 n ptr u8 n -- )
   T-LABEL. SUBJECT:TIMED-OUT ;

\ The failed case for a child that completed by another variant than the one
\ asserted, as `expected exit got signal 9`.
: T-OUTCOME-MISS ( n ptr u8 n ptr u8 n -- )
   {: code:n want:ptr wantu:n got:ptr gotu:n :}
   T-NEXT
   T-FAIL
   s" assert: expected " type want wantu type
   s"  got " type got gotu type space code FMT:.INT cr
   T-LABEL-CLEAR ;

\ The failed case for a child that exited by another code than the one
\ asserted, then the program and the capture that explain it.
: T-EXIT-MISS ( n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: code:n want:n src:ptr srcu:n out:ptr outu:n err:ptr erru:n :}
   code want T=
   code want <> if src srcu out outu err erru SUBJECT:SHOW then ;

\ The exit and signal asserts take the program, its stdout and its stderr, then
\ the outcome and the exit code or signal the case wants.
: T-OUTCOME-EXITED= ( ptr u8 n ptr u8 n ptr u8 n outcome n -- )
   {: src:ptr srcu:n out:ptr outu:n err:ptr erru:n oc want:n :}
   oc MATCH outcome
     exited OF want src srcu out outu err erru T-EXIT-MISS ENDOF
     signaled OF s" exit" s" signal" T-OUTCOME-MISS ENDOF
     timeout OF src srcu out outu err erru T-TIMED-OUT ENDOF
   ;MATCH ;

: T-OUTCOME-SIGNALED= ( ptr u8 n ptr u8 n ptr u8 n outcome n -- )
   {: src:ptr srcu:n out:ptr outu:n err:ptr erru:n oc want:n :}
   oc MATCH outcome
     exited OF s" signal" s" exit" T-OUTCOME-MISS ENDOF
     signaled OF want T= ENDOF
     timeout OF src srcu out outu err erru T-TIMED-OUT ENDOF
   ;MATCH ;

: T-OUTCOME-TIMEOUT ( outcome -- )   \ the deadline passed before a terminal status
   MATCH outcome
     exited OF s" timeout" s" exit" T-OUTCOME-MISS ENDOF
     signaled OF s" timeout" s" signal" T-OUTCOME-MISS ENDOF
     timeout OF 0 0= TTRUE ENDOF
   ;MATCH ;
