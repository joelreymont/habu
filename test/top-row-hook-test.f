\ top-row-hook-test.f - a raw store into COMPILE-PREFLIGHT-CELL or TOP-HOOK-CELL
\ traps, the cells around that band stay writable, and a load leaves the
\ preflight armed.

\ Run: bin/hb --load test/top-row-hook-test.f

require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/subject.f
require src/lib/process.f

package TOP-ROW-HOOK-TEST

: TRH-PREFLIGHT-ARMED ( -- )
   s" load leaves compile-immediate preflight armed" T-LABEL
   data-base COMPILE-PREFLIGHT-CELL + @ 0 <> TTRUE ;

\ ---- child forges: sealed cells ------------------------------------------------
2048 constant TRH-IO-CAP
10000 constant TRH-TIMEOUT-MS

variable TRH-OUT-U
variable TRH-ERR-U
variable TRH-EXITED
variable TRH-RC

create TRH-OUT TRH-IO-CAP allot
create TRH-ERR TRH-IO-CAP allot

: TRH-STORE! ( len len outcome ptr u8 n -- ) {: outu:len erru:len oc src:ptr u:n :}
   erru LEN>N TRH-ERR-U !  outu LEN>N TRH-OUT-U !
   oc MATCH outcome
     exited OF TRH-RC ! 0 0= TRH-EXITED ! ENDOF
     signaled OF TRH-RC ! 0 0= 0= TRH-EXITED ! ENDOF
     timeout OF src u TRH-OUT TRH-OUT-U @ TRH-ERR TRH-ERR-U @ T-TIMED-OUT ENDOF
   ;MATCH ;

: TRH-RUN-SUBJECT ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu TRH-OUT TRH-IO-CAP >LEN TRH-ERR TRH-IO-CAP >LEN
   TRH-TIMEOUT-MS >MS SUBJECT:RUN src srcu TRH-STORE! ;

: TRH-ASSERT-OK ( -- )
   TRH-EXITED @ TTRUE
   TRH-RC @ 0 T= ;

: TRH-NEG-SEAL ( -- )
   s" raw ! into COMPILE-PREFLIGHT-CELL traps ENGINE-ERROR:SEAL-VIOLATION" T-LABEL
   s" data-base COMPILE-PREFLIGHT-CELL + 99 swap !" TRH-RUN-SUBJECT
   TRH-EXITED @ TTRUE
   TRH-RC @ ENGINE-ERROR:SEAL-VIOLATION T=
   s" raw ! into TOP-HOOK-CELL traps ENGINE-ERROR:SEAL-VIOLATION" T-LABEL
   s" data-base TOP-HOOK-CELL + 99 swap !" TRH-RUN-SUBJECT
   TRH-EXITED @ TTRUE
   TRH-RC @ ENGINE-ERROR:SEAL-VIOLATION T=
   s" one cell below the band stays writable" T-LABEL
   s" data-base COMPILE-PREFLIGHT-CELL 8 - + 99 swap !" TRH-RUN-SUBJECT
   TRH-ASSERT-OK
   s" one cell past the band stays writable" T-LABEL
   s" data-base TOP-HOOK-CELL 8 + + 99 swap !" TRH-RUN-SUBJECT
   TRH-ASSERT-OK ;

: TRH-MAIN ( -- )
   T-RESET
   TRH-PREFLIGHT-ARMED
   TRH-NEG-SEAL
   T-REPORT
   s" top-row-hook: ok" type cr ;

TRH-MAIN

;package
