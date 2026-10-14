\ More than 129 C entry points, including an entry saved before table growth.
1 set-tier
require src/lib/test.f
require src/lib/ffi-abi.f
require src/lib/ffi-callback.f
require src/lib/task.f
require src/lib/codegen.f
require src/lib/adt/result.f

package FFI-CB-GROWTH-TEST
private

PROCESS-SYMBOLS
FUNCTION: QSORT qsort ( ptr u8 n n n -- )
   0 32 WRITES-BYTES
;FUNCTION

4 constant ROW-N
create ROW ROW-N cells allot
variable EARLY-FN
variable LATE-FN
128 CODEGEN:BUFFER GEN
TASK:MIN-STACK TASK:TASK HOST
TASK:MIN-STACK TASK:TASK WORKER

CALLBACK: EARLY ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK

: ORDER ( ptr u8 ptr u8 -- n ) {: a b :}
   a CELL-VIEW @ b CELL-VIEW @ - ;

' ORDER EARLY-BODY !

: PAD ( n -- ) {: k:n :}
   GEN CODEGEN:RESET
   s" CALLBACK: PAD-" GEN CODEGEN:APPEND-STRING
   k GEN CODEGEN:APPEND-DECIMAL
   s"  ( -- ) ;CALLBACK" GEN CODEGEN:APPEND-STRING
   GEN CODEGEN:CONTENTS INCLUDE-EVALUATE ;

: FILL ( -- )
   130 0 ?do i PAD loop ;

: BIND-EARLY ( -- )
   EARLY TASK:SELF-CONTEXT FFI-CB:ENTRY EARLY-FN ! ;

BIND-EARLY
FILL

CALLBACK: LATE ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
' ORDER LATE-BODY !
CALLBACK: FAULTY ( ptr u8 ptr u8 -- i32 ) -777 FALLBACK ;CALLBACK

-9917 constant FAULT-CODE
131 constant LATE-K
4 cells constant BINDING-ROW-BYTES

: FAIL ( ptr u8 ptr u8 -- n ) 2drop FAULT-CODE throw ;
' FAIL FAULTY-BODY !

: SORT ( n -- bool ) {: fn:n :}
   4 ROW ! 3 ROW cell+ ! 2 ROW 2 cells + ! 1 ROW 3 cells + !
   ROW BYTE-VIEW ROW-N CELL fn QSORT
   true ROW-N 0 ?do ROW i cells + @ i 1+ = and loop ;

: SLOT-CELL ( n n -- ptr n ) {: k:n off:n :}
   TASK:MAIN-BASE {: base:ptr :}
   base CB-ROWS + atomic@ base FFI:>CELL - {: table:n :}
   base table + k BINDING-ROW-BYTES * + off + ;

: HOLD-LATE ( -- )
   0 CB-ROW-BUSY LATE-K CB-ROW-OWNER SLOT-CELL atomic-cas 0= TTRUE ;

: RELEASE-LATE ( -- )
   0 LATE-K CB-ROW-OWNER SLOT-CELL atomic! ;

: EXPOSED-ROWS ( -- )
   HOST TASK:EXPOSE
   EARLY HOST TASK:CONTEXT FFI-CB:ENTRY drop
   LATE HOST TASK:CONTEXT FFI-CB:ENTRY drop
   HOLD-LATE
   [: HOST TASK:UNEXPOSE ;] catch E-TASK-STATE = TTRUE
   HOST TASK:CONTEXT 0 CB-ROW-REGION SLOT-CELL atomic@ = TTRUE
   HOST TASK:CONTEXT LATE-K CB-ROW-REGION SLOT-CELL atomic@ = TTRUE
   RELEASE-LATE
   HOST TASK:UNEXPOSE
   0 CB-ROW-REGION SLOT-CELL atomic@ 0= TTRUE
   LATE-K CB-ROW-REGION SLOT-CELL atomic@ 0= TTRUE
   HOST TASK:KILL ;

: WORK-ENDS-BOUND ( -- )
   LATE TASK:SELF-CONTEXT FFI-CB:ENTRY drop
   17 TASK:RETURN ;

: WORKER-ROW ( -- )
   ['] WORK-ENDS-BOUND WORKER TASK:ACTIVATE
   WORKER TASK:JOIN MATCH result
      ok OF 17 T= ENDOF
      err OF drop false TTRUE ENDOF
   ;MATCH
   LATE TASK:SELF-CONTEXT FFI-CB:ENTRY LATE-FN !
   LATE-FN @ SORT TTRUE
   LATE FFI-CB:UNBIND ;

: THROW-FALLBACK ( -- )
   FAULTY TASK:SELF-CONTEXT FFI-CB:ENTRY {: fn:n :}
   2 ROW ! 1 ROW cell+ !
   ROW BYTE-VIEW 2 CELL fn QSORT
   FAULTY FFI-CB:FAULT@ FAULT-CODE = TTRUE
   FAULTY FFI-CB:UNBIND ;

public

: RUN ( -- )
   LATE TASK:SELF-CONTEXT FFI-CB:ENTRY LATE-FN !
   EARLY-FN @ SORT TTRUE
   LATE-FN @ SORT TTRUE
   EARLY FFI-CB:UNBIND
   LATE FFI-CB:UNBIND
   EXPOSED-ROWS
   WORKER-ROW
   THROW-FALLBACK
   s" ffi-callback-growth: ok" type cr ;

;package

FFI-CB-GROWTH-TEST:RUN
