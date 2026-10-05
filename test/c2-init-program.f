\ A real C2 owner initializes and clears fixed records before returning bytes.
require lib/test.f
require lib/memory.f
require lib/c2-memory.f
require lib/task.f

package C2-INIT-PROGRAM
public
STRUCTURE c2ipair 0 DERIVE init FIELD first n FIELD second n ;STRUCTURE
STRUCTURE c2inest 0 DERIVE init FIELD marker n FIELD pair c2ipair ;STRUCTURE

private

TASK:MIN-STACK TASK:TASK INIT-WORKER
variable READY

: CHANGE ( n mut-view<p,i,a,init<i,c2ipair>> -- n mut-view<p,i,a,init<i,c2ipair>> )
   C2--INIT--PROGRAM-C2IPAIR:FIRST@ 23 <> if -7302 throw then
   C2--INIT--PROGRAM-C2IPAIR:SECOND@ 91 <> if -7303 throw then
   41 C2--INIT--PROGRAM-C2IPAIR:FIRST!
   C2--INIT--PROGRAM-C2IPAIR:FIRST@ 41 <> if -7304 throw then
   swap 1+ swap ;

: BODY ( n mut-view<p,l,a,u8> -- bool mut-view<p,l,a,u8> )
   20 77 C2-MEM:MUT-BYTE!
   23 91 C2--INIT--PROGRAM-C2IPAIR:MAKE [: CHANGE ;] C2-MEM:WITH-INIT
   swap 6 = >r
   0 C2-MEM:MUT-BYTE@ swap 0= >r
   8 C2-MEM:MUT-BYTE@ swap 0= r> and r> and
   >r 20 C2-MEM:MUT-BYTE@ swap 77 = r> and swap ;

: RESULT ( -- bool )
   5 24 MEM:BYTES-ALLOC-LEN [: BODY ;] C2-MEM:WITH-MUT ;

: CHECK-NESTED ( mut-view<p,i,a,init<i,c2inest>> -- mut-view<p,i,a,init<i,c2inest>> )
   C2--INIT--PROGRAM-C2INEST:MARKER@ 7 T=
   C2--INIT--PROGRAM-C2INEST:PAIR@ C2--INIT--PROGRAM-C2IPAIR:UNMAKE swap
   8 T= 9 T= ;

: NESTED ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> )
   7 8 9 C2--INIT--PROGRAM-C2IPAIR:MAKE C2--INIT--PROGRAM-C2INEST:MAKE
   [: CHECK-NESTED ;] C2-MEM:WITH-INIT ;

: NESTED-RESULT ( -- n )
   24 MEM:BYTES-ALLOC-LEN [: NESTED ;] C2-MEM:WITH-MUT 1 ;

: SHORT ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> )
   1 2 C2--INIT--PROGRAM-C2IPAIR:MAKE [: ;] C2-MEM:WITH-INIT ;

: SHORT-OWNER ( -- )
   8 MEM:BYTES-ALLOC-LEN [: SHORT ;] C2-MEM:WITH-MUT ;

: SHORT-RESULT ( -- bool )
   ['] SHORT-OWNER catch E-SPAN-CAPACITY = ;

: FAIL ( mut-view<p,i,a,init<i,c2ipair>> -- mut-view<p,i,a,init<i,c2ipair>> )
   -7301 throw ;

: THROW-OWNER ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> )
   3 4 C2--INIT--PROGRAM-C2IPAIR:MAKE [: FAIL ;] C2-MEM:WITH-INIT ;

: THROW-CALL ( -- )
   16 MEM:BYTES-ALLOC-LEN [: THROW-OWNER ;] C2-MEM:WITH-MUT ;

: THROW-RESULT ( -- bool )
   ['] THROW-CALL catch -7301 = ;

: PAUSE-INSIDE ( mut-view<p,i,a,init<i,c2ipair>> -- mut-view<p,i,a,init<i,c2ipair>> )
   1 READY atomic!
   begin TASK:PAUSE again ;

: HALT-OWNER ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> )
   5 6 C2--INIT--PROGRAM-C2IPAIR:MAKE [: PAUSE-INSIDE ;] C2-MEM:WITH-INIT ;

: HALT-BODY ( -- )
   16 MEM:BYTES-ALLOC-LEN [: HALT-OWNER ;] C2-MEM:WITH-MUT ;

: HALT-RESULT ( -- bool )
   0 READY atomic!
   ['] HALT-BODY INIT-WORKER TASK:ACTIVATE
   begin READY atomic@ 0= while TASK:PAUSE repeat
   INIT-WORKER TASK:HALT
   INIT-WORKER TASK:JOIN
   MATCH result
      ok OF drop false ENDOF
      err OF E-TASK-NO-RESULT = ENDOF
   ;MATCH ;

public

: RUN ( -- )
   T-RESET
   s" a two-cell value initializes a byte view and clears before restoration" T-LABEL
   RESULT TTRUE
   s" a nested three-cell fixed record uses its complete width" T-LABEL
   NESTED-RESULT 1 T=
   s" initialization refuses a byte bound shorter than its record" T-LABEL
   SHORT-RESULT TTRUE
   s" callback failure closes initialized storage" T-LABEL
   THROW-RESULT TTRUE
   s" task halt drains initialized and allocation frames" T-LABEL
   HALT-RESULT TTRUE
   T-REPORT
   s" c2-init-program: ok" type cr ;

;package

C2-INIT-PROGRAM:RUN
