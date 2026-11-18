split-str
\ A real C2 owner initializes and clears fixed records before returning bytes.
require src/lib/test.f
require src/lib/memory.f
require src/lib/c2-memory.f
require src/lib/task.f
require src/lib/test/wait.f

\ A package's own `+`, `@` and `!`, defined before a DERIVE init, capture none
\ of the accessors it makes: each reads and writes its own field.
package C2IPCAP
: + ( n n -- n ) - ;
: @ ( ^n -- n ) drop 7 ;
: ! ( n ^n -- ) drop drop ;
public
STRUCTURE cp DERIVE init FIELD first n FIELD second n ;STRUCTURE
private
undefine +
undefine @
undefine !
: CHANGE ( mut-view<p,i,a,init<i,cp>> -- mut-view<p,i,a,init<i,cp>> )
   C2IPCAP-CP:SECOND@ 91 T=  C2IPCAP-CP:FIRST@ 23 T=
   41 C2IPCAP-CP:SECOND!  C2IPCAP-CP:SECOND@ 41 T=
   C2IPCAP-CP:FIRST@ 23 T=  57 C2IPCAP-CP:FIRST!
   C2IPCAP-CP:FIRST@ 57 T=  C2IPCAP-CP:SECOND@ 41 T= ;
: BODY ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> )
   23 91 C2IPCAP-CP:MAKE [: CHANGE ;] C2-MEM:WITH-INIT ;
public
: GO ( -- ) 16 MEM:BYTES-ALLOC-LEN [: BODY ;] C2-MEM:WITH-MUT ;
;package

package C2-INIT-PROGRAM
public
STRUCTURE c2ipair DERIVE init FIELD first n FIELD second n ;STRUCTURE
STRUCTURE c2inest DERIVE init FIELD marker n FIELD pair c2ipair ;STRUCTURE
STRUCTURE c2itrio FIELD first n FIELD second n FIELD third n ;STRUCTURE
STRUCTURE c2iwide DERIVE init FIELD marker n FIELD trio c2itrio ;STRUCTURE

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

: SET-TRIO ( mut-view<p,i,a,init<i,c2iwide>> -- mut-view<p,i,a,init<i,c2iwide>> )
   4 5 6 C2--INIT--PROGRAM-C2ITRIO:MAKE C2--INIT--PROGRAM-C2IWIDE:TRIO!
   C2--INIT--PROGRAM-C2IWIDE:TRIO@ C2--INIT--PROGRAM-C2ITRIO:UNMAKE
   6 T= 5 T= 4 T=  C2--INIT--PROGRAM-C2IWIDE:MARKER@ 7 T= ;

: WIDE ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> )
   7 1 2 3 C2--INIT--PROGRAM-C2ITRIO:MAKE C2--INIT--PROGRAM-C2IWIDE:MAKE
   [: SET-TRIO ;] C2-MEM:WITH-INIT ;

: WIDE-OWNER ( -- )
   32 MEM:BYTES-ALLOC-LEN [: WIDE ;] C2-MEM:WITH-MUT ;

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
   READY 1 INIT-WORKER TEST-WAIT:FOR-CELL
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
   s" a three-cell field at an offset sets and gets every cell" T-LABEL
   WIDE-OWNER
   s" a package's own +, @ and ! capture no accessor" T-LABEL
   C2IPCAP:GO
   T-REPORT
   s" c2-init-program: ok" type cr ;

;package

C2-INIT-PROGRAM:RUN
