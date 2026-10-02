\ A checked owner releases a foreign acquisition through its supplied disposer.
require lib/test.f
require lib/c2-owner.f
require lib/memory.f
require lib/task.f
require lib/fmt.f

package C2-OWNER-DISPOSE
private

20 PTR-U8-TABLE FOREIGN
20 TYPED-BUFFER FOREIGN-LEN NUM:alloc-byte-len
20 TYPED-BUFFER CALLS n
20 TYPED-BUFFER ORDER n
20 TYPED-BUFFER CHUNK-LENGTH n
variable RELEASES
variable EMPTY-CALLS
variable READY
variable GO
variable CAP-CALLS
variable RESUMED
TASK:MIN-STACK TASK:TASK WORKER
TASK:MIN-STACK TASK:TASK WORKER-B
TASK:MIN-STACK TASK:TASK WORKER-C

: FOREIGN-CELL ( n -- ptr ptr u8 ) cells FOREIGN + 0 ptr-field ;

: ACQUIRE ( n -- ) {: id:n :}
   8 MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: p:ptr size:NUM:alloc-byte-len :}
   id p c!
   p id FOREIGN-CELL !
   size id FOREIGN-LEN ! ;

: RELEASE ( n -- ) {: id:n :}
   id CALLS @ 0<> if -9365 throw then
   id FOREIGN-CELL @ {: p:ptr :}
   p c@ id T=
   p id FOREIGN-LEN @ MEM:RELEASE-BYTES
   1 id CALLS +!
   id RELEASES @ ORDER !
   1 RELEASES +! ;

: DISPOSE ( mut-view<p,p,a,u8> -- )
   0 C2-MEM:MUT-BYTE@ swap >r
   C2-MEM:PUBLISH drop
   r> RELEASE ;

: DISPOSE-FAIL ( mut-view<p,p,a,u8> -- )
   DISPOSE -9362 throw ;

: DISPOSE-PAUSE ( mut-view<p,p,a,u8> -- )
   1 READY atomic!
   begin GO atomic@ 0= while TASK:PAUSE repeat
   DISPOSE ;

: DISPOSE-PAUSE-FAIL ( mut-view<p,p,a,u8> -- )
   DISPOSE-PAUSE -9362 throw ;

: DISPOSE-EMPTY ( mut-view<p,p,a,u8> -- )
   0 C2-MEM:MUT-BYTE@ swap 0 T=
   C2-MEM:PUBLISH drop
   1 EMPTY-CALLS +! ;

: DISPOSE-CHUNK ( mut-view<p,p,a,u8> -- )
   0 C2-MEM:MUT-BYTE@ swap >r
   r@ CHUNK-LENGTH @ 1- C2-MEM:MUT-BYTE@ swap 0 T=
   C2-MEM:PUBLISH drop
   r> RELEASE ;

: DISPOSE-CHUNK-FAIL ( mut-view<p,p,a,u8> -- )
   DISPOSE-CHUNK -9362 throw ;

: DISPOSE-CHUNK-PAUSE ( mut-view<p,p,a,u8> -- )
   1 READY atomic!
   begin GO atomic@ 0= while TASK:PAUSE repeat
   DISPOSE-CHUNK ;


: MARK ( C2-MEM:owner<p,i,a> mut-view<p,p,b,u8> n -- C2-MEM:owner<p,i,a> )
   >r
   0 C2-MEM:MUT-BYTE@ swap 0 T=
   0 r> C2-MEM:MUT-BYTE! C2-MEM:PUBLISH drop ;

: ALLOC-ID ( C2-MEM:owner<p,i,a> n -- C2-MEM:owner<p,i,a> )
   >r 16 MEM:BYTES-ALLOC-LEN [: DISPOSE ;] C2-MEM:ALLOC-DISPOSE r> MARK ;

: ALLOC-FAIL-ID ( C2-MEM:owner<p,i,a> n -- C2-MEM:owner<p,i,a> )
   >r 16 MEM:BYTES-ALLOC-LEN [: DISPOSE-FAIL ;] C2-MEM:ALLOC-DISPOSE r> MARK ;

: ALLOC-PAUSE-ID ( C2-MEM:owner<p,i,a> n -- C2-MEM:owner<p,i,a> )
   >r 16 MEM:BYTES-ALLOC-LEN [: DISPOSE-PAUSE ;] C2-MEM:ALLOC-DISPOSE r> MARK ;

: ALLOC-PAUSE-FAIL-ID ( C2-MEM:owner<p,i,a> n -- C2-MEM:owner<p,i,a> )
   >r 16 MEM:BYTES-ALLOC-LEN [: DISPOSE-PAUSE-FAIL ;] C2-MEM:ALLOC-DISPOSE r> MARK ;

: ALLOC-CHUNK-ID ( C2-MEM:owner<p,i,a> n n -- C2-MEM:owner<p,i,a> )
   {: id:n bytes:n :}
   bytes MEM:BYTES-ALLOC-LEN [: DISPOSE-CHUNK ;] C2-MEM:ALLOC-DISPOSE
   bytes id CHUNK-LENGTH !
   id MARK ;

: ALLOC-CHUNK-FAIL-ID ( C2-MEM:owner<p,i,a> n n -- C2-MEM:owner<p,i,a> )
   {: id:n bytes:n :}
   bytes MEM:BYTES-ALLOC-LEN [: DISPOSE-CHUNK-FAIL ;] C2-MEM:ALLOC-DISPOSE
   bytes id CHUNK-LENGTH !
   id MARK ;

: ALLOC-CHUNK-PAUSE-ID ( C2-MEM:owner<p,i,a> n n -- C2-MEM:owner<p,i,a> )
   {: id:n bytes:n :}
   bytes MEM:BYTES-ALLOC-LEN [: DISPOSE-CHUNK-PAUSE ;] C2-MEM:ALLOC-DISPOSE
   bytes id CHUNK-LENGTH !
   id MARK ;

: BODY-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND 0 ACQUIRE 0 ALLOC-ID C2-MEM:UNBIND ;

: BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: BODY-INIT ;] C2-MEM:WITH-INIT ;

: THROW-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND 1 ACQUIRE 1 ALLOC-ID -9363 throw ;

: THROW-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: THROW-INIT ;] C2-MEM:WITH-INIT ;

: THROW-CALL ( -- )
   16 MEM:BYTES-ALLOC-LEN [: THROW-BODY ;] C2-MEM:WITH-MUT ;

: FAIL-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND 0 MEM:BYTES-ALLOC-LEN [: DISPOSE ;] C2-MEM:ALLOC-DISPOSE
   C2-MEM:PUBLISH drop C2-MEM:UNBIND ;

: FAIL-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: FAIL-INIT ;] C2-MEM:WITH-INIT ;

: FAIL-CALL ( -- )
   16 MEM:BYTES-ALLOC-LEN [: FAIL-BODY ;] C2-MEM:WITH-MUT ;

: ACQ-FAIL-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND
   16 MEM:BYTES-ALLOC-LEN [: DISPOSE-EMPTY ;] C2-MEM:ALLOC-DISPOSE
   C2-MEM:PUBLISH drop
   -9364 throw ;

: ACQ-FAIL-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: ACQ-FAIL-INIT ;] C2-MEM:WITH-INIT ;

: ACQ-FAIL-CALL ( -- )
   16 MEM:BYTES-ALLOC-LEN [: ACQ-FAIL-BODY ;] C2-MEM:WITH-MUT ;

: INNER-INIT ( C2-MEM:owner<p,i,a> mut-view<q,j,b,init<j,C2-MEM:owner-state>> -- C2-MEM:owner<p,i,a> mut-view<q,j,b,init<j,C2-MEM:owner-state>> )
   C2-MEM:BIND
   8 ACQUIRE 8 ALLOC-ID C2-MEM:UNBIND
   swap 9 ACQUIRE 9 ALLOC-ID swap ;

: INNER-ROOT ( C2-MEM:owner<p,i,a> mut-view<q,q,b,u8> -- C2-MEM:owner<p,i,a> mut-view<q,q,b,u8> )
   C2-MEM:SEED-OWNER [: INNER-INIT ;] C2-MEM:WITH-INIT ;

: INNER-OWNER ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> )
   C2-MEM:OWNER-SIZE [: INNER-ROOT ;] C2-MEM:WITH-MUT ;

: OUTER-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND
   7 ACQUIRE 7 ALLOC-ID
   INNER-OWNER
   8 CALLS @ 1 T=
   7 CALLS @ 0 T=
   9 CALLS @ 0 T=
   C2-MEM:UNBIND ;

: OUTER-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: OUTER-INIT ;] C2-MEM:WITH-INIT ;

: TASK-A-INNER-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND 17 ACQUIRE 17 ALLOC-ID C2-MEM:UNBIND ;

: TASK-A-INNER-ROOT
   ( C2-MEM:owner<p,i,a> mut-view<q,q,b,u8> -- C2-MEM:owner<p,i,a> mut-view<q,q,b,u8> )
   C2-MEM:SEED-OWNER [: TASK-A-INNER-INIT ;] C2-MEM:WITH-INIT ;

: TASK-A-INNER ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> )
   C2-MEM:OWNER-SIZE [: TASK-A-INNER-ROOT ;] C2-MEM:WITH-MUT ;

: TASK-A-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND 16 ACQUIRE 16 ALLOC-ID TASK-A-INNER
   1 READY atomic-add drop
   begin GO atomic@ 0= while TASK:PAUSE repeat
   C2-MEM:UNBIND ;

: TASK-A-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: TASK-A-INIT ;] C2-MEM:WITH-INIT ;

: TASK-A-CALL ( -- )
   C2-MEM:OWNER-SIZE [: TASK-A-ROOT ;] C2-MEM:WITH-MUT
   123 TASK:RETURN ;

: TASK-B-INNER-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND 19 ACQUIRE 19 ALLOC-ID
   1 READY atomic-add drop
   begin TASK:PAUSE again ;

: TASK-B-INNER-ROOT
   ( C2-MEM:owner<p,i,a> mut-view<q,q,b,u8> -- C2-MEM:owner<p,i,a> mut-view<q,q,b,u8> )
   C2-MEM:SEED-OWNER [: TASK-B-INNER-INIT ;] C2-MEM:WITH-INIT ;

: TASK-B-INNER ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> )
   C2-MEM:OWNER-SIZE [: TASK-B-INNER-ROOT ;] C2-MEM:WITH-MUT ;

: TASK-B-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND 18 ACQUIRE 18 ALLOC-ID TASK-B-INNER C2-MEM:UNBIND ;

: TASK-B-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: TASK-B-INIT ;] C2-MEM:WITH-INIT ;

: TASK-B-CALL ( -- )
   C2-MEM:OWNER-SIZE [: TASK-B-ROOT ;] C2-MEM:WITH-MUT ;

: ERROR-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND
   5 ACQUIRE 5 ALLOC-ID
   6 ACQUIRE 6 ALLOC-FAIL-ID
   -9363 throw ;

: ERROR-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: ERROR-INIT ;] C2-MEM:WITH-INIT ;

: ERROR-CALL ( -- )
   16 MEM:BYTES-ALLOC-LEN [: ERROR-BODY ;] C2-MEM:WITH-MUT ;

: HALT-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND 3 ACQUIRE 3 ALLOC-ID
   1 READY atomic!
   begin TASK:PAUSE again ;

: HALT-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: HALT-INIT ;] C2-MEM:WITH-INIT ;

: HALT-CALL ( -- )
   16 MEM:BYTES-ALLOC-LEN [: HALT-BODY ;] C2-MEM:WITH-MUT ;

: PAUSE-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND 4 ACQUIRE 4 ALLOC-PAUSE-ID C2-MEM:UNBIND ;

: PAUSE-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: PAUSE-INIT ;] C2-MEM:WITH-INIT ;

: PAUSE-CALL ( -- )
   16 MEM:BYTES-ALLOC-LEN [: PAUSE-BODY ;] C2-MEM:WITH-MUT ;

: DEFER-THROW-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND 2 ACQUIRE 2 ALLOC-PAUSE-FAIL-ID -9363 throw ;

: DEFER-THROW-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: DEFER-THROW-INIT ;] C2-MEM:WITH-INIT ;

: DEFER-THROW-CALL ( -- )
   16 MEM:BYTES-ALLOC-LEN [: DEFER-THROW-BODY ;] C2-MEM:WITH-MUT ;

: DEFER-THROW-CATCH ( -- )
   ['] DEFER-THROW-CALL catch
   dup -9362 <> if throw then drop
   1 RESUMED atomic!
   begin TASK:PAUSE again ;

: CHUNK-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND
   10 ACQUIRE 10 65488 ALLOC-CHUNK-ID
   11 ACQUIRE 11 7 ALLOC-CHUNK-ID
   12 ACQUIRE 12 65513 ALLOC-CHUNK-FAIL-ID
   13 ACQUIRE 13 17 ALLOC-CHUNK-ID
   -9363 throw ;

: CHUNK-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: CHUNK-INIT ;] C2-MEM:WITH-INIT ;

: CHUNK-CALL ( -- )
   C2-MEM:OWNER-SIZE [: CHUNK-BODY ;] C2-MEM:WITH-MUT ;

: CHUNK-PAUSE-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND
   14 ACQUIRE 14 65488 ALLOC-CHUNK-ID
   15 ACQUIRE 15 17 ALLOC-CHUNK-PAUSE-ID
   C2-MEM:UNBIND ;

: CHUNK-PAUSE-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: CHUNK-PAUSE-INIT ;] C2-MEM:WITH-INIT ;

: CHUNK-PAUSE-CALL ( -- )
   C2-MEM:OWNER-SIZE [: CHUNK-PAUSE-BODY ;] C2-MEM:WITH-MUT ;

: LIMIT-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND
   16 MEM:BYTES-ALLOC-LEN [: DISPOSE-EMPTY ;] C2-MEM:ALLOC-DISPOSE
   C2-MEM:PUBLISH drop
   MEM-MAX-N MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   C2-MEM:PUBLISH drop C2-MEM:UNBIND ;

: LIMIT-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: LIMIT-INIT ;] C2-MEM:WITH-INIT ;

: LIMIT-CALL ( -- )
   C2-MEM:OWNER-SIZE [: LIMIT-BODY ;] C2-MEM:WITH-MUT ;

: MAP-FAIL-INIT ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND
   16 MEM:BYTES-ALLOC-LEN [: DISPOSE-EMPTY ;] C2-MEM:ALLOC-DISPOSE
   C2-MEM:PUBLISH drop
   1 48 lshift MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   C2-MEM:PUBLISH drop C2-MEM:UNBIND ;

: MAP-FAIL-BODY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: MAP-FAIL-INIT ;] C2-MEM:WITH-INIT ;

: MAP-FAIL-CALL ( -- )
   C2-MEM:OWNER-SIZE [: MAP-FAIL-BODY ;] C2-MEM:WITH-MUT ;


: HALTED? ( result<n,n> -- bool )
   MATCH result
      ok OF drop false ENDOF
      err OF E-TASK-NO-RESULT = ENDOF
   ;MATCH ;

: ANSWER? ( result<n,n> -- bool )
   MATCH result
      ok OF 123 = ENDOF
      err OF drop false ENDOF
   ;MATCH ;

: CLEANUP-ERROR? ( result<n,n> -- bool )
   MATCH result
      ok OF drop false ENDOF
      err OF -9362 = ENDOF
   ;MATCH ;

\ Only the scalar countdown is deferred; scoped callbacks stay literal.
defer CAP-NEST ( n -- n )

: CAP-INIT
   ( n mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- n mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   1 CAP-CALLS +!
   C2-MEM:BIND swap CAP-NEST swap C2-MEM:UNBIND ;

: CAP-ROOT ( n mut-view<p,p,a,u8> -- n mut-view<p,p,a,u8> )
   1 CAP-CALLS +!
   C2-MEM:SEED-OWNER [: CAP-INIT ;] C2-MEM:WITH-INIT ;

: CAP-STEP ( n -- n )
   dup 0= if exit then
   1- C2-MEM:OWNER-SIZE [: CAP-ROOT ;] C2-MEM:WITH-MUT ;

: CAP-INSTALL ( -- ) [: CAP-STEP ;] is CAP-NEST ;

: CAP-OVER ( -- ) 17 CAP-NEST drop ;

public
: RUN ( -- )
   T-RESET
   16 MEM:BYTES-ALLOC-LEN [: BODY ;] C2-MEM:WITH-MUT
   s" a registered foreign resource is released once after a zeroed allocation" T-LABEL
   0 CALLS @ 1 T=
   RELEASES @ 1 T=
   s" a body throw closes its registered acquisition" T-LABEL
   ['] THROW-CALL catch -9363 T=
   1 CALLS @ 1 T=
   s" failed allocation invokes no disposer" T-LABEL
   ['] FAIL-CALL catch E-MEM-SIZE T=
   RELEASES @ 2 T=
   s" failed acquisition closes the registered empty slot once" T-LABEL
   ['] ACQ-FAIL-CALL catch -9364 T=
   EMPTY-CALLS @ 1 T=
   s" an outer allocation made inside an inner owner stays with the outer owner" T-LABEL
   16 MEM:BYTES-ALLOC-LEN [: OUTER-BODY ;] C2-MEM:WITH-MUT
   7 CALLS @ 1 T=
   8 CALLS @ 1 T=
   9 CALLS @ 1 T=
   s" a throwing disposer releases first and does not prevent older disposal" T-LABEL
   ['] ERROR-CALL catch -9362 T=
   5 CALLS @ 1 T=
   6 CALLS @ 1 T=
   3 ORDER @ 9 T=
   4 ORDER @ 7 T=
   0 READY atomic!
   ['] HALT-CALL WORKER TASK:ACTIVATE
   begin READY atomic@ 0= while TASK:PAUSE repeat
   WORKER TASK:HALT
   s" task halt drains a live owner's foreign acquisition" T-LABEL
   WORKER TASK:JOIN HALTED? TTRUE
   3 CALLS @ 1 T=
   0 READY atomic! 0 GO atomic!
   ['] PAUSE-CALL WORKER TASK:ACTIVATE
   begin READY atomic@ 0= while TASK:PAUSE repeat
   WORKER TASK:HALT
   1 GO atomic!
   s" task halt waits for a yielding disposer to release its resource" T-LABEL
   WORKER TASK:JOIN HALTED? TTRUE
   4 CALLS @ 1 T=
   s" chunk callbacks continue through a throwing disposer and release in reverse order" T-LABEL
   ['] CHUNK-CALL catch -9362 T=
   10 CALLS @ 1 T= 11 CALLS @ 1 T= 12 CALLS @ 1 T= 13 CALLS @ 1 T=
   9 ORDER @ 13 T= 10 ORDER @ 12 T= 11 ORDER @ 11 T= 12 ORDER @ 10 T=
   0 READY atomic! 0 GO atomic!
   ['] CHUNK-PAUSE-CALL WORKER TASK:ACTIVATE
   begin READY atomic@ 0= while TASK:PAUSE repeat
   WORKER TASK:HALT
   1 GO atomic!
   s" halted chunk cleanup waits for a yielding disposer before older release" T-LABEL
   WORKER TASK:JOIN HALTED? TTRUE
   14 CALLS @ 1 T= 15 CALLS @ 1 T=
   13 ORDER @ 15 T= 14 ORDER @ 14 T=
   s" oversized append is refused before mapping and closes prior disposal" T-LABEL
   ['] LIMIT-CALL catch E-MEM-SIZE T=
   EMPTY-CALLS @ 2 T=
   s" failed huge mapping closes a prior owner allocation" T-LABEL
   ['] MAP-FAIL-CALL catch E-MEM-MAP T=
   EMPTY-CALLS @ 3 T=
   CAP-INSTALL
   s" all 32 public frames are available before and after exhaustion" T-LABEL
   0 CAP-CALLS ! 16 CAP-NEST 0 T= CAP-CALLS @ 32 T=
   0 CAP-CALLS ! ['] CAP-OVER catch E-C2-CAPACITY T= CAP-CALLS @ 32 T=
   0 CAP-CALLS ! 16 CAP-NEST 0 T= CAP-CALLS @ 32 T=
   0 READY atomic! 0 GO atomic!
   ['] TASK-A-CALL WORKER TASK:ACTIVATE
   ['] TASK-B-CALL WORKER-B TASK:ACTIVATE
   begin READY atomic@ 2 < while TASK:PAUSE repeat
   s" two tasks retain separate nested owner chains" T-LABEL
   17 CALLS @ 1 T= 16 CALLS @ 0 T=
   19 CALLS @ 0 T= 18 CALLS @ 0 T=
   WORKER-B TASK:HALT
   WORKER-B TASK:JOIN HALTED? TTRUE
   19 CALLS @ 1 T= 18 CALLS @ 1 T=
   RELEASES @ 2 - ORDER @ 19 T=
   RELEASES @ 1- ORDER @ 18 T=
   16 CALLS @ 0 T=
   1 GO atomic!
   WORKER TASK:JOIN ANSWER? TTRUE
   16 CALLS @ 1 T=
   RELEASES @ 1- ORDER @ 16 T=
   0 READY atomic! 0 GO atomic! 0 RESUMED atomic!
   [: -9364 throw ;] WORKER-C TASK:AT-EXIT
   ['] DEFER-THROW-CATCH WORKER-C TASK:ACTIVATE
   begin READY atomic@ 0= while TASK:PAUSE repeat
   WORKER-C TASK:HALT
   1 GO atomic!
   s" deferred halt preserves cleanup over body and exit errors without resuming catch" T-LABEL
   WORKER-C TASK:JOIN CLEANUP-ERROR? TTRUE
   RESUMED atomic@ 0 T=
   2 CALLS @ 1 T=
   T-REPORT
   s" c2-owner-dispose release-order:" type
   RELEASES @ 0 ?do STR-SPACE emit i ORDER @ FMT:.INT loop cr
   s" c2-owner-dispose: ok" type cr ;
;package

C2-OWNER-DISPOSE:RUN
