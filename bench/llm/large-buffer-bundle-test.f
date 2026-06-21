\ large-buffer-bundle-test.f - composed OS-backed 64K buffer regression.
\
\ Load after lib/test.f, lib/memory.f, tools/lint/source-lex.f,
\ bench/llm/forth-task-lines-lib.f, and bench/llm/attempt-solutions-lib.f.

4096 constant LBB-BUFS
1024 constant LBB-EXTRA-BUFS
128 constant LBB-NESTED-BUFS
32 constant LBB-NESTED-SPANS
1100 constant LBB-ROWS
90 constant LBB-MARK
91 constant LBB-END-MARK

variable LBB-HERE
variable LBB-SRC-P
variable LBB-SRC-CAP-U
variable LBB-SRC-U

TRUSTED: LBB-SRC-BUF ( -- ptr u8 )
   LBB-SRC-P @ ;

: LBB-SRC-CAP ( -- n )
   LBB-SRC-CAP-U @ ;

: LBB-SRC$ ( -- ptr u8 n )
   LBB-SRC-BUF LBB-SRC-U @ ;

: LBB-STORE-SRC-SPAN ( ptr u8 n -- )
   LBB-SRC-CAP-U ! LBB-SRC-P ! ;

: LBB-TOTAL ( -- n )
   LBB-BUFS MEM-64K * ;

: LBB-EXTRA-TOTAL ( -- n )
   LBB-EXTRA-BUFS MEM-64K * ;

: LBB-NESTED-TOTAL ( -- n )
   LBB-NESTED-BUFS MEM-64K * ;

: LBB-HERE-SNAPSHOT ( -- )
   here data-base - LBB-HERE ! ;

: LBB-HERE-UNCHANGED ( -- )
   here data-base - LBB-HERE @ T= ;

: LBB-SRC-ROOM ( n -- ) {: add :}
   add 0 < if E-BM-SCHEMA throw then
   add LBB-SRC-CAP LBB-SRC-U @ - > if E-BM-SCHEMA throw then ;

: LBB-SRC+ ( ptr u8 n -- ) {: a:ptr u :}
   u LBB-SRC-ROOM
   a LBB-SRC-BUF LBB-SRC-U @ + u BYTE-COPY
   LBB-SRC-U @ u + LBB-SRC-U ! ;

: LBB-SRC-LN ( ptr u8 n -- )
   LBB-SRC+
   1 LBB-SRC-ROOM
   10 LBB-SRC-BUF LBB-SRC-U @ + c!
   LBB-SRC-U @ 1+ LBB-SRC-U ! ;

: LBB-SRC-RESET ( -- )
   0 LBB-SRC-U ! ;

: LBB-SRC-INIT ( -- )
   LBB-BUFS MEM-ALLOC-64K-BUFFERS LBB-STORE-SRC-SPAN
   LBB-SRC-RESET ;

: LBB-END ( ptr u8 n -- ptr u8 ) {: a:ptr u :}
   a u 1 - + ;

: LBB-SLOT ( ptr u8 n -- ptr u8 ) {: a:ptr idx :}
   a idx MEM-64K * + ;

: LBB-TOUCH-ENDS ( ptr u8 n -- ) {: a:ptr u :}
   LBB-MARK a c!
   LBB-END-MARK a u LBB-END c!
   a c@ LBB-MARK T=
   a u LBB-END c@ LBB-END-MARK T= ;

: LBB-TOUCH-64K-SLOTS ( ptr u8 n n -- ) {: a:ptr u count :}
   count MEM-64K * u T=
   count 0 ?do
      LBB-MARK a i LBB-SLOT c!
      a i LBB-SLOT c@ LBB-MARK T=
   loop ;

: LBB-TOUCH-SPAN ( ptr u8 n n -- ) {: a:ptr u count :}
   a u LBB-TOUCH-ENDS
   a u count LBB-TOUCH-64K-SLOTS ;

: LBB-NESTED-FRAME ( ptr u8 n n -- ) {: a:ptr u remaining :}
   a u LBB-NESTED-BUFS LBB-TOUCH-SPAN
   remaining 0 > if
      LBB-NESTED-BUFS MEM-ALLOC-64K-BUFFERS
      remaining 1 - recurse
   then
   a u LBB-NESTED-BUFS LBB-TOUCH-SPAN ;

: LBB-ALLOC-NESTED-SPANS ( -- )
   LBB-NESTED-BUFS MEM-ALLOC-64K-BUFFERS
   dup LBB-NESTED-TOTAL T=
   LBB-NESTED-SPANS 1 - LBB-NESTED-FRAME ;

: LBB-COMPOSED-WITH-FIRST ( ptr u8 n -- ) {: a:ptr u :}
   u LBB-TOTAL T=
   a u LBB-BUFS LBB-TOUCH-SPAN
   LBB-EXTRA-BUFS MEM-ALLOC-64K-BUFFERS
   dup LBB-EXTRA-TOTAL T=
   LBB-EXTRA-BUFS LBB-TOUCH-SPAN
   LBB-ALLOC-NESTED-SPANS
   a u LBB-BUFS LBB-TOUCH-SPAN
   LBB-HERE-UNCHANGED ;

: LBB-TASK-HEADER$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature" ;

: LBB-FORTH-ROW$ ( -- ptr u8 n )
   s" 1	SQUARE	(i64 -- i64)	arith	1 -> 1	forth	stack	Square.	-	v1	-	-" ;

: LBB-AS-TASKS$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
1	SQUARE	(i64 -- i64)	arith	1 -> 1	forth	stack	Square.	-	v1	-	-
" ;

: LBB-AS-SOLUTIONS$ ( -- ptr u8 n )
   s" : SQUARE ( i64 -- i64 ) dup * ;
" ;

: LBB-BUILD-LARGE-TASKS ( -- )
   LBB-SRC-RESET
   LBB-TASK-HEADER$ LBB-SRC-LN
   LBB-ROWS 0 ?do LBB-FORTH-ROW$ LBB-SRC-LN loop ;

: LBB-ASSERT-LARGE-FTL ( -- )
   LBB-BUILD-LARGE-TASKS
   LBB-SRC$ FTL-EMIT-DATA nip MEM-64K > TTRUE
   LBB-HERE-UNCHANGED ;

: LBB-ASSERT-AS-DATA ( -- )
   AS-RESET
   LBB-AS-TASKS$ AS-TASKS!
   LBB-AS-SOLUTIONS$ AS-SOLUTIONS!
   AS-BUILD-TASKS
   AS-SCAN-SOLUTIONS
   AS-VERIFY-SEEN
   LBB-HERE-UNCHANGED ;

T-RESET
LBB-SRC-INIT
LBB-HERE-SNAPSHOT
LBB-BUFS MEM-ALLOC-64K-BUFFERS LBB-COMPOSED-WITH-FIRST
LBB-ASSERT-LARGE-FTL
LBB-ASSERT-AS-DATA
T-REPORT
