\ large-buffer-bundle-test.f - composed OS-backed 64K buffer regression.
\
\ Load after lib/test.f, lib/memory.f, tools/lint/source-lex.f,
\ bench/llm/forth-task-lines-lib.f, and bench/llm/attempt-solutions-lib.f.

4096 constant LBB-BUFS
1024 constant LBB-EXTRA-BUFS
128 constant LBB-NESTED-BUFS
32 constant LBB-NESTED-SPANS
90 constant LBB-MARK
91 constant LBB-END-MARK

variable LBB-HERE

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

T-RESET
LBB-HERE-SNAPSHOT
LBB-BUFS MEM-ALLOC-64K-BUFFERS LBB-COMPOSED-WITH-FIRST
T-REPORT
