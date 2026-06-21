\ large-buffer-bundle-test.f - composed 64K buffer regression.
\
\ Load after lib/test.f, tools/lint/source-lex.f,
\ bench/llm/forth-task-lines-lib.f, and bench/llm/attempt-solutions-lib.f.

$10000 constant LBB-64K
2048 constant LBB-BUFS
90 constant LBB-MARK
91 constant LBB-END-MARK

variable LBB-FIRST
variable LBB-END

: LBB-TOTAL ( -- n )
   LBB-64K LBB-BUFS * ;

: LBB-ALLOC ( -- )
   here LBB-FIRST !
   LBB-BUFS 0 ?do LBB-64K allot loop
   here LBB-END ! ;

: LBB-SPAN ( -- n )
   LBB-END @ LBB-FIRST @ - ;

: LBB-LAST ( -- ptr u8 )
   LBB-END @ 1 - ;

: LBB-SMOKE ( -- n )
   LBB-MARK LBB-LAST c!
   LBB-LAST c@ ;

: LBB-USED ( -- n )
   here data-base - ;

: LBB-REMAINING ( -- n )
   DATA-SIZE LBB-USED - ;

: LBB-REMAINING-64K ( -- n )
   LBB-REMAINING LBB-64K / ;

: LBB-ALLOC-TO-FINAL-64K ( -- )
   LBB-REMAINING LBB-64K - allot ;

: LBB-END-BYTE ( -- ptr u8 )
   here 1 - ;

: LBB-END-SMOKE ( -- n )
   LBB-END-MARK LBB-END-BYTE c!
   LBB-END-BYTE c@ ;

: LBB-FINAL-CAPACITY ( -- )
   LBB-REMAINING-64K LBB-BUFS > TTRUE
   LBB-REMAINING LBB-64K > if
      LBB-ALLOC-TO-FINAL-64K
      LBB-REMAINING LBB-64K T=
      LBB-64K allot
      LBB-REMAINING 0 T=
      LBB-END-SMOKE LBB-END-MARK T=
   then ;

T-RESET
LBB-ALLOC
LBB-TOTAL $8000000 T=
LBB-SPAN LBB-TOTAL T=
LBB-SMOKE LBB-MARK T=
LBB-FINAL-CAPACITY
T-REPORT
