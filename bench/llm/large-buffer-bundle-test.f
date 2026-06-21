\ large-buffer-bundle-test.f - composed 64K buffer regression.
\
\ Load after lib/test.f, tools/lint/source-lex.f,
\ bench/llm/forth-task-lines-lib.f, and bench/llm/attempt-solutions-lib.f.

$10000 constant LBB-64K
2048 constant LBB-BUFS
90 constant LBB-MARK

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

T-RESET
LBB-ALLOC
LBB-TOTAL $8000000 T=
LBB-SPAN LBB-TOTAL T=
LBB-SMOKE LBB-MARK T=
T-REPORT

s" large-buffer-bundle-test: ok" type cr
