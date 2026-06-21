\ large-buffer-bundle-test.f - composed 64K buffer regression.
\
\ Load after lib/test.f, tools/lint/source-lex.f,
\ bench/llm/forth-task-lines-lib.f, and bench/llm/attempt-solutions-lib.f.

$10000 constant LBB-64K
16 constant LBB-BUFS
90 constant LBB-MARK

create LBB-00 LBB-64K allot
create LBB-01 LBB-64K allot
create LBB-02 LBB-64K allot
create LBB-03 LBB-64K allot
create LBB-04 LBB-64K allot
create LBB-05 LBB-64K allot
create LBB-06 LBB-64K allot
create LBB-07 LBB-64K allot
create LBB-08 LBB-64K allot
create LBB-09 LBB-64K allot
create LBB-10 LBB-64K allot
create LBB-11 LBB-64K allot
create LBB-12 LBB-64K allot
create LBB-13 LBB-64K allot
create LBB-14 LBB-64K allot
create LBB-15 LBB-64K allot

: LBB-TOTAL ( -- n )
   LBB-64K LBB-BUFS * ;

: LBB-LAST ( -- ptr u8 )
   LBB-15 LBB-64K 1 - + ;

: LBB-SMOKE ( -- n )
   LBB-MARK LBB-LAST c!
   LBB-LAST c@ ;

T-RESET
LBB-TOTAL $100000 T=
LBB-SMOKE LBB-MARK T=
T-REPORT

s" large-buffer-bundle-test: ok" type cr
