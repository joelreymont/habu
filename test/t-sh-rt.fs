\ t-sh-rt.fs — the runtime-routine port (src/habu/rt.f): the standalone emits
\ EMIT-DOT + EMIT-ATOI and the words must match habu's bootstrap/cg/rt.fs output word for
\ word (same encoders, same labels, same layout). Run: gforth test/t-sh-rt.fs -e bye
require ../bootstrap/cg/templ.fs
require ../bootstrap/cg/rt.fs
require sh-driver.fs
create RBUF 8192 allot
create EB 65536 allot  variable EL
: e+ ( a u -- )  bounds ?do i c@ EB EL @ + c! 1 EL +! loop ;
: n+ ( n -- )  0 <# 10 hold #s #> e+ ;
: w@ ( i -- u32 )  4 * RBUF +  dup c@  over 1+ c@ 8 lshift or
   over 2 + c@ 16 lshift or  swap 3 + c@ 24 lshift or ;
: REF ( -- )  0 EL !  ICODE-RESET
   NEWLBL DOT-LBL !  EMIT-DOT  NEWLBL ATOI-LBL !  EMIT-ATOI
   RBUF ASSEMBLE 4 /  0 ?do i w@ n+ loop ;
: GEN ( -- a u )
   0 CL !
   s" src/arch/arm64/asm.f" +F  s" src/arch/arm64/icode.f" +F  s" src/arch/arm64/mnem.f" +F
   s" src/core/util.f" +F  s" src/arch/arm64/walk.f" +F  s" src/habu/rt.f" +F
   s" : GO ASM-INIT NEWLBL DOT-LBL ! EMIT-DOT NEWLBL ATOI-LBL ! EMIT-ATOI " +B
   s" 0 BEGIN dup CP @ < WHILE dup CW@ RD32 . 1 + REPEAT drop ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ GEN  EB EL @ compare 0= -> true }T
