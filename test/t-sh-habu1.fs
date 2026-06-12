\ t-sh-habu1.fs — engine-builder port, part 1: the standalone emits the engine's
\ PRIMS + helper routines (cemit/tok/prot/flush/find/num) + seed DICT and the words
\ must match habu's bootstrap/cg/forth.fs emitters exactly (same label-allocation order on
\ both sides). Run: gforth test/t-sh-habu1.fs -e bye
require ../bootstrap/cg/forth.fs
require sh-driver.fs
create RBUF 65536 allot
create EB 262144 allot  variable EL
: E+ ( a u -- )  bounds ?do i c@ EB EL @ + c! 1 EL +! loop ;
: N+ ( n -- )  0 <# 10 hold #s #> E+ ;
: w@ ( i -- u32 )  4 * RBUF +  dup c@  over 1+ c@ 8 lshift or
   over 2 + c@ 16 lshift or  swap 3 + c@ 24 lshift or ;
: REF ( -- )  0 EL !  ICODE-RESET  0 #PL !  0 PNP !
   NEWLBL LCEMIT !  NEWLBL LTOK !  NEWLBL LPROT !  NEWLBL LFLUSH !
   NEWLBL LFIND !  NEWLBL LNUM !  NEWLBL LNCOUNT !  NEWLBL LDICT !
   EMIT-PRIMS  EMIT-CEMIT  EMIT-TOK  EMIT-PROT  EMIT-FLUSH  EMIT-FIND  EMIT-NUM
   EMIT-DICT
   RBUF ASSEMBLE 4 /  0 ?do i w@ N+ loop ;
: GEN ( -- a u )
   0 CL !
   s" src/arch/arm64/asm.f" +F  s" src/arch/arm64/icode.f" +F  s" src/arch/arm64/mnem.f" +F
   s" src/core/util.f" +F  s" src/os/macos/sys.f" +F  s" src/os/macos/env.f" +F  s" src/habu/treeshake.f" +F  s" src/habu/rt.f" +F
   s" src/habu/habu1.f" +F
   s" : GO ASM-INIT " +B
   s" NEWLBL Lcemit ! NEWLBL Ltok ! NEWLBL Lprot ! NEWLBL Lflush ! " +B
   s" NEWLBL Lfind ! NEWLBL Lnum ! NEWLBL Lncount ! NEWLBL Ldict ! " +B
   s" emit-prims emit-cemit emit-tok emit-prot emit-flush emit-find emit-num " +B
   s" emit-dict " +B
   s" 0 BEGIN dup ASM-LEN 4 / < WHILE dup CW@ RD32 . 1 + REPEAT drop ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ GEN  EB EL @ compare 0= -> true }T
