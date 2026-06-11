\ t-sh-engine.fs — engine-builder port, part 1: the standalone emits the engine's
\ PRIMS + helper routines (cemit/tok/prot/flush/find/num) + seed DICT and the words
\ must match caf's src/cg/forth.fs emitters exactly (same label-allocation order on
\ both sides). Run: gforth test/t-sh-engine.fs -e bye
require ../src/cg/forth.fs
require sh-driver.fs
create RBUF 65536 allot
create EB 262144 allot  variable EL
: e+ ( a u -- )  bounds ?do i c@ EB EL @ + c! 1 EL +! loop ;
: n+ ( n -- )  0 <# 10 hold #s #> e+ ;
: w@ ( i -- u32 )  4 * RBUF +  dup c@  over 1+ c@ 8 lshift or
   over 2 + c@ 16 lshift or  swap 3 + c@ 24 lshift or ;
: REF ( -- )  0 EL !  ICODE-RESET  0 #PL !  0 PNP !
   NEWLBL Lcemit !  NEWLBL Ltok !  NEWLBL Lprot !  NEWLBL Lflush !
   NEWLBL Lfind !  NEWLBL Lnum !  NEWLBL Lncount !  NEWLBL Ldict !
   emit-prims  emit-cemit  emit-tok  emit-prot  emit-flush  emit-find  emit-num
   emit-dict
   RBUF ASSEMBLE 4 /  0 ?do i w@ n+ loop ;
: GEN ( -- a u )
   0 CL !
   s" selfhost/asm.fs" +F  s" selfhost/icode.fs" +F  s" selfhost/mnem.fs" +F
   s" selfhost/util.fs" +F  s" selfhost/walk.fs" +F  s" selfhost/rt.fs" +F
   s" selfhost/engine.fs" +F
   s" : GO ASM-INIT " +B
   s" NEWLBL Lcemit ! NEWLBL Ltok ! NEWLBL Lprot ! NEWLBL Lflush ! " +B
   s" NEWLBL Lfind ! NEWLBL Lnum ! NEWLBL Lncount ! NEWLBL Ldict ! " +B
   s" emit-prims emit-cemit emit-tok emit-prot emit-flush emit-find emit-num " +B
   s" emit-dict " +B
   s" 0 BEGIN dup ASM-LEN 4 / < WHILE dup CW@ RD32 . 1 + REPEAT drop ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ GEN  EB EL @ compare 0= -> true }T
