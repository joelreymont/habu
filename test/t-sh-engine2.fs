\ t-sh-engine2.fs — engine-builder port, part 2: the standalone's EMIT-FORTH builds
\ the COMPLETE engine image (main loop + JIT keywords + prims + helpers + dict +
\ baked source) word-for-word identical to caf's src/cg/forth.fs EMIT-FORTH for the
\ same source. Run: gforth test/t-sh-engine2.fs -e bye
require ../src/cg/forth.fs
require sh-driver.fs
create RBUF 131072 allot
create EB 786432 allot  variable EL
: e+ ( a u -- )  bounds ?do i c@ EB EL @ + c! 1 EL +! loop ;
: n+ ( n -- )  0 <# 10 hold #s #> e+ ;
: w@ ( i -- u32 )  4 * RBUF +  dup c@  over 1+ c@ 8 lshift or
   over 2 + c@ 16 lshift or  swap 3 + c@ 24 lshift or ;
: REF ( -- )  0 EL !
   s" 6 7 * ." EMIT-FORTH
   RBUF ASSEMBLE 4 /  0 ?do i w@ n+ loop ;
: GEN ( -- a u )
   0 CL !
   s" selfhost/asm.fs" +F  s" selfhost/icode.fs" +F  s" selfhost/mnem.fs" +F
   s" selfhost/util.fs" +F  s" selfhost/walk.fs" +F  s" selfhost/rt.fs" +F
   s" selfhost/crash.fs" +F  s" selfhost/engine.fs" +F  s" selfhost/prof.fs" +F  s" selfhost/engine2.fs" +F
   s" : SRC$ s" +B  s\" \" 6 7 * .\" ;" +B  s"  " +B
   s" : GO SRC$ EMIT-FORTH " +B
   s" 0 BEGIN dup ASM-LEN 4 / < WHILE dup CW@ RD32 . 1 + REPEAT drop ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ GEN  EB EL @ compare 0= -> true }T
