\ t-sh-habu2.fs — engine-builder port, part 2: the standalone's EMIT-FORTH builds
\ the COMPLETE engine image (main loop + JIT keywords + prims + helpers + dict +
\ baked source) word-for-word identical to habu's bootstrap/cg/forth.fs EMIT-FORTH for the
\ same source. Run: gforth test/t-sh-habu2.fs -e bye
require ../bootstrap/cg/forth.fs
require sh-driver.fs
create RBUF 131072 allot
create EB 786432 allot  variable EL
: E+ ( a u -- )  bounds ?do i c@ EB EL @ + c! 1 EL +! loop ;
: N+ ( n -- )  0 <# 10 hold #s #> E+ ;
: w@ ( i -- u32 )  4 * RBUF +  dup c@  over 1+ c@ 8 lshift or
   over 2 + c@ 16 lshift or  swap 3 + c@ 24 lshift or ;
: REF ( -- )  0 EL !
   s" 6 7 * ." EMIT-FORTH
   RBUF ASSEMBLE 4 /  0 ?do i w@ N+ loop ;
: GEN ( -- a u )
   0 CL !
   s" src/arch/arm64/asm.f" +F  s" src/arch/arm64/icode.f" +F  s" src/arch/arm64/mnem.f" +F
   s" src/core/util.f" +F  s" src/os/macos/sys.f" +F  s" src/os/macos/env.f" +F  s" src/habu/rt.f" +F
   s" src/habu/crash.f" +F  s" src/habu/habu1.f" +F  s" src/habu/prof.f" +F  s" src/habu/regalloc.f" +F  s" src/habu/jit.f" +F  s" src/habu/habu2.f" +F
   s" : SRC$ s" +B  s\" \" 6 7 * .\" ;" +B  s"  " +B
   s" : GO SRC$ EMIT-FORTH " +B
   s" 0 BEGIN dup ASM-LEN 4 / < WHILE dup CW@ RD32 . 1 + REPEAT drop ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ GEN  EB EL @ compare 0= -> true }T
