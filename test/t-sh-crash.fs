\ t-sh-crash.fs — the crash-handler port (src/habu/crash.f) emits the SAME words
\ as habu's bootstrap/cg/crash.fs: hex printer + handler + sigaction installer, golden
\ word-for-word. Run: gforth test/t-sh-crash.fs -e bye
require ../bootstrap/cg/templ.fs
require ../bootstrap/cg/crash.fs
require sh-driver.fs
create RBUF 8192 allot
create EB 65536 allot  variable EL
: E+ ( a u -- )  bounds ?do i c@ EB EL @ + c! 1 EL +! loop ;
: N+ ( n -- )  0 <# 10 hold #s #> E+ ;
: w@ ( i -- u32 )  4 * RBUF +  dup c@  over 1+ c@ 8 lshift or
   over 2 + c@ 16 lshift or  swap 3 + c@ 24 lshift or ;
: REF ( -- )  0 EL !  ICODE-RESET
   LBL LCRASHH !  LBL LHEX !  LBL LHDR !
   G-INSTALL-CRASH  EMIT-HEX  EMIT-CRASH-HANDLER
   RBUF ASSEMBLE 4 /  0 ?do i w@ N+ loop ;
: GEN ( -- a u )
   0 CL !
   s" src/arch/arm64/asm.f" +F  s" src/arch/arm64/icode.f" +F  s" src/arch/arm64/mnem.f" +F  s" src/os/macos/sys.f" +F  s" src/os/macos/env.f" +F
   s" src/core/util.f" +F  s" src/habu/crash.f" +F
   s" : GO ASM-INIT LBL Lcrashh ! LBL Lhex ! LBL Lhdr ! " +B
   s" g-install-crash emit-hex emit-crash-handler " +B
   s" 0 BEGIN dup CP @ < WHILE dup CW@ RD32 . 1 + REPEAT drop ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ GEN  EB EL @ compare 0= -> true }T
