\ t-sh-crash.fs — the crash-handler port (selfhost/crash.fs) emits the SAME words
\ as caf's src/cg/crash.fs: hex printer + handler + sigaction installer, golden
\ word-for-word. Run: gforth test/t-sh-crash.fs -e bye
require ../src/cg/templ.fs
require ../src/cg/crash.fs
require sh-driver.fs
create RBUF 8192 allot
create EB 65536 allot  variable EL
: e+ ( a u -- )  bounds ?do i c@ EB EL @ + c! 1 EL +! loop ;
: n+ ( n -- )  0 <# 10 hold #s #> e+ ;
: w@ ( i -- u32 )  4 * RBUF +  dup c@  over 1+ c@ 8 lshift or
   over 2 + c@ 16 lshift or  swap 3 + c@ 24 lshift or ;
: REF ( -- )  0 EL !  ICODE-RESET
   NEWLBL Lcrashh !  NEWLBL Lhex !  NEWLBL Lhdr !
   g-install-crash  emit-hex  emit-crash-handler
   RBUF ASSEMBLE 4 /  0 ?do i w@ n+ loop ;
: GEN ( -- a u )
   0 CL !
   s" selfhost/asm.fs" +F  s" selfhost/icode.fs" +F  s" selfhost/mnem.fs" +F
   s" selfhost/util.fs" +F  s" selfhost/walk.fs" +F  s" selfhost/crash.fs" +F
   s" : GO ASM-INIT NEWLBL Lcrashh ! NEWLBL Lhex ! NEWLBL Lhdr ! " +B
   s" g-install-crash emit-hex emit-crash-handler " +B
   s" 0 BEGIN dup CP @ < WHILE dup CW@ RD32 . 1 + REPEAT drop ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ GEN  EB EL @ compare 0= -> true }T
