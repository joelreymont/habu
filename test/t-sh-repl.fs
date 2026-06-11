\ t-sh-repl.fs — the ported builder's STDIN mode (emit-source's read-from-fd-0
\ branch): (1) golden — standalone EMIT-FORTH with STDIN? on matches habu's word for
\ word; (2) behavioral — the standalone BUILDS a signed stdin engine with the ported
\ toolchain, and piping a program into it works. Run: gforth test/t-sh-repl.fs -e bye
require ../bootstrap/cg/forth.fs
require ../bootstrap/cg/exec.fs
require sh-driver.fs
create RBUF 131072 allot
create EB 786432 allot  variable EL
: e+ ( a u -- )  bounds ?do i c@ EB EL @ + c! 1 EL +! loop ;
: n+ ( n -- )  0 <# 10 hold #s #> e+ ;
: w@ ( i -- u32 )  4 * RBUF +  dup c@  over 1+ c@ 8 lshift or
   over 2 + c@ 16 lshift or  swap 3 + c@ 24 lshift or ;
: REF ( -- )  0 EL !
   STDIN? on  s" " EMIT-FORTH  STDIN? off
   RBUF ASSEMBLE 4 /  0 ?do i w@ n+ loop ;
: GEN ( -- a u )
   0 CL !
   s" src/core/sha256.f" +F  s" src/arch/arm64/asm.f" +F  s" src/arch/arm64/icode.f" +F
   s" src/arch/arm64/mnem.f" +F  s" src/core/util.f" +F
   s" src/habu/rt.f" +F  s" src/habu/crash.f" +F  s" src/os/macos/macho.f" +F
   s" src/os/macos/sign2.f" +F  s" src/habu/habu1.f" +F  s" src/habu/prof.f" +F  s" src/habu/vsjit.f" +F  s" src/habu/habu2.f" +F
   s" -1 STDIN? ! : GO here 0 EMIT-FORTH " +B
   s" 0 BEGIN dup ASM-LEN 4 / < WHILE dup CW@ RD32 . 1 + REPEAT drop ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ GEN  EB EL @ compare 0= -> true }T
\ behavioral: the standalone builds a signed stdin engine; pipe a program through it
: GEN2 ( -- )
   0 CL !
   s" src/core/sha256.f" +F  s" src/arch/arm64/asm.f" +F  s" src/arch/arm64/icode.f" +F
   s" src/arch/arm64/mnem.f" +F  s" src/core/util.f" +F
   s" src/habu/rt.f" +F  s" src/habu/crash.f" +F  s" src/os/macos/macho.f" +F
   s" src/os/macos/sign2.f" +F  s" src/habu/habu1.f" +F  s" src/habu/prof.f" +F  s" src/habu/vsjit.f" +F  s" src/habu/habu2.f" +F
   s" create PZ2 32 allot " +B
   s" : PZ! s" +B  s\" \" /tmp/sh-repl-bin\"" +B
   s"  {: a u :} 0 BEGIN dup u < WHILE dup a + c@ over PZ2 + c! 1 + REPEAT drop 0 PZ2 u + c! ; " +B
   s" : GO -1 STDIN? ! here 0 EMIT-FORTH BUILD-MACHO " +B
   s\" s\" repl\" SET-SIGID CODESIG2 " +B
   s" PZ! PZ2 1537 493 open dup MBUF MLEN @ write drop close ; GO" +B
   CBUF CL @ NF-RUN ;
GEN2
: REPL-OUT ( -- a u )
   s\" echo ': SQ dup * ; 5 SQ . : L 3 0 do i . loop ; L' | /tmp/sh-repl-bin > /tmp/sh-repl-out 2>/dev/null" system
   s" /tmp/sh-repl-out" slurp-file ;
T{ REPL-OUT  s\" 25\n0\n1\n2\n" compare 0= -> true }T
