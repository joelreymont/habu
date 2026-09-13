\ debugger-resume.f - one-shot restoration across code protection pages.
require test/gate-common.f

package DEBUGGER-RESUME

: SOURCE ( -- )
   GE-SRC-RESET
   s" 0 set-tier" GE-SRC-LINE
   s" package BPR" GE-SRC-LINE
   s" variable WV 17 WV !" GE-SRC-LINE
   s" : TARGET ( n -- n ) WV @ + ;" GE-SRC-LINE
   s" ' TARGET constant TARGET-XT" GE-SRC-LINE
   \ Empty JIT bodies still have an entry/return sequence. Move the live cursor
   \ beyond a 64 KiB protection unit, independent of the initial alignment.
   4096 0 ?do
      s" : PAD" GE-SRC+ i GE-SRC-U+ s"  ( -- ) ;" GE-SRC-LINE
   loop
   \ The address comparison only establishes the test's page separation.
   s" TRUSTED: PAGES? ( -- bool ) cp@ $FFFF invert and TARGET-XT $FFFF invert and <> ;" GE-SRC-LINE
   s" : CHECK-PAGES ( -- ) PAGES? 0= if 99 throw then ; CHECK-PAGES" GE-SRC-LINE
   s" WV BPW+ TARGET-XT BP+" GE-SRC-LINE
   s" 2 TARGET . 2 TARGET ." GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

public

: TEST ( -- )
   s" habu-debugger-resume" GT-START
   GE-HB-RESET SOURCE
   GE-HB$ GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   s" breakpoint resumes across protection pages" GE-EXPECT-OK
   S\" 19\n19\n" s" resumed word and subsequent call" GE-EXPECT-OUT
   s" habu-bp-watch:" s" watch table printed before resuming" GE-EXPECT-ERR-HAS
   s" 0000000000000011" s" watched value survives" GE-EXPECT-ERR-HAS
   GT-CLEANUP
   s" debugger-resume: ok" type cr ;

;package
DEBUGGER-RESUME:TEST
