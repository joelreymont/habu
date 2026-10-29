\ debugger-resume.f - one-shot restoration across code protection pages.
require test/gate-common.f

package DEBUGGER-RESUME

1792 constant PAD-WORDS              \ enough padding to cross a protection unit

: SOURCE ( -- )
   GE-SRC-RESET
   s" 0 set-tier" GE-SRC-LINE
   s" require src/habu/debug.f" GE-SRC-LINE   \ a no-op on the product, which bakes DEBUG
   s" package BPR" GE-SRC-LINE
   s" variable WV 17 WV !" GE-SRC-LINE
   s" : TARGET ( n -- n ) WV @ + ;" GE-SRC-LINE
   s" ' TARGET constant TARGET-XT" GE-SRC-LINE
   \ Move the live compile cursor past a 64 KiB protection unit, whatever the
   \ initial alignment, so the one-shot restore must reopen a span the compiler
   \ is not holding. PAD-WORDS is tuned, not assumed: CHECK-PAGES proves the
   \ separation in the child and throws 99 when it is too small. Each body
   \ carries calls: the source must fit GE-SRC-CAP, and tier 0 keeps constants
   \ on a virtual stack, so `1 drop` and `dup drop` bodies compile to the two
   \ instructions an empty one does (measured).
   PAD-WORDS 0 ?do
      s" : PAD" GE-SRC+ i GE-SRC-U+
      s"  ( n -- n ) dup + dup + dup + dup + dup + dup + dup + dup + ;"
      GE-SRC-LINE
   loop
   \ The address comparison only establishes the test's page separation.
   s" TRUSTED: PAGES? ( -- bool ) cp@ $FFFF invert and TARGET-XT $FFFF invert and <> ;" GE-SRC-LINE
   s" : CHECK-PAGES ( -- ) PAGES? 0= if 99 throw then ; CHECK-PAGES" GE-SRC-LINE
   s" WV DEBUG:BPW+ ' TARGET DEBUG:BP+" GE-SRC-LINE
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
