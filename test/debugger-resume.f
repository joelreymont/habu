\ debugger-resume.f - one-shot restoration across code protection pages.
require test/gate-common.f

package DEBUGGER-RESUME

1792 constant PAD-WORDS              \ enough padding to cross a protection unit

: SOURCE ( -- )
   GE-SRC-RESET
   s" 0 set-tier" GE-SRC-LINE
   s" package BPR" GE-SRC-LINE
   s" variable WV 17 WV !" GE-SRC-LINE
   s" : TARGET ( n -- n ) WV @ + ;" GE-SRC-LINE
   s" ' TARGET constant TARGET-XT" GE-SRC-LINE
   \ Move the live compile cursor beyond a 64 KiB protection unit, independent
   \ of the initial alignment, so the one-shot restore below has to reopen a
   \ span the compiler is not already holding. That is the whole point of the
   \ padding, and PAD-WORDS is a TUNING quantity rather than an assumption:
   \ CHECK-PAGES proves the separation in the child and throws 99 when it is no
   \ longer enough. It did exactly that when a JIT body stopped costing what it
   \ used to -- an empty one is two instructions now (dot
   \ habu-use-pre-and-1830972f), where it was five, so 4096 empty bodies spanned
   \ half a unit instead of a unit and a quarter.
   \ Each body carries calls rather than being empty: the span has to come out
   \ of a source that fits GE-SRC-CAP, and an empty body advances the cursor by
   \ too few bytes per byte of source to get there. They have to be CALLS --
   \ tier 0 keeps constants on a virtual stack and materializes them only when
   \ something forces it, so `1 drop` and `dup drop` bodies compile to the same
   \ two instructions an empty one does (measured, not assumed).
   PAD-WORDS 0 ?do
      s" : PAD" GE-SRC+ i GE-SRC-U+
      s"  ( n -- n ) dup + dup + dup + dup + dup + dup + dup + dup + ;"
      GE-SRC-LINE
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
