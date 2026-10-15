\ engine-writers.f - the engine's definition and replay writers, judged in the
\ native build window.
\
\ Run: bin/hb --load test/engine-writers.f
\
\ test/engine-writers-child.f calls each writer through a checked word of the
\ package whose row types it, OUTER or CHECKER-OVERLAY.
\ test/engine-writers-prepare.f defines those words in packages of those names
\ in test/native-window-owner-child.f's window (CHECKER-OVERLAY reopened, OUTER
\ fresh), then runs the production seal, and the child's cases run after it.
require src/lib/test.f
require src/lib/fs-mutate.f
require test/whitebox-child.f
require test/window-child-args.f

package ENGINE-WRITERS-SUITE

\ The child runs test/native-window-owner-child.f, which reopens the engine's
\ build window: `hb: internal engine word: DECLARATIONS`, exit 70 on the sealed
\ product. So it runs on the engine test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" engine-writers" WHITEBOX-CHILD:PROVIDE ;

: ARGS ( -- )
   s" test/engine-writers-child.f" WINDOW-CHILD-ARGS:CHILD
   WINDOW-CHILD-ARGS:CORE WINDOW-CHILD-ARGS:TARGET WINDOW-CHILD-ARGS:HABU
   s" test/engine-writers-prepare.f" WINDOW-CHILD-ARGS:PREPARE ;

\ The child's fd 1 is its T-REPORT line, then the window's verdict: 0 when the
\ window accepted the fixture. The child loads the window and forks a process
\ per case, so it takes the long row's child deadline (test/suite-budget.f): it
\ catches a hang, not a slow child.
: RESULT ( -- ) S\" test: ok\nwindow: 0\n" WINDOW-CHILD-ARGS:EXPECT ;

: RUN ( -- )
   T-RESET
   [: PREPARE ARGS RESULT ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
