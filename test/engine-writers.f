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
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f
require test/suite-budget.f              \ CHILD-MS, every child's hang guard

package ENGINE-WRITERS-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

\ The child runs test/native-window-owner-child.f, which reopens the engine's
\ build window: `hb: internal engine word: DECLARATIONS`, exit 70 on the sealed
\ product. So it runs on the engine test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" engine-writers" WHITEBOX-CHILD:PROVIDE ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ The window stops at src/core/cell-effects.f. These are the prefix files the
\ include words and the seal stand on, in the build's order, before the
\ fixture's owner words and the seal itself (test/engine-writers-prepare.f).
: TARGET-ARGS ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" src/os/linux-x86-64/target.f" ARG
      s" src/os/linux/layout-constants.f" ARG
      s" src/os/linux/layout.f" ARG exit
   then
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" ARG
      s" src/os/linux/layout-constants.f" ARG
      s" src/os/linux/layout.f" ARG exit
   then
   s" src/os/macos/target.f" ARG s" src/os/macos/layout.f" ARG ;

: ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" ARG
   s" test/native-window-owner-child.f" ARG
   s" --" ARG
   s" test/engine-writers-child.f" ARG
   s" src/core/declaration-transaction.f" ARG
   s" src/core/generated-declaration.f" ARG
   s" src/core/decl-event.f" ARG
   s" src/core/structure-make.f" ARG
   s" src/core/structure-decl.f" ARG
   s" src/core/enum-decl.f" ARG
   s" src/core/structures.f" ARG
   s" src/core/bytes.f" ARG
   TARGET-ARGS
   s" src/habu/stack-abi.f" ARG
   s" src/habu/layout.f" ARG
   s" src/os/env-base.f" ARG
   s" src/core/include.f" ARG
   s" src/habu/code-span.f" ARG
   s" test/engine-writers-prepare.f" ARG
   WHITEBOX-CHILD:ENV! ;

\ The child's fd 1 is its T-REPORT line, then the window's verdict: 0 when the
\ window accepted the fixture. The child loads the window and forks a process
\ per case, so it takes the long row's child deadline (test/suite-budget.f): it
\ catches a hang, not a slow child.
: RESULT ( -- )
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN
   SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N S\" test: ok\nwindow: 0\n" STR= 0= rc 0 <> or
      if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N S\" test: ok\nwindow: 0\n" T$= ;

: RUN ( -- )
   T-RESET
   [: PREPARE ARGS RESULT ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
