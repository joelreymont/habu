\ Exercise current checker source after the same ownership transfer as a build.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f

package DEFER-HISTORY-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

\ The child runs test/native-window-owner-child.f, which reopens the engine's
\ build window: `hb: internal engine word: DECLARATIONS`, exit 70 on the sealed
\ product. So it runs on the engine test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" defer-history" WHITEBOX-CHILD:PROVIDE ;

: ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/native-window-owner-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/defer-history-child.f" >LEN PROC-ARGV+
   s" src/core/bytes.f" >LEN PROC-ARGV+
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" >LEN PROC-ARGV+
      s" src/os/linux/layout.f" >LEN PROC-ARGV+
   else
      s" src/os/macos/target.f" >LEN PROC-ARGV+
      s" src/os/macos/layout.f" >LEN PROC-ARGV+
   then
   s" src/habu/stack-abi.f" >LEN PROC-ARGV+
   s" src/habu/layout.f" >LEN PROC-ARGV+
   s" src/os/env-base.f" >LEN PROC-ARGV+
   s" src/habu/code-span.f" >LEN PROC-ARGV+
   s" src/habu/xref.f" >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV! ;

: CHECK ( -- )
   ARGS
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 180000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0 <> if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N S\" defer history: ok\nwindow: 0\n" T$=
   \ The child checks exact candidate refusals; the checker also diagnoses them.
   erru LEN>N 0 > TTRUE ;

: RUN ( -- )
   T-RESET
   [: PREPARE CHECK ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
