\ Both compiler tiers use a checker loaded from the current source tree.
require lib/test.f
require lib/string.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package LOOP-WINDOW-TEST

$4000 constant IO-CAP
180000 constant DEADLINE-MS
create OUT IO-CAP allot
create ERR IO-CAP allot

: ARGS! ( bool -- ) {: native:bool :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/native-window-owner-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/native-window-loop-obligations.f" >LEN PROC-ARGV+
   native if s" test/compiler/aot-mode.f" >LEN PROC-ARGV+ then
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

: RUN-TIER ( bool -- )
   ARGS!
   ENGINE-CANDIDATE:PATH$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   OUT outu LEN>N S\" loop-runtime: ok\nwindow: 0\n" STR= 0= rc 0 <> or if
      ERR erru LEN>N type cr
   then
   rc 0 T=
   OUT outu LEN>N S\" loop-runtime: ok\nwindow: 0\n" T$=
   \ The child asserts each exact catch code; the load path must also diagnose.
   erru LEN>N 0 > TTRUE ;

T-RESET
s" loop obligations through the JIT and fresh checker" T-LABEL
0 1 = RUN-TIER
s" loop obligations through the native compiler and fresh checker" T-LABEL
0 0= RUN-TIER
T-REPORT
;package
