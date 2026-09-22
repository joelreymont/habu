require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f

package PRIMITIVE-TRUST-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

\ The child runs test/native-window-owner-child.f, which reopens the engine's
\ build window: `hb: internal engine word: DECLARATIONS`, exit 70 on the sealed
\ product. So it runs on the engine test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" primitive-trust" WHITEBOX-CHILD:PROVIDE ;

: ARGS ( ptr u8 n bool -- ) {: fixture:ptr u:n native:bool :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/native-window-owner-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   fixture u >LEN PROC-ARGV+
   native if s" test/compiler/aot-mode.f" >LEN PROC-ARGV+ then
   WHITEBOX-CHILD:ENV! ;

: RESULT ( ptr u8 n -- ) {: want:ptr u:n :}
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 180000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0 <> if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N want u T$=
   ERR erru LEN>N s" trust-boundary primitive" CONTAINS? TTRUE
   ERR erru LEN>N s" CHECKER-RESET-SOURCE" CONTAINS? TTRUE ;

: CASES ( -- )
   s" test/primitive-trust-child.f" 0 0= 0= ARGS
   S\" primitive trust: ok\nwindow: 0\n" RESULT
   s" test/primitive-trust-reject.f" 0 0= 0= ARGS
   S\" primitive trust reject: armed\nwindow: 70\n" RESULT
   s" test/primitive-trust-reject.f" 0 0= ARGS
   S\" primitive trust reject: armed\nwindow: 70\n" RESULT ;

: RUN ( -- )
   T-RESET
   [: PREPARE CASES ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
