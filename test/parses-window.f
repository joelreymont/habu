\ The operand declarers keep their intrinsic ids and CTL-PARSES on a freshly
\ transferred checker: the build's replacement-checker handover, which
\ test/parses-window-child.f meets after the window's own cell-effects.f.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f

package PARSES-WINDOW-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

\ The child runs test/native-window-owner-child.f, which reopens the engine's
\ build window, refused on the sealed product; so it runs on the engine
\ test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" parses-window" WHITEBOX-CHILD:PROVIDE ;

: ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/native-window-owner-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/parses-window-child.f" >LEN PROC-ARGV+
   WHITEBOX-CHILD:ENV! ;

: CHECK ( -- )
   ARGS
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 180000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0 <> if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N S\" parses window: ok\nwindow: 0\n" T$= ;

: RUN ( -- )
   T-RESET
   [: PREPARE CHECK ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
