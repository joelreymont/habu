\ The admission controls run in private children; no image build is needed.
require src/lib/test.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package PAYLOAD-ADMISSION-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

: CASE-RUN ( ptr u8 n n ptr u8 n -- ) {: mode:ptr modeu:n expected:n text:ptr textu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/aot-payload-admission-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   rc expected <> if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc expected T=
   OUT outu LEN>N text textu CONTAINS?
   ERR erru LEN>N text textu CONTAINS? or TTRUE ;

: BAD ( ptr u8 n -- )
   74 s" aot-capture: capture bounds differ from frozen payload window" CASE-RUN ;

: RUN ( -- )
   T-RESET
   s" 0" 0 s" payload memo reuse: ok" CASE-RUN
   s" 7" 0 s" payload matching band: ok" CASE-RUN
   s" 1" BAD s" 2" BAD s" 3" BAD
   s" 4" BAD s" 5" BAD s" 6" BAD
   T-REPORT ;

RUN
;package
