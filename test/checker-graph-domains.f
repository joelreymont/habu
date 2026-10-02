\ Real source effects are copied to saved graphs in a private matching engine.
\ Each malformed graph runs in its own process because graph refusal exits.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f

package GRAPH-DOMAIN-SUITE

$1000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

: RUN-CASE ( ptr u8 n n -- ) {: mode:ptr modeu:n want:n :}
   mode modeu T-LABEL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/checker-graph-domains-child.f" >LEN PROC-ARGV+
   PROC-ENV-RESET
   s" HABU_GRAPH_DOMAIN_MODE" >LEN mode modeu >LEN PROC-ENV+
   WHITEBOX-CHILD:ENV+
   PROC-ENV-INHERIT-MISSING
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc want <> IF OUT outu LEN>N type ERR erru LEN>N type cr THEN
   rc want T=
   want 0= IF
      s" shared-dag" mode modeu CORE-STR= IF
         OUT outu LEN>N s" shared graph bounded" CONTAINS? TTRUE
      ELSE
         OUT outu LEN>N s" graph domain accepted" CONTAINS? TTRUE
      THEN
   ELSE
      OUT outu LEN>N s" checker: invalid captured effect graph" CONTAINS?
      ERR erru LEN>N s" checker: invalid captured effect graph" CONTAINS? or
      dup 0= IF OUT outu LEN>N type ERR erru LEN>N type cr THEN TTRUE
   THEN ;

: CASES ( -- )
   s" baseline" 0 RUN-CASE
   s" region-separate" 76 RUN-CASE
   s" region-shared" 76 RUN-CASE
   s" scope-shared" 0 RUN-CASE
   s" nested-baseline" 0 RUN-CASE
   s" nested-region-separate" 76 RUN-CASE
   s" nested-region-shared" 76 RUN-CASE
   s" nested-scope-shared" 0 RUN-CASE
   s" shared-dag" 0 RUN-CASE ;

: RUN ( -- )
   T-RESET CLEANUP-RESET
   [: s" checker-graph-domains" WHITEBOX-CHILD:PROVIDE CASES ;]
   [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
