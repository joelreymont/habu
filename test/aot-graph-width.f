\ Saved effect-width validation from real declarations without ARM capture.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require src/os/script-argv.f
require test/whitebox-child.f

package GRAPH-WIDTH-SUITE
$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

: CHILD ( ptr u8 n -- ) {: mode:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/native-window-owner-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/aot-graph-width-child.f" >LEN PROC-ARGV+
   HB-TARGET-LINUX? IF
      s" src/os/linux/target.f" >LEN PROC-ARGV+
      s" src/os/linux/layout.f" >LEN PROC-ARGV+
   ELSE
      s" src/os/macos/target.f" >LEN PROC-ARGV+
      s" src/os/macos/layout.f" >LEN PROC-ARGV+
   THEN
   s" src/habu/stack-abi.f" >LEN PROC-ARGV+
   s" src/habu/layout.f" >LEN PROC-ARGV+
   s" src/os/env-base.f" >LEN PROC-ARGV+
   s" test/aot-graph-publication.f" >LEN PROC-ARGV+
   PROC-ENV-RESET
   s" HABU_GRAPH_WIDTH_MODE" >LEN mode u >LEN PROC-ENV+
   WHITEBOX-CHILD:ENV+ PROC-ENV-INHERIT-MISSING
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 90000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   u 0= IF
      rc 0 <> IF OUT outu LEN>N type ERR erru LEN>N type cr THEN
      rc 0 T=
      OUT outu LEN>N s" graph width valid:" CONTAINS? TTRUE
   ELSE
      rc 76 <> IF OUT outu LEN>N type ERR erru LEN>N type cr THEN
      rc 76 T=
      OUT outu LEN>N s" graph corruption applied: " CONTAINS? TTRUE
      OUT outu LEN>N mode u CONTAINS? TTRUE
      OUT outu LEN>N s" graph width refusal preserved publication state" CONTAINS? TTRUE
      OUT outu LEN>N s" checker: captured row width disagrees with its type" CONTAINS?
      ERR erru LEN>N s" checker: captured row width disagrees with its type" CONTAINS? or TTRUE
   THEN ;

: CASES ( -- )
   SCRIPT-ARGC 0= IF
      s" " CHILD s" scalar-zero" CHILD
      s" wide-width" CHILD s" logical-width" CHILD EXIT
   THEN
   0 SCRIPT-ARGV$ s" scalar" STR= IF
      s" " CHILD s" scalar-zero" CHILD EXIT
   THEN
   0 SCRIPT-ARGV$ s" composite" STR= IF
      s" wide-width" CHILD s" logical-width" CHILD EXIT
   THEN
   s" aot-graph-width: unknown case group" 76 die ;

: RUN ( -- )
   T-RESET CLEANUP-RESET
   [: s" aot-graph-width" WHITEBOX-CHILD:PROVIDE CASES ;]
   [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
