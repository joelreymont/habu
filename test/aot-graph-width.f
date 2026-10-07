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

: DIAGNOSTIC$ ( ptr u8 n -- ptr u8 n ) {: mode:ptr u:n :}
   mode u s" family" STR= IF
      s" tfam: a seeded effect has the wrong family arity" EXIT THEN
   mode u s" exception" STR= IF
      s" checker: exceptional quotation rows are not portable" EXIT THEN
   mode u s" scalar-zero" STR=
   mode u s" wide-width" STR= or
   mode u s" logical-width" STR= or
   mode u s" producer-scalar-zero" STR= or IF
      s" checker: captured row width disagrees with its type" EXIT THEN
   mode u s" recovery" STR= IF
      s" checker: a failed declaration's row is not portable" EXIT THEN
   s" checker: invalid captured effect graph" ;

: WIDTH? ( ptr u8 n -- bool ) {: mode:ptr u:n :}
   mode u s" scalar-zero" STR=
   mode u s" wide-width" STR= or
   mode u s" logical-width" STR= or ;

: CHILD ( ptr u8 n -- ) {: mode:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/native-window-owner-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/aot-graph-width-child.f" >LEN PROC-ARGV+
   s" src/core/declaration-transaction.f" >LEN PROC-ARGV+
   s" src/core/generated-declaration.f" >LEN PROC-ARGV+
   s" src/core/decl-event.f" >LEN PROC-ARGV+
   s" src/core/structure-make.f" >LEN PROC-ARGV+
   s" src/core/structure-decl.f" >LEN PROC-ARGV+
   s" src/core/enum-decl.f" >LEN PROC-ARGV+
   s" src/core/structures.f" >LEN PROC-ARGV+
   s" src/core/bytes.f" >LEN PROC-ARGV+
   s" src/core/dynamic-storage.f" >LEN PROC-ARGV+
   HB-TARGET-LINUX-X86-64? IF
      s" src/os/linux-x86-64/target.f" >LEN PROC-ARGV+
      s" src/os/linux-x86-64/layout.f" >LEN PROC-ARGV+
   ELSE HB-TARGET-LINUX? IF
      s" src/os/linux/target.f" >LEN PROC-ARGV+
      s" src/os/linux/layout.f" >LEN PROC-ARGV+
   ELSE
      s" src/os/macos/target.f" >LEN PROC-ARGV+
      s" src/os/macos/layout.f" >LEN PROC-ARGV+
   THEN THEN
   s" src/habu/stack-abi.f" >LEN PROC-ARGV+
   s" src/habu/layout.f" >LEN PROC-ARGV+
   s" src/os/env-base.f" >LEN PROC-ARGV+
   s" src/core/include.f" >LEN PROC-ARGV+
   s" src/habu/code-span.f" >LEN PROC-ARGV+
   s" src/habu/xref.f" >LEN PROC-ARGV+
   s" src/core/generated-declaration-dictionary.f" >LEN PROC-ARGV+
   s" src/core/generated-declaration-protection.f" >LEN PROC-ARGV+
   s" test/aot-graph-publication.f" >LEN PROC-ARGV+
   s" lib/tier.f" >LEN PROC-ARGV+
   PROC-ENV-RESET
   s" HABU_GRAPH_WIDTH_MODE" >LEN mode u >LEN PROC-ENV+
   WHITEBOX-CHILD:ENV+ PROC-ENV-INHERIT-MISSING
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 180000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   u 0= IF
      rc 0 <> IF OUT outu LEN>N type ERR erru LEN>N type cr THEN
      rc 0 T=
      OUT outu LEN>N s" graph width valid:" CONTAINS? TTRUE
      OUT outu LEN>N s" graph metadata load: ok" CONTAINS? TTRUE
   ELSE
      rc 76 <> IF OUT outu LEN>N type ERR erru LEN>N type cr THEN
      rc 76 T=
      OUT outu LEN>N s" graph corruption applied: " CONTAINS? TTRUE
      OUT outu LEN>N mode u CONTAINS? TTRUE
      mode u WIDTH? IF
         OUT outu LEN>N s" graph width refusal preserved publication state" CONTAINS? TTRUE
      THEN
      mode u DIAGNOSTIC$ {: diagnostic:ptr du:n :}
      OUT outu LEN>N diagnostic du CONTAINS?
      ERR erru LEN>N diagnostic du CONTAINS? or TTRUE
   THEN ;

: CASES ( -- )
   SCRIPT-ARGC 0= IF
      s" " CHILD s" scalar-zero" CHILD
      s" wide-width" CHILD s" logical-width" CHILD
      s" length" CHILD s" authority-bits" CHILD
      s" cycle" CHILD s" tag" CHILD
      s" variables" CHILD s" family" CHILD
      s" exception" CHILD
      s" producer-scalar-zero" CHILD s" recovery" CHILD EXIT
   THEN
   0 SCRIPT-ARGV$ s" scalar" STR= IF
      s" " CHILD s" scalar-zero" CHILD EXIT
   THEN
   0 SCRIPT-ARGV$ s" composite" STR= IF
      s" wide-width" CHILD s" logical-width" CHILD EXIT
   THEN
   0 SCRIPT-ARGV$ s" structure-a" STR= IF
      s" length" CHILD s" authority-bits" CHILD EXIT THEN
   0 SCRIPT-ARGV$ s" structure-b" STR= IF
      s" cycle" CHILD s" tag" CHILD EXIT THEN
   0 SCRIPT-ARGV$ s" structure-c" STR= IF
      s" variables" CHILD s" family" CHILD EXIT THEN
   0 SCRIPT-ARGV$ s" exception" STR= IF
      s" exception" CHILD EXIT THEN
   0 SCRIPT-ARGV$ s" source" STR= IF
      s" producer-scalar-zero" CHILD s" recovery" CHILD EXIT THEN
   s" aot-graph-width: unknown case group" 76 die ;

: RUN ( -- )
   T-RESET CLEANUP-RESET
   [: s" aot-graph-width" WHITEBOX-CHILD:PROVIDE CASES ;]
   [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
