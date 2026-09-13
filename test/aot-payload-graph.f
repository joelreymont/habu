\ Supported graph metadata survives source-effect destruction and a fresh boot.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-id.f
require lib/engine-candidate.f

package PAYLOAD-GRAPH-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot
create ART FS-PATH-CAP allot
variable ART-U
create IMAGE FS-PATH-CAP allot
variable IMAGE-U

: ART$ ( -- ptr u8 n ) ART ART-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: SETUP ( -- )
   s" habu-payload-graph" TMPDIR-MKDIR {: path:ptr u:n :}
   path u CLEANUP-TREE+
   path u s" metadata.aot" ART JOIN-PATH ART-U !
   path u s" hb-partial" IMAGE JOIN-PATH IMAGE-U ! ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ARGS ( ptr u8 n -- ) {: fixture:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" ARG
   s" test/native-window-owner-child.f" ARG
   s" --" ARG
   fixture u ARG
   s" src/core/declaration-transaction.f" ARG
   s" src/core/generated-declaration.f" ARG
   s" src/core/decl-event.f" ARG
   s" src/core/structure-make.f" ARG
   s" src/core/structure-decl.f" ARG
   s" src/core/enum-decl.f" ARG
   s" src/core/structures.f" ARG
   s" src/core/bytes.f" ARG
   s" src/core/dynamic-storage.f" ARG
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" ARG s" src/os/linux/layout.f" ARG
   else
      s" src/os/macos/target.f" ARG s" src/os/macos/layout.f" ARG
   then
   s" src/habu/layout.f" ARG
   s" src/os/env-base.f" ARG
   s" src/core/include.f" ARG
   s" src/core/sha256.f" ARG
   s" lib/prelude.f" ARG
   PROC-ENV-RESET
   s" HABU_PAYLOAD_TEST_ARTIFACT" >LEN ART$ >LEN PROC-ENV+
   s" HABU_PAYLOAD_TEST_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: CASE-RUN ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n diagnostic:ptr diagnosticu:n :}
   s" test/aot-payload-graph-child.f" ARGS
   s" HABU_PAYLOAD_TEST_MODE" >LEN mode modeu >LEN PROC-ENV-SET
   ENGINE-CANDIDATE:PATH$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   modeu 0= if
      rc 0 <> OUT outu LEN>N s" graph metadata file roundtrip: ok" CONTAINS? 0= or
      if OUT outu LEN>N type ERR erru LEN>N type cr then
      rc 0 T=
      OUT outu LEN>N s" graph metadata file roundtrip: ok" CONTAINS? TTRUE
      OUT outu LEN>N S\" window: 0\n" CONTAINS? TTRUE
   else
      rc 76 <> if OUT outu LEN>N type ERR erru LEN>N type cr then
      rc 76 T=
      OUT outu LEN>N s" graph corruption applied: " CONTAINS? TTRUE
      OUT outu LEN>N mode modeu CONTAINS? TTRUE
      OUT outu LEN>N diagnostic diagnosticu CONTAINS?
      ERR erru LEN>N diagnostic diagnosticu CONTAINS? or TTRUE
      mode modeu s" scalar-zero" CORE-STR=
      mode modeu s" wide-width" CORE-STR= or
      mode modeu s" logical-width" CORE-STR= or IF
         OUT outu LEN>N s" graph width refusal preserved publication state" CONTAINS? TTRUE
      THEN
   then ;

: BAD-GRAPH ( ptr u8 n -- ) s" checker: invalid captured effect graph" CASE-RUN ;


\ Each synchronous call returns only after that process has exited. The reader
\ receives the file alone; the consumer receives only the newly emitted engine.
: CHILD ( ptr u8 n n ptr u8 n -- bool )
   {: engine:ptr engineu:n expected:n message:ptr messageu:n :}
   engine engineu >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N message messageu CONTAINS?
   ERR erru LEN>N message messageu CONTAINS? or {: said:bool :}
   rc expected <> said 0= or if
      OUT outu LEN>N type ERR erru LEN>N type cr
   then
   rc expected T= said TTRUE
   rc expected = said and ;


: CONSUMER-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" ARG s" test/aot-payload-native-consumer.f" ARG
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING ;


: FRESH-NATIVE ( -- )
   s" imported definitions are absent from the original engine" T-LABEL
   CONSUMER-ARGS
   ENGINE-CANDIDATE:PATH$ 70 s" E-UNDEFINED: PAYLOAD-NATIVE:BUMP" CHILD drop
   s" a native producer writes a portable two-cell family and verified effects" T-LABEL
   s" test/aot-payload-native-producer.f" ARGS
   ENGINE-CANDIDATE:PATH$ 0 s" native graph artifact written" CHILD 0= if exit then
   s" a fresh reader bakes the exited producer's artifact" T-LABEL
   PROC-ARGV-RESET
   s" --load" ARG s" test/aot-payload-native-reader.f" ARG s" --" ARG
   ART$ ARG IMAGE$ ARG ENGINE-CANDIDATE:PATH$ ARG
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ 0 s" native graph artifact baked" CHILD 0= if exit then
   s" a fresh boot executes imported code and compiles typed dependents" T-LABEL
   CONSUMER-ARGS
   IMAGE$ 0 s" native graph fresh consumer: ok" CHILD drop ;


: RUN ( -- )
   T-RESET SETUP
   s" " s" " CASE-RUN
   FRESH-NATIVE
   s" length" BAD-GRAPH
   s" authority-bits" BAD-GRAPH
   s" cycle" BAD-GRAPH
   s" tag" BAD-GRAPH
   s" variables" BAD-GRAPH
   s" family" s" tfam: a seeded effect has the wrong family arity" CASE-RUN
   s" exception" s" checker: exceptional quotation rows are not portable" CASE-RUN
   s" scalar-zero" s" checker: captured row width disagrees with its type" CASE-RUN
   s" wide-width" s" checker: captured row width disagrees with its type" CASE-RUN
   s" logical-width" s" checker: captured row width disagrees with its type" CASE-RUN
   s" producer-scalar-zero" s" checker: captured row width disagrees with its type" CASE-RUN
   CLEANUP-RUN T-REPORT ;

RUN
;package
