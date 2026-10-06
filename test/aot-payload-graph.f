\ Supported graph metadata survives source-effect destruction and a fresh boot.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require test/whitebox-child.f

package PAYLOAD-GRAPH-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot
variable PRE-OUT-U
create ART FS-PATH-CAP allot
variable ART-U
create IMAGE FS-PATH-CAP allot
variable IMAGE-U
create SUBJECT FS-PATH-CAP allot
variable SUBJECT-U

: ART$ ( -- ptr u8 n ) ART ART-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: PRE-OUT$ ( -- ptr u8 n ) OUT PRE-OUT-U @ ;

\ The child runs test/native-window-owner-child.f, which reopens the engine's
\ build window: `hb: internal engine word: DECLARATIONS`, exit 70 on the sealed
\ product. So every child runs on the engine test/whitebox-child.f names, in the
\ temp root this file already owns.
: SETUP ( -- )
   s" habu-payload-graph" HB-TMP-MKDIR {: path:ptr u:n :}
   path u CLEANUP-TREE+
   path u s" metadata.aot" ART JOIN-PATH ART-U !
   path u s" hb-partial" IMAGE JOIN-PATH IMAGE-U !
   path u s" visibility.f" SUBJECT JOIN-PATH SUBJECT-U !
   path u WHITEBOX-CHILD:PROVIDE-IN ;

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
   s" src/habu/stack-abi.f" ARG
   s" src/habu/layout.f" ARG
   s" src/os/env-base.f" ARG
   s" src/core/include.f" ARG
   s" src/core/sha256.f" ARG
   s" lib/prelude.f" ARG
   PROC-ENV-RESET
   s" HABU_PAYLOAD_TEST_ARTIFACT" >LEN ART$ >LEN PROC-ENV+
   s" HABU_PAYLOAD_TEST_ENGINE" >LEN WHITEBOX-CHILD:ENGINE$ >LEN PROC-ENV+
   WHITEBOX-CHILD:ENV+
   PROC-ENV-INHERIT-MISSING ;

: CASE-RUN ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n diagnostic:ptr diagnosticu:n :}
   s" test/aot-payload-graph-child.f" ARGS
   s" HABU_PAYLOAD_TEST_MODE" >LEN mode modeu >LEN PROC-ENV-SET
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
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
   WHITEBOX-CHILD:ENV! ;


: VERIFY-CHILD ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n bool -- )
   {: engine:ptr engineu:n source:ptr sourceu:n status:ptr statusu:n packet:ptr packetu:n pre:bool :}
   PROC-ARGV-RESET
   s" --load" ARG s" tools/check-verify-child.f" ARG
   s" --" ARG SUBJECT$ ARG
   pre if s" verifier-prepass" ARG then
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN engine engineu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   engine engineu >LEN source sourceu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N PRE-OUT-U !
   s" verifier child: " type engine engineu type cr
   source sourceu type cr
   OUT outu LEN>N type ERR erru LEN>N type cr
   rc 0 T=
   PRE-OUT$ status statusu CONTAINS? TTRUE
   packetu 0<> if PRE-OUT$ packet packetu CONTAINS? TTRUE then ;

: PREVERIFY-CHILD ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   true VERIFY-CHILD ;

: VISIBILITY-CASE ( ptr u8 n -- )
   s" : SEED-GLOBAL ( -- n ) 1 ; package SEED-VIS private : HIDDEN ( -- n ) 2 ; public : SHOWN ( -- n ) 3 ; ;package"
   s" check-verify: verified" s" " false VERIFY-CHILD
   PRE-OUT$ S\" \"word\":\"SEED-GLOBAL\",\"package\":\"\",\"visibility\":\"global\"" CONTAINS? TTRUE
   PRE-OUT$ S\" \"word\":\"HIDDEN\",\"package\":\"seed-vis\",\"visibility\":\"private\"" CONTAINS? TTRUE
   PRE-OUT$ S\" \"word\":\"SHOWN\",\"package\":\"seed-vis\",\"visibility\":\"public\"" CONTAINS? TTRUE ;

: PREVERIFY-CASES ( ptr u8 n -- )
   {: engine:ptr engineu:n :}
   s" a body using-shadow records its packet and stops as a refusal" T-LABEL
   engine engineu
   s" : SV-W ( -- n ) 1 ; package SV-U public : SV-W ( -- n ) 2 ; ;package using SV-U : SV-BODY ( -- n ) SV-W ; ;using"
   s" check-verify: stopped 70 " s" E-USING-SHADOW-GLOBAL" PREVERIFY-CHILD
   s" a does> using-shadow records its packet and stops as a refusal" T-LABEL
   engine engineu
   s" : SV-W ( n -- ) drop ; package SV-U public : SV-W ( n -- ) drop ; ;package using SV-U : SV-DO ( n -- ) create , does> ( -- ) @ SV-W ; ;using"
   s" check-verify: stopped 70 " s" E-USING-SHADOW-GLOBAL" PREVERIFY-CHILD
   s" a top-level using-shadow records its packet and stops as a refusal" T-LABEL
   engine engineu
   s" : SV-W ( n -- ) drop ; package SV-U public : SV-W ( n -- ) drop ; ;package using SV-U 1 SV-W ;using"
   s" check-verify: stopped 70 " s" E-USING-SHADOW-GLOBAL" PREVERIFY-CHILD
   s" an unfinished definition keeps the source stop code" T-LABEL
   engine engineu s" : SV-OPEN ( -- n ) 1"
   s" check-verify: stopped 7155 " s" " PREVERIFY-CHILD
   s" a bad trust signature keeps the checker stop code" T-LABEL
   engine engineu
   s\" : SV-SIG ( -- n ) 1 ; s\" SV-SIG\" s\" -- sv-no-such-type\" trust"
   s" check-verify: stopped 7156 " s" E-BAD-STORED-SIGNATURE" PREVERIFY-CHILD ;


: FRESH-NATIVE ( -- )
   s" imported definitions are absent from the original engine" T-LABEL
   CONSUMER-ARGS
   WHITEBOX-CHILD:ENGINE$ 70 s" E-UNDEFINED: PAYLOAD-NATIVE:BUMP" CHILD drop
   s" a native producer writes a portable two-cell family and verified effects" T-LABEL
   s" test/aot-payload-native-producer.f" ARGS
   s" test/aot-payload-native-prepare.f" ARG
   s" --literals" ARG
   WHITEBOX-CHILD:ENGINE$ 0 s" native graph artifact written" CHILD 0= if exit then
   s" a fresh reader bakes the exited producer's artifact" T-LABEL
   PROC-ARGV-RESET
   s" --load" ARG s" test/aot-payload-native-reader.f" ARG s" --" ARG
   ART$ ARG IMAGE$ ARG WHITEBOX-CHILD:ENGINE$ ARG
   WHITEBOX-CHILD:ENV!
   WHITEBOX-CHILD:ENGINE$ 0 s" native graph artifact baked" CHILD 0= if exit then
   s" a fresh boot executes imported code and compiles typed dependents" T-LABEL
   CONSUMER-ARGS
   IMAGE$ 0 s" native graph fresh consumer: ok" CHILD if
      s" the seeded verifier renders global, private and public definitions" T-LABEL
      IMAGE$ VISIBILITY-CASE
      IMAGE$ PREVERIFY-CASES
      s" the product verifier renders global, private and public definitions" T-LABEL
      ENGINE-CANDIDATE:PATH$ VISIBILITY-CASE
      ENGINE-CANDIDATE:PATH$ PREVERIFY-CASES
   then ;


: CASES ( -- )
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
   s" recovery" s" checker: a failed declaration's row is not portable" CASE-RUN ;

: RUN ( -- )
   T-RESET
   [: SETUP CASES ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
