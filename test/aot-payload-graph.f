\ A native producer's graph metadata crosses process exits: a fresh reader bakes
\ the exited producer's artifact, and a fresh boot of that image executes the
\ imported code and compiles typed dependents. On the seeded image, completion
\ selects the imported word, captured parser control holds and yields to a
\ replacement, and the preverifier keeps its refusal packets and stop codes.
\ A capture refuses a failed declaration's row.
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/fmt.f
require src/lib/test/subject.f
require test/whitebox-child.f
require test/window-child-args.f

package PAYLOAD-GRAPH-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot
create IN IO-CAP allot                  \ a verifier child's stdin
variable PRE-OUT-U
create ART PATH-CAP allot
variable ART-U
create IMAGE PATH-CAP allot
variable IMAGE-U
create SUBJECT PATH-CAP allot
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
   s" habu-payload-graph" FS:MKTEMP-DIR {: path:ptr u:n :}
   path u FS:CLEANUP-TREE+
   path u s" metadata.aot" ART FS:JOIN-PATH ART-U !
   path u s" hb-partial" IMAGE FS:JOIN-PATH IMAGE-U !
   path u s" control.f" SUBJECT FS:JOIN-PATH SUBJECT-U !
   path u WHITEBOX-CHILD:PROVIDE-IN ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ARGS ( ptr u8 n -- )
   WINDOW-CHILD-ARGS:PAYLOAD
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
   s" test/aot-payload-graph-child.f" OUT outu LEN>N ERR erru LEN>N rc SUBJECT:RC-TIMED-OUT
   rc 76 <> if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 76 T=
   OUT outu LEN>N s" graph corruption applied: " CONTAINS? TTRUE
   OUT outu LEN>N mode modeu CONTAINS? TTRUE
   OUT outu LEN>N diagnostic diagnosticu CONTAINS?
   ERR erru LEN>N diagnostic diagnosticu CONTAINS? or TTRUE ;


\ Each synchronous call returns only after that process has exited. The reader
\ receives the file alone; the consumer receives only the newly emitted engine.
: CHILD ( ptr u8 n n ptr u8 n -- bool )
   {: engine:ptr engineu:n expected:n message:ptr messageu:n :}
   engine engineu >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N PRE-OUT-U !
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


\ The verifier child's stdin for SOURCE: the empty line that ends the open
\ documents, none here, then SOURCE.
: CHILD-IN$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   u 1+ IO-CAP > if E-STR-CAPACITY throw then
   10 IN c!
   a IN 1+ u BYTE-COPY
   IN u 1+ ;

: VERIFY-WITH ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n bool ptr u8 n -- )
   {: engine:ptr engineu:n source:ptr sourceu:n status:ptr statusu:n packet:ptr packetu:n pre:bool child:ptr childu:n :}
   PROC-ARGV-RESET
   s" --load" ARG child childu ARG
   s" --" ARG SUBJECT$ ARG
   pre if s" verifier-prepass" ARG then
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN engine engineu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   engine engineu >LEN source sourceu CHILD-IN$ >LEN
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

: VERIFY-CHILD ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n bool -- )
   s" tools/check-verify-child.f" VERIFY-WITH ;

: PREVERIFY-CHILD ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   true VERIFY-CHILD ;

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


: COLD-COMPLETION$ ( -- ptr u8 n )
   s" using PAYLOAD-NATIVE 41 BUMP . ;using" ;

: WARM-COMPLETION$ ( -- ptr u8 n )
   s" : WARM-BUMP ( n -- n ) PAYLOAD-NATIVE:BUMP ; using PAYLOAD-NATIVE 41 BUMP . ;using" ;

\ Both sources end with the same cursor suffix: the position just after BU.
: COMPLETION-AT ( ptr u8 n -- n )
   nip s" MP . ;using" nip - ;

: COUNT-IN ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n b:ptr v:n :}
   v 0= u v < or if 0 exit then
   0 u v - 1+ 0 ?do
      a i + v b v CORE-STR= if 1+ then
   loop ;

: COMPLETION-CHILD ( ptr u8 n n -- )
   {: source:ptr sourceu:n at:n :}
   PROC-ARGV-RESET
   s" --load" ARG s" tools/check-verify-child.f" ARG
   s" --" ARG SUBJECT$ ARG s" --at" ARG
   SB-RESET at FMT:SB-U SB$ ARG
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN IMAGE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN source sourceu CHILD-IN$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   s" completion child: " type IMAGE$ type cr
   source sourceu type cr
   OUT outu LEN>N type ERR erru LEN>N type cr
   rc 0 T=
   OUT outu LEN>N s" check-verify: verified" CONTAINS? TTRUE
   OUT outu LEN>N S\" check-verify: candidate {\"word\":\"BUMP\"" COUNT-IN 1 T=
   OUT outu LEN>N S\" check-verify: candidate {\"word\":\"BUMP\"}" COUNT-IN 1 T= ;

: COMPLETION-CASES ( -- )
   s" verifier tooling leaves the seeded word without a checker symbol" T-LABEL
   PROC-ARGV-RESET
   s" --load" ARG s" test/aot-payload-native-cold-probe.f" ARG
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN IMAGE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   IMAGE$ 0 s" native graph cold verifier: ok" CHILD drop
   PRE-OUT$ type
   s" a fresh seeded image executes the imported bare public word" T-LABEL
   SUBJECT$ COLD-COMPLETION$ FS:WRITE-ALL
   PROC-ARGV-RESET s" --load" ARG SUBJECT$ ARG
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN IMAGE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   IMAGE$ 0 s" 42" CHILD drop
   PRE-OUT$ type cr
   s" cold top-level completion selects seeded BUMP once without a location" T-LABEL
   COLD-COMPLETION$ 2dup COMPLETION-AT COMPLETION-CHILD
   s" warmed top-level completion makes the same selection" T-LABEL
   WARM-COMPLETION$ 2dup COMPLETION-AT COMPLETION-CHILD ;

: COLD-CONTROL$ ( -- ptr u8 n )
   s" using PAYLOAD-NATIVE CPARSE MISSING ;using 4242 ." ;

: WARM-CONTROL$ ( -- ptr u8 n )
   s" : WARM-CPARSE ( -- ) PAYLOAD-NATIVE:CPARSE ; using PAYLOAD-NATIVE CPARSE MISSING ;using 4242 ." ;

\ The cold binding has no checker symbol, so its row is accepted but remains
\ opaque. A checked use materializes that same selected parser; its row then
\ bounds one operand, exposing the later unknown token.
: COLD-ROW-CONTROL$ ( -- ptr u8 n )
   s" parses: PAYLOAD-NATIVE:CPARSE 1 using PAYLOAD-NATIVE CPARSE MISSING ;using ROW-AFTER" ;

: WARM-ROW-CONTROL$ ( -- ptr u8 n )
   s" : WARM-CPARSE ( -- ) PAYLOAD-NATIVE:CPARSE ; parses: PAYLOAD-NATIVE:CPARSE 1 using PAYLOAD-NATIVE CPARSE MISSING ;using ROW-AFTER" ;

: RENDER-CONTROL$ ( -- ptr u8 n )
   s" PAYLOAD-NATIVE:CRENDER : AFTER-RENDER ( -- n ) RENDERED ; AFTER-RENDER ." ;

: DEFER-CONTROL$ ( -- ptr u8 n )
   s" PAYLOAD-NATIVE:CDEFER MISSING 4242 ." ;

: ALIAS-CONTROL$ ( -- ptr u8 n )
   s" PAYLOAD-NATIVE:ALIAS-PARSE MISSING 4242 ." ;

: GLOBAL-CONTROL$ ( -- ptr u8 n )
   s" GPARSE MISSING 4242 ." ;

: REPLACED-CONTROL$ ( -- ptr u8 n )
   s" undefine GPARSE : GPARSE ( -- ) ; GPARSE MISSING" ;

: LIVE-REPLACED-CONTROL$ ( -- ptr u8 n )
   s" GPARSE MISSING" ;

: CONTROL-RUN ( ptr u8 n n ptr u8 n -- )
   {: source:ptr sourceu:n rc:n message:ptr messageu:n :}
   SUBJECT$ source sourceu FS:WRITE-ALL
   PROC-ARGV-RESET s" --load" ARG SUBJECT$ ARG
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN IMAGE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   IMAGE$ rc message messageu CHILD drop ;

: CONTROL-CASES ( -- )
   s" the fresh image runs captured CPARSE and consumes its operand" T-LABEL
   COLD-CONTROL$ 0 s" 4242" CONTROL-RUN
   PRE-OUT$ type cr
   s" a cold verifier defers the source consumed by captured CPARSE" T-LABEL
   IMAGE$ COLD-CONTROL$ s" check-verify: deferred" s" " false VERIFY-CHILD
   s" a qualified checked intake preserves the same control behavior" T-LABEL
   IMAGE$ WARM-CONTROL$ s" check-verify: deferred" s" " false VERIFY-CHILD
   s" a cold selected parser accepts its row without materialization" T-LABEL
   IMAGE$ COLD-ROW-CONTROL$ s" check-verify: deferred" s" " false VERIFY-CHILD
   PRE-OUT$ s" E-PARSES-ROW" CONTAINS? TFALSE
   s" a materialized selected parser row has the same bound" T-LABEL
   IMAGE$ WARM-ROW-CONTROL$ s" check-verify: refused" s" E-UNDEFINED-TOP-LEVEL" false VERIFY-CHILD
   PRE-OUT$ S\" \"token\":\"ROW-AFTER\"" CONTAINS? TTRUE
   s" a captured renderer defines a name absent from source" T-LABEL
   RENDER-CONTROL$ 0 s" 42" CONTROL-RUN
   IMAGE$ RENDER-CONTROL$ s" check-verify: deferred" s" " false VERIFY-CHILD
   s" a captured deferred word leaves its following source to the run" T-LABEL
   IMAGE$ DEFER-CONTROL$ s" check-verify: deferred" s" " false VERIFY-CHILD
   s" an exported alias retains its captured parsing behavior" T-LABEL
   ALIAS-CONTROL$ 0 s" 4242" CONTROL-RUN
   IMAGE$ ALIAS-CONTROL$ s" check-verify: deferred" s" " false VERIFY-CHILD
   s" a captured global parser consumes its operand before replacement" T-LABEL
   GLOBAL-CONTROL$ 0 s" 4242" CONTROL-RUN
   IMAGE$ GLOBAL-CONTROL$ s" check-verify: deferred" s" " false VERIFY-CHILD
   s" a source replacement cannot inherit the captured parser control" T-LABEL
   REPLACED-CONTROL$ 70 s" MISSING" CONTROL-RUN
   IMAGE$ REPLACED-CONTROL$ s" check-verify: refused" s" E-UNDEFINED-TOP-LEVEL" false VERIFY-CHILD
   s" a live replacement does not inherit captured parser control" T-LABEL
   IMAGE$ LIVE-REPLACED-CONTROL$ s" check-verify: refused" s" E-UNDEFINED-TOP-LEVEL"
   false s" test/aot-payload-native-replace-child.f" VERIFY-WITH ;


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
      COMPLETION-CASES
      CONTROL-CASES
      IMAGE$ PREVERIFY-CASES
   then ;


: CASES ( -- )
   FRESH-NATIVE
   s" recovery" s" checker: a failed declaration's row is not portable" CASE-RUN ;

: RUN ( -- )
   T-RESET
   [: SETUP CASES ;] [: FS:CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
