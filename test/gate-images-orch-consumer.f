require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/string.f
require src/lib/process-env.f
require test/c2-image-pair.f
require test/whitebox-engine.f

package GATE-ORCH-CONSUMER
private
create PATH PATH-CAP allot

: EVIDENCE$ ( ptr u8 n -- ptr u8 n )
   s" HABU_GATE_ORCH_EVIDENCE" GETENV 2swap PATH FS:JOIN-PATH PATH swap ;

public
: RUN ( -- )
   s" consumer-started" EVIDENCE$ s" consumer entered" FS:WRITE-ALL
   s" HABU_GATE_ORCH_FAIL" GETENV s" 1" STR= if
      s" consumer ran after failed builder" 76 die
   then
   s" HABU_GATE_ORCH_EVIDENCE" GETENV s" pair" PATH FS:JOIN-PATH
      PATH swap FS:MAKE-DIR
   s" pair" EVIDENCE$ C2-IMAGE-PAIR:PROVIDE
   WHITEBOX-ENGINE:PATH$ FS:EXECUTABLE? 0= if
      s" consumer saw no whitebox source" 75 die
   then
   s" consumer" EVIDENCE$ s" consumer passed" FS:WRITE-ALL
   s" consumer row passed" type cr ;
;package

GATE-ORCH-CONSUMER:RUN
