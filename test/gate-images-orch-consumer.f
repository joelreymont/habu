require src/lib/fs.f
require src/lib/string.f
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
   WHITEBOX-ENGINE:PATH$ FS:EXECUTABLE? 0= if
      s" consumer saw no whitebox source" 75 die
   then
   s" consumer" EVIDENCE$ s" consumer passed" FS:WRITE-ALL
   s" consumer row passed" type cr ;
;package

GATE-ORCH-CONSUMER:RUN
