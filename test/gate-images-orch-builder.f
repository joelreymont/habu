\ Stands in for test/app-image-engine.f in gate-images-test.f's private tree.
\ ENSURE waits for a row queued after the consumer, so a serial gate times out.
require src/lib/error.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-env.f
require src/lib/test/wait.f

package APP-IMAGE-ENGINE
private
60000000000 constant RELEASE-NS
create PATH PATH-CAP allot

: EVIDENCE ( ptr u8 n -- ptr u8 n )
   s" HABU_GATE_ORCH_EVIDENCE" GETENV 2swap PATH FS:JOIN-PATH PATH swap ;

: RELEASE-SEEN? ( -- bool )
   s" release" EVIDENCE FS:FILE?
   s" later" EVIDENCE FS:FILE? and ;

: WAIT-RELEASE ( -- )
   [: RELEASE-SEEN? ;] mono-ns RELEASE-NS +
   s" gate-images-orch-builder WAIT-RELEASE" TEST-WAIT:POLL ;

: RELEASED ( -- )
   [: WAIT-RELEASE ;] catch {: code:n :}
   code E-PROC-TIMEOUT = if
      s" gate orchestration: independent rows never released builder"
      PROCESS:TIMEOUT-RC die
   then
   code 0<> if code throw then ;

public
: ENSURE ( -- )
   RELEASED
   s" builder saw release" type cr
   s" HABU_GATE_ORCH_FAIL" GETENV s" 1" STR= if
      s" builder failed after release" 73 die
   then
   s" app-image" EVIDENCE s" app image" FS:WRITE-ALL
   s" app-image" EVIDENCE FS:CHMOD-X
   s" builder-released" EVIDENCE s" builder saw release" FS:WRITE-ALL ;

: PATH$ ( -- ptr u8 n )
   s" app-image" EVIDENCE ;
;package
