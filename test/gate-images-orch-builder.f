\ Controlled compiler bundle builder for gate-images-test.f. The release and
\ later rows must both run while this builder is live: later is registered
\ after consumer, which waits for this bundle, so later runs only when a row
\ waiting on images steps aside. A serial gate, or one that holds the registry
\ on consumer, never releases it, so its bounded wait expires: the builder
\ names the wait and exits PROC-TIMEOUT-RC, which the gate reports as a
\ timeout rather than hanging the focused test or counting a failure.
require src/lib/errors.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-env.f
require src/lib/test/wait.f
require src/os/script-argv.f

package GATE-ORCH-BUILDER
private
60000000000 constant RELEASE-NS
create PATH FS-PATH-CAP allot
create NOISE 1024 allot

: AT ( ptr u8 n -- ptr u8 n )
   0 SCRIPT-ARGV$ 2swap PATH JOIN-PATH PATH swap ;

: EVIDENCE ( ptr u8 n -- ptr u8 n )
   s" HABU_GATE_ORCH_EVIDENCE" GETENV 2swap PATH JOIN-PATH PATH swap ;

: RELEASE-SEEN? ( -- bool )
   s" release" AT FILE?
   s" later" EVIDENCE FILE? and ;

: WAIT-RELEASE ( -- )
   [: RELEASE-SEEN? ;] mono-ns RELEASE-NS +
   s" gate-images-orch-builder WAIT-RELEASE" TEST-WAIT:POLL ;

\ A throw code does not cross the process boundary: the expired wait leaves as
\ PROC-TIMEOUT-RC, and COMPILER-IMAGES:BUILD throws E-PROC-TIMEOUT again.
: RELEASED ( -- )
   [: WAIT-RELEASE ;] catch {: code:n :}
   code E-PROC-TIMEOUT = if
      s" gate orchestration: independent rows never released builder"
      PROC-TIMEOUT-RC die
   then
   code 0<> if code throw then ;

: FAILURE-NOISE ( -- )
   1024 0 ?do $78 NOISE i + c! loop
   s" builder stdout prefix marker" type cr
   36 0 ?do NOISE 1024 type loop
   s" builder stdout suffix marker" type cr
   1024 0 ?do $79 NOISE i + c! loop
   2 S\" builder stderr prefix marker\n" write drop
   36 0 ?do 2 NOISE 1024 write drop loop
   2 S\" builder stderr suffix marker\n" write drop ;

public
: RUN ( -- )
   s" HABU_GATE_COMPILER_BUNDLE" GETENV s" 1" STR= 0= if
      s" gate orchestration: compiler bundle switch missing" 72 die
   then
   RELEASED
   s" builder saw release" type cr
   s" HABU_GATE_ORCH_FAIL" GETENV s" 1" STR= if
      FAILURE-NOISE
      s" builder failed after release" 73 die
   then
   s" hb-root" AT s" rooted" WRITE-ALL
   s" hb-root" AT CHMOD-X
   s" hb-ordinary" AT s" ordinary" WRITE-ALL
   s" hb-root.map" AT s" names" WRITE-ALL
   s" hb-ordinary.map" AT s" names" WRITE-ALL
   s" hb-whitebox-source" AT s" whitebox" WRITE-ALL
   s" hb-whitebox-source" AT CHMOD-X
   s" builder-released" EVIDENCE s" builder saw release" WRITE-ALL ;
;package

GATE-ORCH-BUILDER:RUN
