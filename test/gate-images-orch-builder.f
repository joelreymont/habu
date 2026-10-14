\ Controlled compiler bundle builder for gate-images-test.f. The release
\ row must run while this builder is live; a bounded wait makes a serial gate
\ fail promptly rather than hanging the focused test.
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/string.f
require src/lib/process-env.f
require src/os/script-argv.f

package GATE-ORCH-BUILDER
private
create PATH FS-PATH-CAP allot
create NOISE 1024 allot

: AT ( ptr u8 n -- ptr u8 n )
   0 SCRIPT-ARGV$ 2swap PATH JOIN-PATH PATH swap ;

: WAIT-RELEASE ( -- )
   mono-ns 60000000000 + {: deadline:n :}
   begin s" release" AT FILE? 0= while
      mono-ns deadline > if
         s" gate orchestration: independent row never released builder" 71 die
      then
   repeat ;

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
   WAIT-RELEASE
   s" builder saw release" type cr
   s" HABU_GATE_ORCH_FAIL" GETENV s" 1" STR= if
      FAILURE-NOISE
      s" builder failed after release" 73 die
   then
   s" hb-root" AT s" rooted" WRITE-ALL
   s" hb-root" AT CHMOD-X
   s" hb-ordinary" AT s" ordinary" WRITE-ALL
   s" hb-root.map" AT s" names" WRITE-ALL
   s" hb-whitebox-source" AT s" whitebox" WRITE-ALL
   s" hb-whitebox-source" AT CHMOD-X
   s" HABU_GATE_ORCH_EVIDENCE" GETENV s" builder-released" PATH JOIN-PATH
      PATH swap s" builder saw release" WRITE-ALL ;
;package

GATE-ORCH-BUILDER:RUN
