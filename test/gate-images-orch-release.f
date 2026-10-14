require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f

package GATE-ORCH-RELEASE
private
create PATH FS-PATH-CAP allot
public
: RUN ( -- )
   s" HABU_GATE_COMPILER_IMAGES" GETENV s" release" PATH JOIN-PATH
   PATH swap s" release" WRITE-ALL
   s" release row passed" type cr ;
;package

GATE-ORCH-RELEASE:RUN
