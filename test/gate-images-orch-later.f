require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f

package GATE-ORCH-LATER
private
create PATH FS-PATH-CAP allot
public
: RUN ( -- )
   s" HABU_GATE_ORCH_EVIDENCE" GETENV s" later" PATH JOIN-PATH
   PATH swap s" later passed" WRITE-ALL
   s" later row passed" type cr ;
;package

GATE-ORCH-LATER:RUN
