\ fsize-fork.f - the child writes `child`, the parent waits, then `parent`.
require src/lib/process.f
require src/lib/process-fork.f
package FSIZE-FORK
public
: RUN ( -- )
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if s" child" type cr  s" " 0 die then
   pid PROCESS:WAIT PROCESS:STATUS>RC RC>N 0<> if s" fsize-fork: the child failed" 1 die then
   s" parent" type cr ;
;package

FSIZE-FORK:RUN
