\ process-fork.f - checked fork wrappers.
\
\ Load after native refresh: older engines do not have the fork primitive.

require lib/errors.f
require lib/process.f

: PROC-FORK-RAW ( -- pid )
   fork >PID ;

: PROC-FORK ( -- pid )
   PROC-FORK-RAW {: pid:pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;
