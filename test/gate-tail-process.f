\ gate-tail-process.f - focused stdlib tail-process runner.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/runner.f
require test/gate-pool.f
require test/gate-stats.f
require lib/content-key.f
require test/gate-stdlib-lib.f
require test/gate-stdlib-inline-lib.f

\ Member ratchets pin 15 direct engines and 167 SUBJECT forks. Direct engines
\ are only raw parity representatives, except seal.f's three clean-start
\ protected-WID capacity boundaries; all other semantic cases use SUBJECT.
package TAIL-GROUP

10000 constant MAX-MS

variable START-NS

: ELAPSED-MS ( -- n )
   mono-ns START-NS @ - PROC-NS-PER-MS / ;

: FAIL ( n -- ) {: elapsed:n :}
   s" tail-process elapsed-ms=" type elapsed GT-U-TYPE
   s"  max-ms=" type MAX-MS GT-U-TYPE cr
   s" tail process time ratchet exceeded" T-EX-FAIL die ;

public

: RUN ( -- )
   GSI-TIMINGS!
   mono-ns START-NS !
   GSI-TAIL-PROCESS
   ELAPSED-MS {: elapsed:n :}
   elapsed MAX-MS > if elapsed FAIL then ;

;package

TAIL-GROUP:RUN
