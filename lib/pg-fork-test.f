\ pg-fork-test.f - a forked child configures PG while another task holds its
\ configuration lock.
\
\     bin/hb --load lib/pg-fork-test.f
\
\ PG's configuration lock is process-wide, and fork copies only the thread that
\ calls it: a child forked while another task configures PG finds the lock
\ held, with nothing in the child to let it go, and its own PG:CONFIGURE waits
\ for ever. The holder takes CONFIG-LOCK, which is private, so the case reopens
\ PG (lib/test/fork-hold.f runs it). Loading lib/pg.f opens libpq; the case
\ needs no server.

require lib/errors.f
require lib/test.f
require lib/pg.f
require lib/test/fork-hold.f

T-RESET

package PG

private

: FORK-UNDER-CONFIG ( -- )
   [: CONFIG-LOCK [: FORK-HOLD:HOLDING ;] [: CONFIG-UNLOCK ;] finally ;] FORK-HOLD:HOLD
   s" a child forked under a held configuration lock configured PG"
   [: 1 1 1 PG:CONFIGURE ;] 1 FORK-HOLD:FORK-CHECK
   FORK-HOLD:RELEASE ;

FORK-UNDER-CONFIG

;package

T-REPORT
s" pg-fork-test: ok" type cr
