\ pg-fork-test.f - a forked child configures PG while another task holds its
\ configuration lock. fork copies only the calling thread, so the child finds the
\ process-wide lock held with nothing in it to let the lock go. CONFIG-LOCK is
\ private, so the case reopens PG (src/lib/test/fork-hold.f runs it). Loading
\ src/lib/pg.f opens libpq; the case needs no server.
\ Run: bin/hb --load src/lib/pg-fork-test.f

require src/lib/errors.f
require src/lib/test.f
require src/lib/pg.f
require src/lib/test/fork-hold.f

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
