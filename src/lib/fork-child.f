\ fork-child.f - the resets a process forked through PROC-FORK:RAW runs first:
\ fork copies only the calling thread, so process-wide state another task was
\ part way through (a lock it held, a setup it had begun) would wait for ever in
\ the child. RAW runs every reset in the child alone, before it returns there:
\ a reset frees for the child, never under the parent's holder, in stores and
\ async-signal-safe calls that never throw, block or take a lock. It resets only
\ state a task may be stopped half way through: a lock whose holder leaves its
\ table whole at every instruction, a setup the child can start over.
\ STORAGE CLASS. PROCESS-WIDE: one registry, appended under an atomic lock.

require src/lib/error.f
\ HOOKS below is a checked quotation store; the optimizing tier lowers such a
\ store through QUOTATION-STORAGE:STORE, so this file owns that dependency.
require src/core/quotation-storage.f

package FORK-CHILD
private

16 TYPED-BUFFER HOOKS [ -- ]
\ Atomic cells require native cell alignment.
here data-base - negate 7 and allot
variable N
variable MUTEX

: LOCK ( -- )
   begin 0 1 MUTEX atomic-cas 0= until ;

: UNLOCK ( -- ) 0 MUTEX atomic! ;

\ The slot is stored before the count that admits it, and the count with a
\ store-release, so a fork in the middle of a registration leaves the child the
\ whole hook or none of it.
: APPEND ( [ -- ] -- )
   N @ HOOKS !
   N @ 1+ N atomic! ;

public

\ Registers a reset, once, as the module that registers it loads; a full table
\ refuses E-LAYOUT-BOUNDS.
: REGISTER ( [ -- ] -- )
   LOCK [: APPEND ;] [: UNLOCK ;] finally ;

\ Runs every registered reset, oldest first, in a process just forked. Its one
\ caller is PROC-FORK:RAW's child arm: anywhere else it would free locks live
\ tasks hold. The registry's own lock goes first, since a registration another
\ task was in is part of what the fork copied.
: RESET ( -- )
   UNLOCK
   N @ 0 ?do i HOOKS @ execute loop ;

;package
