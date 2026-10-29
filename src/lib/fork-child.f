\ fork-child.f - the resets a process forked through PROC-FORK:RAW runs first:
\ fork copies only the calling thread, so process-wide state another task was
\ part way through (a lock it held, a setup it had begun) would wait for ever in
\ the child. src/lib/process-fork.f RAW runs every reset on the child's first
\ instruction after the fork, before any caller code.
\ STORAGE CLASS. PROCESS-WIDE: one registry, appended under an atomic lock.

require src/lib/errors.f
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
\ refuses E-LAYOUT-BOUNDS. A reset runs in the child alone: what it frees, it
\ frees for the child, never under the parent's holder. It is a few stores: it
\ runs before the fork has returned, so it must not throw, block or take a lock.
\ Only state a task may be stopped half way through belongs here: a lock whose
\ holder leaves its table whole at every instruction, a setup the child can
\ start over. A lock whose holder unmaps a table before it publishes the next
\ cannot be reset, so RAW holds it across the fork (src/lib/process-fork.f
\ FORK-HELD).
\ Owners register their own resets because not all sit beneath process-fork.f:
\ requiring src/lib/pg.f there would open libpq in every program that forks. A
\ module the engine provides, or one of the AOT linker's lib closure
\ (src/habu/app-image-core.f), never requires this file: an engine module's
\ cells here would belong to no capture claim (src/habu/aot-owned-cells.f), and
\ a linker module's reset would land in the application's registry with code
\ and cells the image does not carry (tools/hb-build-stripped-test.f
\ HBT-STRIPPED-FORK refuses that link). Such an owner publishes its reset
\ (TASK:CHILD-RESET, FS:CLEANUP-RESET, PROC-TREE:CHILD-RESET) and
\ src/lib/process-fork.f registers it.
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
