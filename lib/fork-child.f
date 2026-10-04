\ fork-child.f - what a process forked through PROC-FORK:RAW resets first.
\
\ fork copies only the thread that calls it. Process-wide state another task
\ was part way through at that moment - a lock it held, a setup it had begun -
\ is copied into the child as it stood, and no thread in the child will ever
\ finish it, so the child's first use of that state would wait for ever. A
\ reset for each such state is registered here once, and lib/process-fork.f RAW
\ runs every one on the child's first instruction after the fork, before any
\ caller code. A reset runs in the child alone: what it frees, it frees for the
\ child, never under the parent's holder.
\
\ ONLY STATE A TASK MAY BE STOPPED HALF WAY THROUGH BELONGS HERE: a lock whose
\ holder leaves its table whole at every instruction, a setup the child can
\ start over. A lock whose holder unmaps a table before it publishes the next
\ one cannot be reset - the child could not tell where the holder stopped - so
\ RAW holds those across the fork instead (lib/process-fork.f FORK-HELD).
\
\ A RESET IS A FEW STORES. It runs before the fork has returned to its caller,
\ so it must not throw, block or take a lock.
\
\ WHY A REGISTRY. The owners do not all sit beneath lib/process-fork.f:
\ lib/pg.f sits above it, and requiring pg.f there to reach its lock would
\ open libpq in every program that forks. So lib/pg.f and lib/serial.f
\ register their own resets as they load.
\
\ WHO MAY NOT REGISTER. A module the engine provides, or one of the AOT
\ linker's lib closure (src/habu/app-image-core.f), never requires this file.
\ An engine module sits beneath every capture window, where the cells of this
\ file would belong to no claim (src/habu/aot-owned-cells.f). A linker module
\ loads above an application's window while the application is linked, so a
\ reset it registered as it loaded would land in the application's registry
\ with code and cells the image does not carry (tools/hb-build-stripped-test.f
\ HBT-STRIPPED-FORK refuses that link). Such an owner publishes its reset -
\ TASK:CHILD-RESET, CLEANUP-RESET, PROC-TREE:CHILD-RESET - and
\ lib/process-fork.f, which requires it and holds RAW, the one fork,
\ registers it.
\
\ The child inherits the registrations with the rest of the process: it runs the
\ same loaded code, so they are its own.

require lib/errors.f
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

\ Registers a reset, once, as the module that registers it loads. A full table
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
