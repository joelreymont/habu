\ Process resources are reset before an executable image captures live data.
require src/lib/prelude.f
require src/lib/errors.f
\ HOOKS below is a checked quotation store; the optimizing tier lowers such a
\ store through QUOTATION-STORAGE:STORE, so this file owns that dependency.
require src/core/quotation-storage.f

package IMAGE-LIFECYCLE
private

DYNAMIC-BUFFER HOOKS [ -- ]
variable N
\ Declaration-time hooks survive captures; typed DATA cells own relocation.
64 TYPED-BUFFER PERSISTENT [ -- ]
variable PERSISTENT-N
\ Declaration-time DATA cells whose values belong to the running process.
64 TYPED-BUFFER PROCESS ptr n
variable PROCESS-N
\ Atomic cells require native cell alignment. This dictionary storage is shared
\ by every task, unlike the engine's per-task DATA header.
here data-base - negate 7 and allot
variable MUTEX

: LOCK ( -- )
   begin 0 1 MUTEX atomic-cas 0= until ;

: UNLOCK ( -- ) 0 MUTEX atomic! ;

: APPEND ( [ -- ] -- )
   N @ 1+ HOOKS-RESERVE
   N @ HOOKS !
   1 N +! ;

: APPEND-PERSISTENT ( [ -- ] -- )
   PERSISTENT-N @ PERSISTENT !
   1 PERSISTENT-N +! ;

: APPEND-PROCESS ( ptr n -- )
   PROCESS-N @ PROCESS !
   1 PROCESS-N +! ;

public

\ Register when acquiring process-local state. Serialize growth and append;
\ allocation failure must also release the lock for the next registration.
: REGISTER ( [ -- ] -- )
   LOCK [: APPEND ;] [: UNLOCK ;] finally ;

\ Register at declaration time. These 64 DATA-backed slots remain armed
\ across PREPARE and image restore; a full table refuses E-LAYOUT-BOUNDS.
: REGISTER-PERSISTENT ( [ -- ] -- )
   LOCK [: APPEND-PERSISTENT ;] [: UNLOCK ;] finally ;

\ Declare a DATA cell whose value belongs to the running process: the image
\ writer stores it as 0, and the live process keeps its value through every
\ capture, one that throws included. 64 slots; a full table refuses
\ E-LAYOUT-BOUNDS.
: PROCESS-CELL ( ptr n -- )
   LOCK [: APPEND-PROCESS ;] [: UNLOCK ;] finally ;

\ Visit every declared process cell, in declaration order. The image writer
\ runs this after application tasks stop, as PREPARE does.
: PROCESS-CELLS ( [ ptr n -- ] -- ) {: visit :}
   PROCESS-N @ 0 ?do i PROCESS @ visit execute loop ;

\ Number of hooks currently registered, read under the lock REGISTER takes so
\ a concurrent registration is either counted or not, never half-applied. A
\ cell read cannot throw, so the unwind path REGISTER needs is not needed here.
\ A hook that throws during PREPARE is not removed, so it still counts;
\ test/image-lifecycle.f observes both through this word.
: COUNT ( -- n )
   LOCK N @ PERSISTENT-N @ + UNLOCK ;

\ Runs a fork with the lock held; PROC-FORK:RAW is its caller. A registration
\ may grow HOOKS, which unmaps the old table before it publishes the new one, so
\ a child forked while another task was inside could neither wait for that task
\ nor carry on where it stopped. Held, nobody is inside when the process is
\ copied, and parent and child each free their own copy.
: ACROSS-FORK ( [ -- n ] -- n )
   LOCK [: UNLOCK ;] finally ;

private


\ Cleanup may register another resource. Keep those entries when removing
\ the completed callback, and retain the original entry if it throws.
: REMOVE ( n -- )
   1+ N @ swap ?do
      i HOOKS @ i 1- HOOKS !
   loop
   -1 N +! ;

public


\ Capture runs after application tasks stop. Reverse order releases dependents
\ first; cleanup can register again, and a failed callback remains for retry. A
\ refusing callback must not clear its owner's registered flag before it
\ rethrows: the owner sets that flag only after REGISTER completes, or the next
\ REGISTER adds a second entry (src/lib/f64-text.f CLEANUP-NATIVE, REGISTER-CLEANUP).
\ A one-shot hook registered by a persistent one is refused, E-LIFECYCLE-LATE: the
\ one-shot table is already released, and running the hook now would put cleanup
\ that may call foreign functions after the hooks that forget their addresses.
\ Admitted, an image saved from the keyed linker image counted 4 hooks, one in no
\ table, and its first PREPARE threw E-BOUNDS (test/image-lifecycle-late-register.f).
: PREPARE ( -- )
   begin N @ 0 > while
      N @ 1- {: at:n :}
      at HOOKS @ execute
      at REMOVE
   repeat
   HOOKS-RELEASE
   \ One-shot cleanup may use foreign functions. Forget their addresses last,
   \ after resource cleanup has finished, in reverse declaration order.
   PERSISTENT-N @
   begin dup 0 > while 1- dup PERSISTENT @ execute repeat drop
   N @ 0 > if E-LIFECYCLE-LATE throw then ;

\ The eight spans a stripped image reaches this registry through, each handed to
\ the caller's claim. This file is baked into the engine, so its cells sit below
\ any capture window, and the AOT list admits a cell only by name: COUNT spells
\ the lock and both counters, REGISTER, REGISTER-PERSISTENT and PREPARE the two
\ buffer bases and HOOKS's control block, PROCESS-CELL and PROCESS-CELLS PROCESS-N
\ and PROCESS's base. An unclaimed one is refused `caller=<unknown>
\ value=13964713400 target=<unknown>` (measured from a program whose only call was
\ UNICODE:CASEFOLD=, which registers its cleanup on first use).
\ A fresh mapping's zero is the right start for all eight: an open lock, three zero
\ counts, no hook mapping or capacity, empty tables. Declaration-time
\ registrations belong to the builder process that ran them.
\ Only cells code spells are named: HOOKS, a DYNAMIC-BUFFER, passes `HOOKS#base
\ HOOKS#ctl` (src/core/layout-buffer.f DBUF-SOURCE), the block claimed with its
\ extent; PERSISTENT's and PROCESS's accessors are `#base i 8 * +` (LBUF-SOURCE),
\ so their 64 cells are reached by offset and never named.
\ `xt!` stores and then declares the cell to the address-cell table; a stripped
\ image drops the declaration at link and keeps the store
\ (src/habu/aot-closure.f AOT-DECLARATION?). Without that, REGISTER and PREPARE are
\ refused `aot: PC-relative target removed or outside closure site=xt!`. As in
\ src/core/dynamic-storage.f, the cells are named here because they are declared here.
: OWNED-CELLS ( [ ptr u8 n -- ] -- ) {: claim :}
   N BYTE-VIEW CELL claim execute
   PERSISTENT-N BYTE-VIEW CELL claim execute
   MUTEX BYTE-VIEW CELL claim execute
   HOOKS#base BYTE-VIEW CELL claim execute
   HOOKS#ctl BYTE-VIEW DYNAMIC-STORAGE:CONTROL-CELLS cells claim execute
   PERSISTENT#base BYTE-VIEW CELL claim execute
   PROCESS-N BYTE-VIEW CELL claim execute
   PROCESS#base BYTE-VIEW CELL claim execute ;

;package
