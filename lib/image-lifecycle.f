\ Process resources are reset before an executable image captures live data.
require lib/prelude.f
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

public

\ Register when acquiring process-local state. Serialize growth and append;
\ allocation failure must also release the lock for the next registration.
: REGISTER ( [ -- ] -- )
   LOCK [: APPEND ;] [: UNLOCK ;] finally ;

\ Register at declaration time. These 64 DATA-backed slots remain armed
\ across PREPARE and image restore; a full table refuses E-LAYOUT-BOUNDS.
: REGISTER-PERSISTENT ( [ -- ] -- )
   LOCK [: APPEND-PERSISTENT ;] [: UNLOCK ;] finally ;

\ Number of hooks currently registered, read under the lock REGISTER takes so
\ a concurrent registration is either counted or not, never half-applied. A
\ cell read cannot throw, so the unwind path REGISTER needs is not needed here.
\ A hook that throws during PREPARE is not removed, so it still counts;
\ test/image-lifecycle.f observes both through this word.
: COUNT ( -- n )
   LOCK N @ PERSISTENT-N @ + UNLOCK ;

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
\ first; cleanup can register again, and a failed callback remains for retry.
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
   begin dup 0 > while 1- dup PERSISTENT @ execute repeat drop ;

\ THE FIVE CELLS A STRIPPED IMAGE REACHES THIS REGISTRY THROUGH, handed one at a
\ time to a claim the caller supplies. This file is baked into the engine, so
\ every cell it declares sits below any capture window; COUNT above takes the
\ lock and reads both counters, so its compiled code spells the first three
\ addresses, and REGISTER, REGISTER-PERSISTENT and PREPARE spell the two buffer
\ bases. That is the refusal every Tender entry point stopped at once the path
\ scratch let it open a file (measured: `caller=<unknown> value=13964713400
\ target=<unknown>`, the same integer from a program whose only call was
\ UNICODE:CASEFOLD=, which registers its cleanup on first use).
\ THE ZERO OF A FRESH MAPPING IS THE CORRECT START FOR ALL FIVE. A new process
\ has registered nothing: an open lock, two zero counts, no hook mapping and an
\ empty persistent table. Declaration-time registrations belong to the process
\ that made them - the builder ran them while it loaded the program - so an
\ image that starts with an empty registry is not missing state, it is a new
\ instance of it.
\ ONLY THE TWO BASES ARE NAMED, because they are the only cells of those two
\ tables that code spells. HOOKS is a DYNAMIC-BUFFER, whose accessor reaches
\ capacity and slot by arithmetic from the head cell (src/core/layout-buffer.f
\ DBUF-SOURCE), and PERSISTENT's generated accessor is `PERSISTENT#base i 8 * +`
\ (LBUF-SOURCE), so the 64 cells behind its base are reached at a computed
\ offset and never named in code.
\ A STRIPPED IMAGE CAN REGISTER NOW. APPEND stores a quotation into a declared
\ cell, which lowers through `xt!`: `xt!` stores and then declares the cell to
\ the engine's address-cell table, and a stripped image - which carries no reader
\ for that table - drops the declaration at link and keeps the store
\ (src/habu/aot-closure.f AOT-DECLARATION?). Before that drop, a program calling
\ REGISTER or PREPARE was refused with `aot: PC-relative target removed or
\ outside closure site=xt!`.
\ THE NAMING HAPPENS HERE BECAUSE THE CELLS ARE DECLARED HERE, exactly as in
\ src/core/dynamic-storage.f: the AOT list admits a cell only by name, these are
\ private, and this word hands out these five and nothing else.
: OWNED-CELLS ( [ ptr u8 -- ] -- ) {: claim :}
   N BYTE-VIEW claim execute
   PERSISTENT-N BYTE-VIEW claim execute
   MUTEX BYTE-VIEW claim execute
   HOOKS#base BYTE-VIEW claim execute
   PERSISTENT#base BYTE-VIEW claim execute ;

;package
