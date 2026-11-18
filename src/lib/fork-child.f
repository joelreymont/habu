split-str
\ fork-child.f - the resets a process forked through PROC-FORK:RAW runs first:
\ fork copies only the calling thread, so process-wide state another task was
\ part way through (a lock it held, a setup it had begun) would wait for ever in
\ the child. RAW runs every reset in the child alone, before it returns there:
\ a reset frees for the child, never under the parent's holder, in stores and
\ async-signal-safe calls that never throw, block or take a lock. It resets only
\ state a task may be stopped half way through: a lock whose holder leaves its
\ table whole at every instruction, a setup the child can start over.
\ STORAGE CLASS. PROCESS-WIDE: one list of rows, each in the data of the file
\ that registered it, so a built program keeps its build's resets.

require src/core/rollback-cells.f

package FORK-CHILD
private

PERSISTED-PTR-VARIABLE HEAD
HEAD ROLLBACK-CELLS:MARK

\ A row is the next row's address, null for none, then the reset.
CAST: >RESET ( ^n -- ^[ -- ] )

: ROW-LINK ( ^n -- ^^n ) 0 ptr-field ;
: ROW-NEXT ( ^n -- ^n ) ROW-LINK @ ;
: ROW-RESET ( ^n -- ^[ -- ] ) cell+ >RESET ;

public

\ Registers a reset, from the top level of the file that owns it; the head is
\ published with a store-release, so a child forked mid-way runs the reset or not.
: REGISTER ( [ -- ] -- ) {: q :}
   datum here CELL-VIEW {: row:^ :}
   here ptr-cell-mark 0 ,
   0 ,
   q row ROW-RESET xt!
   HEAD @ row ROW-LINK !
   row HEAD atomic! ;

\ Runs every registered reset; outside a just-forked child it would free locks
\ live tasks hold.
: RESET ( -- )
   HEAD @ begin dup NULL-PTR <> while  dup ROW-RESET @ execute  ROW-NEXT  repeat drop ;

;package
