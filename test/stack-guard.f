\ The guard-page contract itself: the shared refusal code, filling a guarded
\ stack to its real capacity, the three named overflow faults, run-in-stack's
\ structural refusals, and a task's own guarded stack.
require test/engine-stack-lifecycle.f
require lib/memory.f

package STACK-LIFECYCLE-TEST

: STACK-CONTRACT-AGREEMENT ( -- )
   s" E-STACK-UNGUARDED matches the engine's own spelling" T-LABEL
   E-STACK-UNGUARDED STACK-ABI:E-STACK-UNGUARDED T= ;

\ A bare sequence of literal pushes at the top level is interpreted directly,
\ not compiled as a checked word, so it carries no declared effect and the
\ runtime data stack really holds COUNT live cells. The boot data stack is one
\ guarded page, [base, base + STACK-ABI:BOOT-BYTES) read/write with an
\ inaccessible page on each side (src/habu/rt.f EMIT-MAP). The interpreter's
\ token loop keeps a few working cells above the live depth, so the last
\ handful of cells are reachable only from compiled code; the fixture brackets
\ that: a fill short of the page by a margin succeeds, a fill of the whole
\ page faults (data), which places the guard page exactly at base + BOOT-BYTES.
16 constant FILL-MARGIN-CELLS
STACK-ABI:BOOT-BYTES 8 / 2 * constant LIT-SRC-CAP   \ "1 " per cell of the page
create LIT-SRC LIT-SRC-CAP allot

: FILL-LITERALS$ ( n -- ptr u8 n ) {: count:n :}
   count 2 * LIT-SRC-CAP > if E-STR-CAPACITY throw then
   count 0 do
      $31 LIT-SRC i 2 * + c!
      $20 LIT-SRC i 2 * + 1 + c!
   loop
   LIT-SRC count 2 * ;

: PAGE-CAPACITY ( -- )
   s" filling the boot stack short of the page by a margin succeeds" T-LABEL
   STACK-ABI:BOOT-BYTES 8 / FILL-MARGIN-CELLS - FILL-LITERALS$ CHILD-RC 0 T=
   s" filling the whole page overflows the data stack" T-LABEL
   STACK-ABI:BOOT-BYTES 8 / FILL-LITERALS$ REFUSED-DATA ;

\ A word cannot call itself by bare name inside its own definition (it is not
\ in the dictionary yet: E-UNDEFINED), so unbounded recursion goes through
\ RECURSE, and RECURSE only leaves a residual value unreconciled against the
\ declared effect inside a BEGIN...AGAIN loop, whose unconditional back edge
\ makes the fall-through to `;` unreachable -- a bare tail call (`1 recurse ;`,
\ no AGAIN) is rejected at 'recurse' for exactly that residual. Verified
\ against the checker on the current engine. Each of the three named stacks
\ overflows the same way: a plain push, a return-stack push, and a DO frame
\ opened without ever reaching LOOP to close it.
: OVERFLOW-NAMES ( -- )
   s" unbounded data-stack recursion overflows (data)" T-LABEL
   s" : X ( n -- ) begin dup recurse again ; 0 X" REFUSED-DATA
   s" unbounded return-stack recursion overflows (return)" T-LABEL
   s" : Y ( -- ) 1 >r recurse r> drop ; Y" REFUSED-RETURN
   s" unbounded DO-frame recursion overflows (loop)" T-LABEL
   s" : Z ( -- ) 1 0 do recurse loop ; Z" REFUSED-LOOP ;

create GBUF 32 allot   \ an ordinary create/allot buffer: never a guarded mapping

: PLAIN-BUFFER-REFUSAL ( -- )
   ['] EMPTY GBUF STACK-ABI:PAGE-BYTES run-in-stack ;

: CAPACITY-ZERO-REFUSAL ( -- )
   STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop {: base:ptr :}
   ['] EMPTY base 0 run-in-stack ;

\ A real guarded base shifted by one cell (8 bytes) is neither the mapping's
\ own base nor PAGE-BYTES aligned any more, even though it still points inside
\ readable/writable memory -- GUARDED-EXTENT?'s alignment proof, not a bounds
\ probe, is what catches it.
: UNALIGNED-BASE-REFUSAL ( -- )
   STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop {: base:ptr :}
   ['] EMPTY base 8 + STACK-ABI:PAGE-BYTES run-in-stack ;

: RUN-IN-STACK-REFUSALS ( -- )
   s" a create/allot buffer is refused" T-LABEL
   ['] PLAIN-BUFFER-REFUSAL catch E-STACK-UNGUARDED T=
   s" capacity 0 on a real guarded pointer is refused" T-LABEL
   ['] CAPACITY-ZERO-REFUSAL catch E-STACK-UNGUARDED T=
   s" a guarded base shifted by 8 bytes is refused" T-LABEL
   ['] UNALIGNED-BASE-REFUSAL catch E-STACK-UNGUARDED T= ;

\ lib/task.f PREPARE gives every task its own guarded data/return/loop stacks
\ (TASK-STACK-BYTES rounds the requested size up to whole guard pages); a task
\ overflowing its own data stack faults the same way the main thread's does,
\ and the crash handler's exit_group kills the whole process, so the overflow
\ is observable as this child process exiting 102 -- there is no per-task exit,
\ only the process-wide fail-closed one.
\ TASK:ACTIVATE ( [ -- ] ptr n -- ) needs a zero-effect quotation, so the
\ recursive overflow word (declared ( n -- ), per OVERFLOW-NAMES above) is
\ wrapped in a zero-arg starter that seeds it.
: TASK-OVERFLOW ( -- )
   s" a task that overflows its own data stack exits 102 (data)" T-LABEL
   s" require lib/task.f : X ( n -- ) begin dup recurse again ; : X0 ( -- ) 0 X ; TASK:MIN-STACK TASK:TASK WORKER : GO ( -- ) ['] X0 WORKER TASK:ACTIVATE begin WORKER TASK:DONE? 0= while TASK:PAUSE repeat ; GO"
   REFUSED-DATA ;

public
: RUN-GUARD ( -- )
   T-RESET
   STACK-CONTRACT-AGREEMENT
   PAGE-CAPACITY
   OVERFLOW-NAMES
   RUN-IN-STACK-REFUSALS
   TASK-OVERFLOW
   T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN-GUARD
