\ The guard-page contract itself: filling a guarded stack to its real
\ capacity, the three named overflow faults, and a task's own guarded stack.
require test/engine-stack-lifecycle-lib.f

package STACK-LIFECYCLE-TEST

\ Top-level literal pushes are interpreted, not checked, so the data stack
\ really holds COUNT cells. The boot data stack is one guarded page [base, base
\ + STACK-ABI:BOOT-BYTES) between inaccessible pages (src/habu/rt.f EMIT-MAP).
\ The interpreter's token loop keeps a few cells above the live depth, so a fill
\ FILL-MARGIN-CELLS short of the page succeeds and a whole-page fill faults
\ (data).
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

\ Unbounded recursion goes through RECURSE inside BEGIN...AGAIN: a word is not
\ in the dictionary inside its own definition, and a bare tail call (`1 recurse
\ ;`) is refused at 'recurse' for its residual. The legacy compiler keeps return
\ and DO frames in the guarded VM stacks. On Intel, Y's live >r value fills the
\ guarded user return stack; native DO frames live in HIR, so Z exhausts the
\ machine call stack and reports on its alternate signal stack.
: MACHINE-OVERFLOW ( ptr u8 n -- )
   CHILD-RC 128 11 + T=                          \ the dump, then death by SIGSEGV
   ERR ERRLEN @ s" habu-crash regs" T-PREFIX= ;

: OVERFLOW-NAMES ( -- )
   s" unbounded data-stack recursion overflows (data)" T-LABEL
   s" : X ( n -- ) begin dup recurse again ; 0 X" REFUSED-DATA
   s" unbounded return-stack recursion overflows (return)" T-LABEL
   s" : Y ( -- ) 1 >r recurse r> drop ; Y"
   REFUSED-RETURN
   s" unbounded DO-frame recursion overflows (loop)" T-LABEL
   s" : Z ( -- ) 1 0 do recurse loop ; Z"
   HB-TARGET-LINUX-X86-64? if MACHINE-OVERFLOW else REFUSED-LOOP then ;

\ src/lib/task.f PREPARE gives each task guarded stacks (TASK-STACK-BYTES rounds
\ up to whole guard pages). A task's overflow faults like the main thread's and
\ the crash handler's exit_group ends the process: exit 102, no per-task exit.
\ TASK:ACTIVATE needs a zero-effect quotation, so a zero-arg starter seeds the
\ ( n -- ) overflow word.
: TASK-OVERFLOW ( -- )
   s" a task that overflows its own data stack exits 102 (data)" T-LABEL
   s" require src/lib/task.f : X ( n -- ) begin dup recurse again ; : X0 ( -- ) 0 X ; TASK:MIN-STACK TASK:TASK WORKER : GO ( -- ) ['] X0 WORKER TASK:ACTIVATE begin WORKER TASK:DONE? 0= while TASK:PAUSE repeat ; GO"
   REFUSED-DATA ;

public
: RUN-GUARD ( -- )
   T-RESET
   PAGE-CAPACITY
   OVERFLOW-NAMES
   TASK-OVERFLOW
   T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN-GUARD
