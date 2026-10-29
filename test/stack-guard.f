\ The guard-page contract itself: the shared refusal code, filling a guarded
\ stack to its real capacity, the three named overflow faults, one case per
\ GUARDED-EXTENT? clause behind run-in-stack's structural refusal, and a
\ task's own guarded stack.
require test/engine-stack-lifecycle-lib.f
require src/lib/memory.f

package STACK-LIFECYCLE-TEST

: STACK-CONTRACT-AGREEMENT ( -- )
   s" E-STACK-UNGUARDED matches the engine's own spelling" T-LABEL
   E-STACK-UNGUARDED STACK-ABI:E-STACK-UNGUARDED T= ;

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

\ GUARDED-EXTENT? (src/habu/habu1.f) tests in order: base non-zero, capacity
\ non-zero, base and capacity PAGE-BYTES aligned, no wrap, base outside DATA.
\ Every clause throws E-STACK-UNGUARDED, so each case changes one field of the
\ accepted extent (POOL, POOL-BYTES) and the order attributes the refusal.
: ACCEPTED-EXTENT ( -- )
   ['] EMPTY POOL POOL-BYTES run-in-stack ;

: NULL-BASE-REFUSAL ( -- )
   ['] EMPTY NULL-PTR POOL-BYTES run-in-stack ;

\ A guarded base shifted by one cell still points into read/write memory: the
\ alignment clause, not a bounds probe, catches it.
: UNALIGNED-BASE-REFUSAL ( -- )
   POOL {: base:ptr :}
   ['] EMPTY base 8 + POOL-BYTES run-in-stack ;

: CAPACITY-ZERO-REFUSAL ( -- )
   ['] EMPTY POOL 0 run-in-stack ;

\ One cell over a whole number of pages: a guarded mapping is only ever sized
\ in whole PAGE-BYTES (MEM:ALLOC-GUARDED throws E-MEM-SIZE otherwise), so a
\ capacity like this names bytes no guard page stands behind.
: PARTIAL-PAGE-CAPACITY-REFUSAL ( -- )
   ['] EMPTY POOL POOL-BYTES 8 + run-in-stack ;

\ The wrap clause: the capacity is non-zero and PAGE-BYTES aligned, so base +
\ capacity is the first to fail, landing one page below the base.
: WRAPPING-EXTENT-REFUSAL ( -- )
   ['] EMPTY POOL $FFFFFFFFFFFF0000 run-in-stack ;

\ `data-base` is PAGE-BYTES aligned by construction, so only the DATA clause
\ refuses it; a create/allot address (GBUF) is page-aligned only by accident, so
\ the base-alignment clause decides that one.
: DATA-REGION-REFUSAL ( -- )
   ['] EMPTY data-base POOL-BYTES run-in-stack ;

create GBUF 32 allot   \ an ordinary create/allot buffer: never a guarded mapping

: PLAIN-BUFFER-REFUSAL ( -- )
   ['] EMPTY GBUF POOL-BYTES run-in-stack ;

: RUN-IN-STACK-REFUSALS ( -- )
   s" the unchanged guarded extent is accepted" T-LABEL
   ['] ACCEPTED-EXTENT catch 0 T=
   s" a null base is refused" T-LABEL
   ['] NULL-BASE-REFUSAL catch E-STACK-UNGUARDED T=
   s" a guarded base shifted by 8 bytes is refused" T-LABEL
   ['] UNALIGNED-BASE-REFUSAL catch E-STACK-UNGUARDED T=
   s" capacity 0 on a real guarded pointer is refused" T-LABEL
   ['] CAPACITY-ZERO-REFUSAL catch E-STACK-UNGUARDED T=
   s" a capacity one cell past a whole page is refused" T-LABEL
   ['] PARTIAL-PAGE-CAPACITY-REFUSAL catch E-STACK-UNGUARDED T=
   s" a capacity that wraps past the base is refused" T-LABEL
   ['] WRAPPING-EXTENT-REFUSAL catch E-STACK-UNGUARDED T=
   s" a page-aligned address inside the DATA region is refused" T-LABEL
   ['] DATA-REGION-REFUSAL catch E-STACK-UNGUARDED T=
   s" a create/allot buffer is refused" T-LABEL
   ['] PLAIN-BUFFER-REFUSAL catch E-STACK-UNGUARDED T= ;

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
   STACK-CONTRACT-AGREEMENT
   PAGE-CAPACITY
   OVERFLOW-NAMES
   RUN-IN-STACK-REFUSALS
   TASK-OVERFLOW
   T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN-GUARD
