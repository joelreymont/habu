\ task.f - checked CPU tasking over pthread.

s" lib/errors.f" required
s" lib/memory.f" required
s" lib/ffi-abi.f" required
require lib/le.f
s" lib/image-lifecycle.f" required
s" lib/codegen.f" required        \ +USER builds its generated accessor with CODEGEN's buffer
s" lib/adt/result.f" required     \ TASK:JOIN answers result<n,n>
\ The cleanup registry below is a checked quotation store; the optimizing tier
\ lowers such a store through QUOTATION-STORAGE:STORE, so this file owns that
\ dependency exactly as lib/image-lifecycle.f owns it for its hooks.
require src/core/quotation-storage.f
require src/habu/task-abi.f
require src/habu/stack-abi.f

package TASK
public

\ The semaphore handle callers hold. A nominal cell, so a raw address - a
\ TASK:FACILITY, say, which is the same machine shape - cannot reach TASK:WAIT.
NEWTYPE sem 0

private

\ The converters are this package's alone: a handle names a record TASK owns,
\ and no caller can mint one over memory of its own. Both directions stay
\ private, so nothing outside can project a handle back to an address either.
CAST: >SEM ( n -- sem )
CAST: SEM>N ( sem -- n )

$8 constant TASK-CELL
$10000 constant TASK-MIN-STACK
$10000 constant TASK-REGION-BYTES
$80 constant TASK-MUTEX-BYTES
0 constant TASK-FACILITY-OWNER-OFF
$8 constant TASK-FACILITY-MUTEX-OFF
TASK-FACILITY-MUTEX-OFF TASK-MUTEX-BYTES + constant TASK-FACILITY-BYTES
$20 constant TASK-SEM-BYTES              \ Linux sem_t or Darwin Mach port storage
$7FFFFFFF constant TASK-SEM-MAX          \ SEM_VALUE_MAX
$04 constant TASK-EINTR
$0B constant TASK-EAGAIN
0 constant TASK-SEM-GUARD-OFF
$8 constant TASK-SEM-OBJ-OFF
TASK-SEM-OBJ-OFF TASK-SEM-BYTES + constant TASK-SEMAPHORE-BYTES
$10 constant TASK-TIMESPEC-BYTES         \ struct timespec on an LP64 host
0 constant TASK-SPEC-SEC-OFF
$8 constant TASK-SPEC-NSEC-OFF
TASK-TIMESPEC-BYTES 2 * constant TASK-SLEEP-BYTES  \ the request and the remainder
1000000 constant TASK-NS-PER-MS
1000000000 constant TASK-NS-PER-S
$FFFFFFFFFFFFFFF8 constant TASK-CELL-MASK          \ with the 7 + below, rounds a row's offset up to a cell

TASK-ABI:EMPTY constant TASK-EMPTY
TASK-ABI:CONSTRUCTED constant TASK-CONSTRUCTED
TASK-ABI:RUNNING constant TASK-RUNNING
TASK-ABI:DONE constant TASK-DONE
TASK-ABI:HALT-REQ constant TASK-HALT-REQ
TASK-ABI:EXPOSED constant TASK-EXPOSED

TASK-ABI:TCB-BYTES constant TASK-TCB-BYTES

BEGIN-STRUCTURE TASK-TCB-SIZE
   CELL +FIELD TCB.SIZE
   CELL +FIELD TCB.XT-CELL
   CELL +FIELD TCB.THREAD
   PTR-FIELD: TCB.STACK
   CELL +FIELD TCB.STACK-U
   PTR-FIELD: TCB.REGION
   CELL +FIELD TCB.REGION-U
   CELL +FIELD TCB.DBASE
   CELL +FIELD TCB.NDICT
   CELL +FIELD TCB.CP
   CELL +FIELD TCB.STATUS
   CELL +FIELD TCB.STOP
   CELL +FIELD TCB.RET
   CELL +FIELD TCB.USER-XT-CELL
   PTR-FIELD: TCB.RSTACK
   CELL +FIELD TCB.RSTACK-U
   PTR-FIELD: TCB.LSTACK
   CELL +FIELD TCB.LSTACK-U
   CELL +FIELD TCB.THROW
   CELL +FIELD TCB.MSG
   PTR-FIELD: TCB.MSG-SENDER
   CELL +FIELD TCB.MSG-PENDING
   TASK-SEMAPHORE-BYTES +FIELD TCB.MSG-FREE
   TASK-SEMAPHORE-BYTES +FIELD TCB.MSG-FULL
   TASK-SEMAPHORE-BYTES +FIELD TCB.PARK
   CELL +FIELD TCB.RESULT
   CELL +FIELD TCB.RESULT-SET
   CELL +FIELD TCB.JOINER
   CELL +FIELD TCB.EXIT-SLOT
   TASK-SEMAPHORE-BYTES +FIELD TCB.DONE
END-STRUCTURE

: TASK-TCB-OFFSET ( ptr a ptr b n -- ) {: field:ptr origin:ptr want:n :}
   field FFI:>CELL origin FFI:>CELL - want <> if
      s" task: tcb field layout" E-TASK-STATE die then ;

: TASK-TCB-LAYOUT-CHECK ( -- )
   TASK-TCB-SIZE TASK-TCB-BYTES <> if s" task: tcb layout" E-TASK-STATE die then
   \ Check the typed field accessors of the shared descriptor: the offsets the
   \ immutable engine entry loads, and the runner's throw slot behind them.
   here CELL-VIEW {: origin:ptr :}
   origin TCB.XT-CELL origin TASK-ABI:XT-OFF TASK-TCB-OFFSET
   origin TCB.STACK origin TASK-ABI:STACK-OFF TASK-TCB-OFFSET
   origin TCB.REGION origin TASK-ABI:REGION-OFF TASK-TCB-OFFSET
   origin TCB.DBASE origin TASK-ABI:DBASE-OFF TASK-TCB-OFFSET
   origin TCB.NDICT origin TASK-ABI:NDICT-OFF TASK-TCB-OFFSET
   origin TCB.CP origin TASK-ABI:CP-OFF TASK-TCB-OFFSET
   origin TCB.STATUS origin TASK-ABI:STATUS-OFF TASK-TCB-OFFSET
   origin TCB.RSTACK origin TASK-ABI:RSTACK-OFF TASK-TCB-OFFSET
   origin TCB.LSTACK origin TASK-ABI:LSTACK-OFF TASK-TCB-OFFSET
   origin TCB.THROW origin TASK-ABI:THROW-OFF TASK-TCB-OFFSET
   origin TCB.MSG origin TASK-ABI:MSG-OFF TASK-TCB-OFFSET
   origin TCB.MSG-SENDER origin TASK-ABI:MSG-SENDER-OFF TASK-TCB-OFFSET
   origin TCB.MSG-PENDING origin TASK-ABI:MSG-PENDING-OFF TASK-TCB-OFFSET
   origin TCB.MSG-FREE origin TASK-ABI:MSG-FREE-OFF TASK-TCB-OFFSET
   origin TCB.MSG-FULL origin TASK-ABI:MSG-FULL-OFF TASK-TCB-OFFSET
   origin TCB.PARK origin TASK-ABI:PARK-OFF TASK-TCB-OFFSET
   origin TCB.RESULT origin TASK-ABI:RESULT-OFF TASK-TCB-OFFSET
   origin TCB.RESULT-SET origin TASK-ABI:RESULT-SET-OFF TASK-TCB-OFFSET
   origin TCB.JOINER origin TASK-ABI:JOINER-OFF TASK-TCB-OFFSET
   origin TCB.EXIT-SLOT origin TASK-ABI:EXIT-SLOT-OFF TASK-TCB-OFFSET
   origin TCB.DONE origin TASK-ABI:DONE-OFF TASK-TCB-OFFSET ;

TASK-TCB-LAYOUT-CHECK

create TASK-SYM-PTHREAD-CREATE
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 99 c,
   114 c, 101 c, 97 c, 116 c, 101 c, 0 c,
create TASK-SYM-PTHREAD-JOIN
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 106 c,
   111 c, 105 c, 110 c, 0 c,
create TASK-SYM-PTHREAD-EXIT
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 101 c,
   120 c, 105 c, 116 c, 0 c,
create TASK-SYM-SCHED-YIELD
   115 c, 99 c, 104 c, 101 c, 100 c, 95 c, 121 c, 105 c, 101 c,
   108 c, 100 c, 0 c,
create TASK-SYM-PTHREAD-MUTEX-INIT
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 109 c,
   117 c, 116 c, 101 c, 120 c, 95 c, 105 c, 110 c, 105 c, 116 c, 0 c,
create TASK-SYM-PTHREAD-MUTEX-LOCK
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 109 c,
   117 c, 116 c, 101 c, 120 c, 95 c, 108 c, 111 c, 99 c, 107 c, 0 c,
create TASK-SYM-PTHREAD-MUTEX-UNLOCK
   112 c, 116 c, 104 c, 114 c, 101 c, 97 c, 100 c, 95 c, 109 c,
   117 c, 116 c, 101 c, 120 c, 95 c, 117 c, 110 c, 108 c, 111 c,
   99 c, 107 c, 0 c,
create TASK-SYM-MUNMAP
   109 c, 117 c, 110 c, 109 c, 97 c, 112 c, 0 c,

variable TASK-USER-NEXT

\ Base and bound are USER-BAND's, declared in src/habu/layout.f. They used to be
\ FFI:SCRATCH-END ($41C8) and APP-ENTRY:XT-CELL ($43A0) - 472 bytes wedged
\ between FFI's scratch and the AOT capture window, of which the five shipped
\ libraries already claimed 448. USER-BAND is one declared run of the per-task
\ header with no engine cell inside it, and the layout asserts that at build
\ time, so this module states the two ends and computes neither.
USER-BAND:START constant TASK-USER-BASE
\ PAUSE is defined before +USER's generated definer. Reserve its one task-local
\ depth cell at the front of the same user band, so PAUSE can consult it.
TASK-USER-BASE constant TASK-DEFER-OFF
TASK-USER-BASE CELL + TASK-USER-NEXT !

\ The arena stops where USER-BAND does. Nothing engine-owned lies inside it -
\ src/habu/layout.f asserts that over every declared claim at build time - so
\ a refused row here means the band is full, not that a library was about to
\ overwrite the AOT window the way the old $41C8..$5000 bound allowed.
USER-BAND:END constant TASK-USER-END

: TASK-DEFER-DEPTH ( -- ptr n )
   data-base TASK-DEFER-OFF + ;

: TASK-DEFER-ENTER ( -- )
   1 TASK-DEFER-DEPTH +! ;

: TASK-DEFER-LEAVE ( -- )
   TASK-DEFER-DEPTH @ dup 0= if drop E-TASK-STATE throw then
   1- TASK-DEFER-DEPTH ! ;

: TASK-NULL ( -- ptr n )
   NULL$ drop CELL-VIEW ;

\ TCB raw-cell pointer refinement and pointer-slot reinterpretation are outside
\ checker inference. Retirement owner: habu-typed-defining-words-aa224eb5.
TRUSTED: TASK-N>PTR ( n -- ptr n ) ;

TRUSTED: TASK-CELL>PTR-SLOT ( ptr n -- ptr ptr n ) ;

\ These two cells are quotation slots in the fixed pthread entry ABI. The
\ low-level structure definer describes offsets; every callback store and load
\ below uses its declared empty stack effect.
TRUSTED: TASK-CELL>XT-SLOT ( ptr n -- ptr [ -- ] ) ;

: TCB.XT ( ptr n -- ptr [ -- ] )
   TCB.XT-CELL TASK-CELL>XT-SLOT ;

: TCB.USER-XT ( ptr n -- ptr [ -- ] )
   TCB.USER-XT-CELL TASK-CELL>XT-SLOT ;

: TASK-ALIGN8 ( -- )
   here FFI:>CELL 7 and dup 0= if drop exit then
   8 swap - allot ;

variable MUNMAP-XT
variable PTHREAD-CREATE-XT
variable PTHREAD-JOIN-XT
variable PTHREAD-EXIT-XT
variable SCHED-YIELD-XT
variable MUTEX-INIT-XT
variable MUTEX-LOCK-XT
variable MUTEX-UNLOCK-XT
variable SYMBOLS-REGISTERED
TASK-ALIGN8
variable SYMBOLS-READY

\ These symbols are borrowed from the process's libc/libSystem dependency.
\ RTLD_DEFAULT is zero on Linux and -2 on macOS; no dlopen reference is owned.
: TASK-SYM ( ptr u8 -- n ) {: name:ptr :}
   HB-TARGET-MACOS? if -2 else 0 then
   name FFI:DLSYM dup 0= if E-TASK-DLSYM throw then ;

\ Capture is quiescent. Foreign addresses belong to this process; the next
\ task operation resolves them afresh. The task entry itself is engine text.
: RESET-SYMBOLS ( -- )
   0 MUNMAP-XT ! 0 PTHREAD-CREATE-XT ! 0 PTHREAD-JOIN-XT !
   0 PTHREAD-EXIT-XT ! 0 SCHED-YIELD-XT ! 0 MUTEX-INIT-XT !
   0 MUTEX-LOCK-XT ! 0 MUTEX-UNLOCK-XT !
   0 SYMBOLS-REGISTERED ! 0 SYMBOLS-READY atomic! ;

: LOAD-SYMBOLS ( -- )
   SYMBOLS-REGISTERED @ 0= if
      [: RESET-SYMBOLS ;] IMAGE-LIFECYCLE:REGISTER
      1 SYMBOLS-REGISTERED !
   then
   TASK-SYM-MUNMAP TASK-SYM MUNMAP-XT !
   TASK-SYM-PTHREAD-CREATE TASK-SYM PTHREAD-CREATE-XT !
   TASK-SYM-PTHREAD-JOIN TASK-SYM PTHREAD-JOIN-XT !
   TASK-SYM-PTHREAD-EXIT TASK-SYM PTHREAD-EXIT-XT !
   TASK-SYM-SCHED-YIELD TASK-SYM SCHED-YIELD-XT !
   TASK-SYM-PTHREAD-MUTEX-INIT TASK-SYM MUTEX-INIT-XT !
   TASK-SYM-PTHREAD-MUTEX-LOCK TASK-SYM MUTEX-LOCK-XT !
   TASK-SYM-PTHREAD-MUTEX-UNLOCK TASK-SYM MUTEX-UNLOCK-XT ! ;

: TASK-SYMBOLS ( -- )
   begin
      SYMBOLS-READY atomic@ 2 = if exit then
      0 1 SYMBOLS-READY atomic-cas 0= if
         [: LOAD-SYMBOLS ;] catch dup 0 <> if
            0 SYMBOLS-READY atomic! throw
         then drop
         2 SYMBOLS-READY atomic! exit
      then
   again ;

\ Exact task-internal C bindings fix every pointer extent and scalar role before
\ entering the bounded FFI trampoline. Retirement owner: habu-ptx-m1-c-1df1d6e7.
TRUSTED: MUNMAP-CALL ( ptr n n -- n ) {: a:ptr len:n :}
   TASK-SYMBOLS FFI:RESET
   a 0 FFI:READABLE!
   len 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 MUNMAP-XT @ ffi-call-bounded ;

TRUSTED: PTHREAD-CREATE-CALL ( ptr n n n ptr n -- n )
   {: thread:ptr attr:n entry:n arg:ptr :}
   TASK-SYMBOLS FFI:RESET
   thread 8 0 FFI:WRITABLE!
   attr 1 FFI:VALUE!
   entry 2 FFI:VALUE!
   arg 3 FFI:READABLE!
   FFI:ARGS FFI:REG-LENS 4 PTHREAD-CREATE-XT @ ffi-call-bounded ;

TRUSTED: PTHREAD-JOIN-CALL ( n ptr n -- n ) {: thread:n out:ptr :}
   TASK-SYMBOLS FFI:RESET
   thread 0 FFI:VALUE!
   out 8 1 FFI:WRITABLE!
   FFI:ARGS FFI:REG-LENS 2 PTHREAD-JOIN-XT @ ffi-call-bounded ;

TRUSTED: PTHREAD-EXIT-CALL ( n -- ) {: value:n :}
   TASK-SYMBOLS FFI:RESET
   value 0 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 1 PTHREAD-EXIT-XT @ ffi-call-bounded drop ;

TRUSTED: SCHED-YIELD-CALL ( -- n )
   TASK-SYMBOLS FFI:RESET
   FFI:ARGS FFI:REG-LENS 0 SCHED-YIELD-XT @ ffi-call-bounded ;

TRUSTED: MUTEX-INIT-CALL ( ptr n n -- n ) {: mutex:ptr attr:n :}
   TASK-SYMBOLS FFI:RESET
   mutex TASK-MUTEX-BYTES 0 FFI:WRITABLE!
   attr 1 FFI:VALUE!
   FFI:ARGS FFI:REG-LENS 2 MUTEX-INIT-XT @ ffi-call-bounded ;

TRUSTED: MUTEX-LOCK-CALL ( ptr n -- n ) {: mutex:ptr :}
   TASK-SYMBOLS FFI:RESET
   mutex TASK-MUTEX-BYTES 0 FFI:WRITABLE!
   FFI:ARGS FFI:REG-LENS 1 MUTEX-LOCK-XT @ ffi-call-bounded ;

TRUSTED: MUTEX-UNLOCK-CALL ( ptr n -- n ) {: mutex:ptr :}
   TASK-SYMBOLS FFI:RESET
   mutex TASK-MUTEX-BYTES 0 FFI:WRITABLE!
   FFI:ARGS FFI:REG-LENS 1 MUTEX-UNLOCK-XT @ ffi-call-bounded ;

\ ---- host semaphore bindings ----------------------------------------
\ Declared rather than hand-staged: these four arrived after the FUNCTION:
\ declarer, so package FFI owns their symbol resolution and their bounded
\ staging and this module states only the C prototypes. The single pointer
\ argument is always one sem_t the semaphore records below own, so every
\ declaration fixes its extent at TASK-SEM-BYTES; the callee writes it.
\
\ sem_wait blocks inside the call. That is the point of this module - a waiting
\ task needs no PAUSE loop - and it is safe because the argument tables the
\ generated body stages sit in the calling task's own DATA region, so a blocked
\ waiter holds nothing the signaller needs.
PROCESS-SYMBOLS

FUNCTION: SEM-INIT-CALL sem_init ( ptr u8 n n -- i32 )
   0 TASK-SEM-BYTES WRITES-BYTES          \ sem_t; then pshared, then the count
;FUNCTION

FUNCTION: SEM-WAIT-CALL sem_wait ( ptr u8 -- i32 )
   0 TASK-SEM-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: SEM-POST-CALL sem_post ( ptr u8 -- i32 )
   0 TASK-SEM-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: SEM-DESTROY-CALL sem_destroy ( ptr u8 -- i32 )
   0 TASK-SEM-BYTES WRITES-BYTES
;FUNCTION

\ sem_trywait is the same decrement without the block: EAGAIN is the answer
\ "would have blocked", not a failure, and it is what the queue's TRY-PUSH and
\ TRY-POP refuse on.
FUNCTION: SEM-TRYWAIT-CALL sem_trywait ( ptr u8 -- i32 )
   0 TASK-SEM-BYTES WRITES-BYTES
;FUNCTION

\ Darwin's unnamed POSIX semaphore entry points are ENOSYS stubs. A Mach
\ semaphore is a process-owned port name stored in the same record. Its wait
\ result is a kernel return code, never errno; a zero timespec tries once.
14 constant MACH-ABORTED
49 constant MACH-TIMED-OUT
0 constant MACH-FIFO
FUNCTION: MACH-TASK task_self_trap ( -- u32 ) ;FUNCTION
FUNCTION: MACH-SEM-CREATE semaphore_create ( n ptr u8 n n -- i32 )
   1 4 WRITES-BYTES
;FUNCTION
FUNCTION: MACH-SEM-DESTROY semaphore_destroy ( n n -- i32 ) ;FUNCTION
FUNCTION: MACH-SEM-WAIT semaphore_wait ( n -- i32 ) ;FUNCTION
FUNCTION: MACH-SEM-SIGNAL semaphore_signal ( n -- i32 ) ;FUNCTION
FUNCTION: MACH-SEM-TRY semaphore_timedwait ( n n -- i32 ) ;FUNCTION

\ nanosleep is the other call here that blocks on purpose: it parks the calling
\ thread until a time arrives rather than until something happens. The first
\ timespec is the request, which the kernel only reads; the second is the one it
\ writes when a signal cuts the sleep short, so the extent is stated on that
\ argument alone and the request stays a read-only pointer.
FUNCTION: NANOSLEEP-CALL nanosleep ( ptr u8 ptr u8 -- i32 )
   1 TASK-TIMESPEC-BYTES WRITES-BYTES
;FUNCTION

: TASK-RC0 ( n -- )
   dup 0 <> if E-TASK-THREAD throw then
   drop ;

: TASK-CHECK-SIZE ( n -- )
   dup TASK-MIN-STACK < if E-TASK-SIZE throw then
   drop ;

\ The status cell is read across threads - DONE?, ACTIVATE's refusals, TASK-KILL
\ and TASK-JOIN-CHECK all read it from the owner while the worker is still
\ moving it - so the read is an acquire load. The ending task publishes DONE
\ with the matching release store (the entry's STLR, src/habu/habu1.f
\ BTASK-ENTRY, and PAUSE's halted exit below), which is what makes an owner that
\ only polls DONE? see every write the body made before it ended.
: TASK-STATE@ ( ptr n -- n )
   TCB.STATUS atomic@ ;

\ The owner's store, and only the owner's: PREPARE and ACTIVATE write it before
\ pthread_create, which orders it into the new thread, and TASK-JOIN-RELEASE and
\ TASK-KILL write it after the join or on a task that has no thread at all. A
\ worker never reaches here - it publishes its own DONE with a release store.
: TASK-STATE! ( n ptr n -- )
   TCB.STATUS ! ;

: TASK-THROW@ ( ptr n -- n )
   TCB.THROW @ ;

: TASK-THROW! ( n ptr n -- )
   TCB.THROW ! ;

: TASK-SELF ( -- ptr n )
   data-base TASK-TCB-CELL + @ TASK-N>PTR ;

: TASK-SELF-N ( -- n )
   data-base TASK-TCB-CELL + @ ;

\ The main region, whichever task asks: DATA is mapped MAP_FIXED at DATA-VA. An
\ ordinary call, so every use computes the address at run time.
: MAIN-BASE ( -- ptr n )
   DATA-VA VA>N TASK-N>PTR ;

\ The handle crosses back through n and the module's raw-cell refinement.
: SEM-REC ( sem -- ptr n )
   SEM>N TASK-N>PTR ;

\ The handle of a semaphore record this package owns but did not define: a
\ task's mailbox holds two in its TCB and the pool below holds a fixed row.
: SEM-AT ( ptr n -- sem )
   FFI:>CELL >SEM ;

: SEM-GUARD ( sem -- ptr n )
   SEM-REC TASK-SEM-GUARD-OFF + ;

\ The sem_t itself, as the byte span the declarations above name written.
: SEM-OBJ ( sem -- ptr u8 )
   SEM-REC BYTE-VIEW TASK-SEM-OBJ-OFF + ;

\ Defence in depth under the type: the guard cell holds the record's OWN
\ address exactly while the POSIX object behind it is live, so an uninitialized
\ record and a destroyed one fail closed instead of entering sem_wait.
: SEM-LIVE? ( sem -- bool ) {: s:sem :}
   s SEM-GUARD atomic@ s SEM>N = ;

: SEM-CHECK ( sem -- )
   SEM-LIVE? 0= if E-TASK-SEM-STATE throw then ;

: SEM-COUNT-CHECK ( n -- ) {: value:n :}
   value 0 < value TASK-SEM-MAX > or if E-TASK-SEM-COUNT throw then ;

: SEM-INIT ( n sem -- ) {: value:n s:sem :}
   value SEM-COUNT-CHECK
   s SEM-LIVE? if E-TASK-SEM-STATE throw then
   HB-TARGET-MACOS? if
      MACH-TASK s SEM-OBJ MACH-FIFO value MACH-SEM-CREATE TASK-RC0
   else
      s SEM-OBJ 0 value SEM-INIT-CALL TASK-RC0
   then
   s SEM>N s SEM-GUARD atomic! ;

\ POSIX leaves destroying a semaphore that still has blocked waiters undefined,
\ so the caller drains its waiters first. Destroying an inactive one is a no-op.
: SEM-DESTROY ( sem -- ) {: s:sem :}
   s SEM-LIVE? 0= if exit then
   0 s SEM-GUARD atomic!
   HB-TARGET-MACOS? if
      MACH-TASK s SEM-OBJ LE:U32@ MACH-SEM-DESTROY TASK-RC0
      0 s SEM-OBJ LE:U32!
   else s SEM-OBJ SEM-DESTROY-CALL TASK-RC0 then ;

\ Blocks inside the host call, so a waiting task needs no PAUSE loop - and
\ observes no TASK:HALT - until it is signalled. A signal interrupts the wait
\ without consuming a count, so EINTR retries.
: MACH-WAIT ( sem -- ) {: s:sem :}
   begin
      s SEM-CHECK
      s SEM-OBJ LE:U32@ MACH-SEM-WAIT
      dup 0= if drop exit then
      MACH-ABORTED <> if E-TASK-THREAD throw then
   again ;

: SEM-WAIT ( sem -- ) {: s:sem :}
   HB-TARGET-MACOS? if s MACH-WAIT exit then
   begin
      s SEM-CHECK
      s SEM-OBJ SEM-WAIT-CALL 0= if exit then
      FFI:ERRNO TASK-EINTR <> if E-TASK-THREAD throw then
   again ;

: SEM-SIGNAL ( sem -- ) {: s:sem :}
   s SEM-CHECK
   HB-TARGET-MACOS? if s SEM-OBJ LE:U32@ MACH-SEM-SIGNAL
   else s SEM-OBJ SEM-POST-CALL then TASK-RC0 ;

\ The decrement that never blocks: true when it took a count, false when the
\ count was zero. EAGAIN is that answer, EINTR retries as SEM-WAIT does.
: MACH-TRY-WAIT ( sem -- bool ) {: s:sem :}
   begin
      s SEM-CHECK
      s SEM-OBJ LE:U32@ 0 MACH-SEM-TRY
      dup 0= if drop true exit then
      dup MACH-TIMED-OUT = if drop false exit then
      MACH-ABORTED <> if E-TASK-THREAD throw then
   again ;

: SEM-TRY-WAIT ( sem -- bool ) {: s:sem :}
   HB-TARGET-MACOS? if s MACH-TRY-WAIT exit then
   begin
      s SEM-CHECK
      s SEM-OBJ SEM-TRYWAIT-CALL 0= if 0 0= exit then
      FFI:ERRNO TASK-EAGAIN = if 0 0= 0= exit then
      FFI:ERRNO TASK-EINTR <> if E-TASK-THREAD throw then
   again ;

\ ---- the semaphore pool ------------------------------------------------------
\ A semaphore a caller asks for at run time instead of defining one. The records
\ are this package's own static storage, so a handed-out handle still names a
\ record TASK owns and no other package needs the extent of one to hold a
\ semaphore of its own.
$40 constant TASK-SEM-POOL-N

: TASK-ZERO-CELLS, ( n -- )
   0 ?do 0 , loop ;

TASK-ALIGN8
create TASK-SEM-USED TASK-SEM-POOL-N TASK-ZERO-CELLS,
TASK-ALIGN8
create TASK-SEM-POOL
   TASK-SEM-POOL-N TASK-SEMAPHORE-BYTES * 8 / TASK-ZERO-CELLS,

: SEM-POOL-REC ( n -- ptr n ) {: idx:n :}
   TASK-SEM-POOL CELL-VIEW idx TASK-SEMAPHORE-BYTES * + ;

: SEM-POOL-USED ( n -- ptr n ) {: idx:n :}
   TASK-SEM-USED CELL-VIEW idx cells + ;

\ One claim wins: the used cell moves 0 -> 1 in one step, so two tasks asking at
\ the same moment are handed different records.
: SEM-POOL-CLAIM? ( n -- bool ) {: idx:n :}
   0 1 idx SEM-POOL-USED atomic-cas 0= ;

: SEM-POOL-RELEASE ( n -- ) {: idx:n :}
   0 idx SEM-POOL-USED atomic! ;

: SEM-POOL-FREE-INDEX ( -- n )
   TASK-SEM-POOL-N 0 ?do
      i SEM-POOL-CLAIM? if i unloop exit then
   loop
   -1 ;

: SEM-NEW ( -- sem )
   SEM-POOL-FREE-INDEX dup 0 < if E-TASK-SEM-POOL throw then
   SEM-POOL-REC SEM-AT ;

\ The index of a pooled record, or -1 for a handle from anywhere else: a defined
\ semaphore and a task's mailbox are not the pool's to hand back out.
: SEM-POOL-INDEX ( sem -- n ) {: s:sem :}
   s SEM>N TASK-SEM-POOL CELL-VIEW FFI:>CELL - {: off:n :}
   off 0 < if -1 exit then
   off TASK-SEM-POOL-N TASK-SEMAPHORE-BYTES * >= if -1 exit then
   off TASK-SEMAPHORE-BYTES mod 0 <> if -1 exit then
   off TASK-SEMAPHORE-BYTES / ;

\ Destroys the semaphore if it is still live, so a recycled record never carries
\ a POSIX object into its next owner; the caller has already ended its waiters.
: SEM-FREE ( sem -- ) {: s:sem :}
   s SEM-POOL-INDEX dup 0 < if drop E-TASK-SEM-POOL throw then
   s SEM-DESTROY
   SEM-POOL-RELEASE ;

\ ---- the task's own wake-up --------------------------------------------------
\ TASK:STOP parks the calling task on the semaphore in its own TCB and TASK:WAKE
\ posts that semaphore. WAKE is a HINT: the count makes a WAKE that arrives
\ before the STOP one the STOP does not wait for, and a STOP takes one hint
\ whoever posted it, so the caller re-checks its own state after every STOP
\ instead of believing the wake-up meant what it hoped. That is what lets one
\ loop complete work for many tasks - it wakes an owner by its TCB and holds no
\ semaphore per waiter, which the pool could not do past TASK-SEM-POOL-N.
: TASK-PARK-SEM ( ptr n -- sem )
   TCB.PARK SEM-AT ;

\ THE PARK LIVES AS LONG AS ITS TCB DOES IN THIS PROCESS, unlike the mailbox
\ and the done semaphore, which a release destroys: the owner ends their users
\ before it releases, but nothing orders a post of the park before the owner's
\ next KILL, JOIN or ACTIVATE, so the park outlives every release. TASK-WAKE
\ posts it after reading RUNNING, HALT-REQ or EXPOSED, and HALT after its CAS,
\ both from any thread. PREPARE creates the park once and no lifecycle word
\ destroys or recreates it, so every admitted post meets this one live
\ semaphore and the lifecycle has no posts in flight to count or wait for.
\ ACTIVATE and EXPOSE drain it, so each run and exposure opens at zero.
\ Only the capture sweep destroys it, where no waker can hold the TCB, as
\ MAIN-PARK-RESET does for the main record; the next PREPARE, here or in the
\ restored image, creates it again.
: PARK-CREATE ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-PARK-SEM SEM-LIVE? if exit then
   0 tcb TASK-PARK-SEM SEM-INIT ;

: PARK-DESTROY ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-PARK-SEM SEM-DESTROY ;

\ Takes every count the park holds, so a run opens at zero while the record
\ stays live under any post still in flight.
: PARK-DRAIN ( ptr n -- ) {: tcb:ptr :}
   begin tcb TASK-PARK-SEM SEM-TRY-WAIT while repeat ;

\ The main thread has no TCB - TASK:SELF answers the null TCB there - so its
\ park is this one record and a WAKE of the null TCB posts it, which is what
\ lets a program with no tasks of its own wait on a loop too. It is initialized
\ on first use. The handshake is TASK-SYMBOLS', so two tasks waking the main
\ thread at the same
\ moment initialize the record once.
TASK-ALIGN8
create MAIN-PARK-REC TASK-SEMAPHORE-BYTES 8 / TASK-ZERO-CELLS,
TASK-ALIGN8
variable MAIN-PARK-READY

: MAIN-PARK ( -- sem )
   MAIN-PARK-REC CELL-VIEW SEM-AT ;

\ This semaphore belongs to the running process, including Darwin's Mach port.
\ Capturing a warm main task must leave the next process an uninitialised park.
: MAIN-PARK-RESET ( -- )
   MAIN-PARK SEM-DESTROY
   0 MAIN-PARK-READY atomic! ;

: MAIN-PARK-CREATE ( -- )
   0 MAIN-PARK SEM-INIT
   [: MAIN-PARK-RESET ;] IMAGE-LIFECYCLE:REGISTER ;

: MAIN-PARK-INIT ( -- )
   begin
      MAIN-PARK-READY atomic@ 2 = if exit then
      0 1 MAIN-PARK-READY atomic-cas 0= if
         [: MAIN-PARK-CREATE ;] catch dup 0 <> if
            MAIN-PARK-RESET throw
         then drop
         2 MAIN-PARK-READY atomic! exit
      then
   again ;

\ The calling thread's park: the record in its own TCB, or the main one.
: PARK-SELF ( -- sem )
   TASK-SELF-N dup 0= if
      drop MAIN-PARK-INIT MAIN-PARK exit
   then
   TASK-N>PTR TASK-PARK-SEM ;

\ Blocks inside the host call like every other wait here, so a stopped task
\ burns no CPU and observes no TASK:HALT until something wakes it - which is why
\ TASK:HALT wakes its target itself.
: TASK-STOP ( -- )
   PARK-SELF SEM-WAIT ;

\ A task that was never activated, one that is only constructed and one that has
\ ended have no run to hint at: the count would sit in a record its next
\ activation is not entitled to, so all three are refused. An exposed task is
\ admitted: TASK-SELF of a thread inside its context is that task, so a body
\ there that stops parks on this record. The task can end, be released and
\ open again between the state read and the post; the post still meets this
\ TCB's one live park (PARK-CREATE). A hint that arrives before the next run
\ or exposure opens is drained there; a WAKE still in flight as it opens lands
\ in it, the one stale hint its re-check absorbs.
: TASK-WAKE ( ptr n -- ) {: tcb:ptr :}
   tcb FFI:>CELL 0= if MAIN-PARK-INIT MAIN-PARK SEM-SIGNAL exit then
   tcb TASK-STATE@ {: st:n :}
   st TASK-RUNNING <> st TASK-HALT-REQ <> and st TASK-EXPOSED <> and
      if E-TASK-STATE throw then
   tcb TASK-PARK-SEM SEM-SIGNAL ;

\ ---- the per-task mailbox ----------------------------------------------------
\ VFX's one-cell mailbox (docs/tasking-models.md section 3) with the semaphores
\ above in place of its PAUSE loop: the slot-free semaphore holds the single
\ unread slot, the message-present semaphore holds the deposited message, and
\ the pending cell is the status bit MSG? reads without blocking.
: MBOX-FREE ( ptr n -- sem )
   TCB.MSG-FREE SEM-AT ;

: MBOX-FULL ( ptr n -- sem )
   TCB.MSG-FULL SEM-AT ;

: MBOX-INIT ( ptr n -- ) {: tcb:ptr :}
   0 tcb TCB.MSG !
   TASK-NULL tcb TCB.MSG-SENDER !
   0 tcb TCB.MSG-PENDING atomic!
   1 tcb MBOX-FREE SEM-INIT
   0 tcb MBOX-FULL SEM-INIT ;

\ Paired with the task's memory, so a task that ends drops its mailbox with its
\ stacks. POSIX leaves destroying a semaphore with blocked waiters undefined, so
\ the owner ends a task's senders before it ends the task.
: MBOX-DESTROY ( ptr n -- ) {: tcb:ptr :}
   0 tcb TCB.MSG-PENDING atomic!
   tcb MBOX-FREE SEM-DESTROY
   tcb MBOX-FULL SEM-DESTROY ;

\ A message carries its sender, so both ends of a send are tasks: the main
\ thread has no TCB and so no mailbox of its own. A send to a task that is not
\ running has nobody to read it, and a send to the sending task could only wait
\ for a get that task is not making; both are refused rather than deadlocked.
: MBOX-SEND-CHECK ( ptr n -- ) {: tcb:ptr :}
   TASK-SELF-N 0= if E-TASK-MAILBOX throw then
   tcb FFI:>CELL TASK-SELF-N = if E-TASK-MAILBOX throw then
   tcb TASK-STATE@ TASK-RUNNING <> if E-TASK-MAILBOX throw then ;

: MBOX-SEND ( n ptr n -- ) {: msg:n tcb:ptr :}
   tcb MBOX-SEND-CHECK
   tcb MBOX-FREE SEM-WAIT
   msg tcb TCB.MSG !
   TASK-SELF tcb TCB.MSG-SENDER !
   1 tcb TCB.MSG-PENDING atomic!
   tcb MBOX-FULL SEM-SIGNAL ;

: MBOX-GET ( -- n ptr n )
   TASK-SELF-N 0= if E-TASK-MAILBOX throw then
   TASK-SELF {: self:ptr :}
   self MBOX-FULL SEM-WAIT
   self TCB.MSG @ self TCB.MSG-SENDER @
   0 self TCB.MSG-PENDING atomic!
   self MBOX-FREE SEM-SIGNAL ;

\ A snapshot of the status bit: true from the moment a send deposits a message
\ until the get that takes it clears it. It never blocks and never throws, so a
\ task that was never activated simply holds no message.
: MBOX-MSG? ( ptr n -- bool )
   TCB.MSG-PENDING atomic@ 0 <> ;

\ ---- the task's outcome ------------------------------------------------------
\ VFX's task exit code and its AtTaskExit cleanup (docs/tasking-models.md
\ sections 3 and 5), answered as lib/adt/result.f's result<ok,err> rather than as
\ a bare code. The rows below live in the TCB, which outlives the thread and its
\ memory, so the answer survives the release the join performs - the same reason
\ TASK:THROW@ still answers after a TASK:KILL.

\ The cleanups a task has registered, as a chain of rows in this package's own
\ storage. Each quotation is stored AS a quotation into storage declared to hold
\ one, which is what makes the store the checker's proven-quotation store instead
\ of a cell this module would have to cast back to code. TASK-EXIT-LINK holds,
\ for each row, the next row of the same task's chain plus one; zero ends the
\ chain. TCB.EXIT-SLOT holds the HEAD row plus one; zero is "this task has no
\ cleanup". A row belongs to one (task, quotation) pair for the life of the
\ image - a registration serves the task's later activations and the same
\ quotation is never given a second row - so the rows cannot leak and need no
\ free list.
$80 constant TASK-EXIT-MAX
TASK-EXIT-MAX TYPED-BUFFER TASK-EXIT-QT [ -- ]
TASK-ALIGN8
create TASK-EXIT-LINK TASK-EXIT-MAX TASK-ZERO-CELLS,
TASK-ALIGN8
variable TASK-EXIT-N
TASK-ALIGN8
variable TASK-EXIT-LOCK

\ The one cell a registration compares against. The checker refuses `=` on two
\ quotations (E-MISMATCH, "expected n n actual [ -- ] [ -- ]"), while a quotation
\ in declared storage reads as a cell through the views, and the same `['] W`
\ stored twice reads the same cell. The incoming quotation goes here to be read
\ as that cell, so this scratch - not the chain - is what registration has to
\ take a lock for.
TYPED-VARIABLE TASK-EXIT-SCRATCH [ -- ]

: TASK-DONE-SEM ( ptr n -- sem )
   TCB.DONE SEM-AT ;

\ Created with the task and destroyed with its memory, exactly like the mailbox:
\ a task that was never activated has nothing to signal, and a released task's
\ record is gone before the next PREPARE opens a fresh one.
: DONE-INIT ( ptr n -- ) {: tcb:ptr :}
   0 tcb TASK-DONE-SEM SEM-INIT ;

: DONE-DESTROY ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-DONE-SEM SEM-DESTROY ;

: TASK-RESULT? ( ptr n -- bool )
   TCB.RESULT-SET atomic@ 0 <> ;

\ The answer is single-assignment: the flag moves 0 -> 1 in one step and the
\ first call wins, so neither a second call nor the task's own cleanup can
\ replace a result the body already gave. A thread with no TCB has no task to
\ answer for and is refused rather than writing into somebody's storage.
: TASK-RETURN ( n -- ) {: value:n :}
   TASK-SELF-N 0= if E-TASK-STATE throw then
   TASK-SELF {: self:ptr :}
   0 1 self TCB.RESULT-SET atomic-cas 0 <> if E-TASK-STATE throw then
   value self TCB.RESULT ! ;

\ Cleared by ACTIVATE, so a reactivated task answers for its new run alone.
: TASK-OUTCOME-RESET ( ptr n -- ) {: tcb:ptr :}
   0 tcb TCB.RESULT !
   0 tcb TCB.RESULT-SET atomic!
   0 tcb TCB.JOINER atomic! ;

: TASK-EXIT-SLOT@ ( ptr n -- n )
   TCB.EXIT-SLOT @ ;

: TASK-EXIT-LINK-AT ( n -- ptr n ) {: idx:n :}
   TASK-EXIT-LINK CELL-VIEW idx cells + ;

: TASK-EXIT-QT-CELL ( n -- n )
   TASK-EXIT-QT BYTE-VIEW CELL-VIEW @ ;

\ Registration only, and only for the scratch cell above: TASK-RUN-EXIT reads
\ the chain without it.
: TASK-EXIT-LOCK-GET ( -- )
   begin 0 1 TASK-EXIT-LOCK atomic-cas 0= until ;

: TASK-EXIT-UNLOCK ( -- )
   0 TASK-EXIT-LOCK atomic! ;

\ A row no chain holds yet. The counter moves in one atomic step, and the caller
\ holds the registration lock, so the row it is handed is its own; past the end
\ of the storage there is no row to hand out. The lock is released before that
\ refusal leaves, so a full table costs the registration and not every later one.
: TASK-EXIT-NEXT ( -- n )
   1 TASK-EXIT-N atomic-add {: idx:n :}
   idx TASK-EXIT-MAX >= if TASK-EXIT-UNLOCK E-TASK-EXIT-TABLE throw then
   idx ;

\ True when the task's chain already holds the quotation in the scratch cell.
: TASK-EXIT-HELD? ( ptr n -- bool ) {: tcb:ptr :}
   TASK-EXIT-SCRATCH BYTE-VIEW CELL-VIEW @ {: want:n :}
   tcb TASK-EXIT-SLOT@
   begin dup 0 <> while
      1 -
      dup TASK-EXIT-QT-CELL want = if drop true exit then
      TASK-EXIT-LINK-AT @
   repeat
   drop false ;

\ Registration is additive: the task keeps every cleanup it registers and the
\ chain runs newest first. A quotation the task already holds is not registered
\ twice - AIO registers its scrub at each activation's first submission, so a
\ second row per activation would exhaust the storage. The quotation is in its
\ row, and that row's link names the old head, before the TCB names the row, so a
\ task ending while a thread registers on it runs the whole old chain or the
\ whole new one, never a half-built row. The head is published with atomic! -
\ a store-release (STLR), src/habu/habu1.f BATSTORE - which is what makes that
\ true on a machine whose plain stores to different addresses may become visible
\ out of order: every store of the row and its link is visible before the head
\ that names them. TASK-RUN-EXIT needs no fence in return, because its loads of
\ the row and the link are addressed FROM the head it read. The lock covers the
\ scratch cell the comparison goes through, so two threads registering on one
\ task at the same moment - AIO from the task itself, the program from the
\ thread that started it - both land.
: TASK-AT-EXIT ( [ -- ] ptr n -- ) {: q tcb:ptr :}
   TASK-EXIT-LOCK-GET
   q TASK-EXIT-SCRATCH !
   tcb TASK-EXIT-HELD? if TASK-EXIT-UNLOCK exit then
   TASK-EXIT-NEXT {: idx:n :}
   q idx TASK-EXIT-QT !
   tcb TASK-EXIT-SLOT@ idx TASK-EXIT-LINK-AT !
   idx 1 + tcb TCB.EXIT-SLOT atomic!
   TASK-EXIT-UNLOCK ;

\ A cleanup that throws ends nothing else: its code becomes the task's error when
\ the body left none, and is dropped when the body already failed or an earlier
\ cleanup already threw, so the FIRST failure is the one the join reports.
: TASK-RUN-EXIT-ONE ( n -- )
   TASK-EXIT-QT @ catch {: rc:n :}
   rc 0= if exit then
   TASK-SELF {: self:ptr :}
   self TASK-THROW@ 0 <> if exit then
   rc self TASK-THROW! ;

\ The cleanups run in the task's own thread, after the body has returned or its
\ throw has been recorded, newest registration first. Every one of them runs,
\ whatever the ones before it did.
: TASK-RUN-EXIT ( -- )
   TASK-SELF TASK-EXIT-SLOT@
   begin dup 0 <> while
      1 -
      dup TASK-RUN-EXIT-ONE
      TASK-EXIT-LINK-AT @
   repeat
   drop ;

\ ---- callback bindings: one row per slot (docs/ffi-callback.md) --------------
\ A row names the region a C call through its slot enters (CB-ROW-REGION, 0
\ unbound) and the thread inside through it (CB-ROW-OWNER: 0 idle, a thread
\ pointer, or CB-ROW-BUSY while a bind, unbind, claim or drop moves the row).
\ The engine's thunk claims the row before it reads the region, so a region
\ stays mapped while a row names it: UNEXPOSE and TASK-END, the only words that
\ release a named region, drop its rows first. The table is published into the
\ main region's CB-ROWS, where every thread's thunk reads it.
\ A call that meets BUSY waits in the thunk until the row is handed back, so a
\ mover holds BUSY only for a few steps that call nothing, wait on nothing and
\ throw nothing: every word below hands its rows back before it throws.
TASK-ALIGN8
create ROWS CB-POOL CB-ROW-BYTES * TASK-CELL / TASK-ZERO-CELLS,

\ Each row's count of region stores, kept beside the table: only the movers
\ below write it and the thunk never reads it, so the row and the thunk stay as
\ they are. ROW-INSIDE? says what it settles.
TASK-ALIGN8
create MOVES CB-POOL TASK-ZERO-CELLS,

: ROW ( n -- ptr n )
   CB-ROW-BYTES * ROWS CELL-VIEW + ;

: ROW-REGION ( ptr n -- ptr n )
   CB-ROW-REGION + ;

: ROW-OWNER ( ptr n -- ptr n )
   CB-ROW-OWNER + ;

: ROW-MOVES ( n -- ptr n )
   cells MOVES CELL-VIEW + ;

\ Names region r in row k, then counts the store, each a release store. Every
\ region store goes through here, by a mover that holds the row BUSY and hands
\ it back only after both - the capture sweep aside, which runs alone.
: ROW-NAME! ( n n -- ) {: r:n k:n :}
   r k ROW ROW-REGION atomic!
   k ROW-MOVES atomic@ 1 + k ROW-MOVES atomic! ;

: ROW-NAMES? ( n n -- bool ) {: k:n reg:n :}
   k ROW ROW-REGION atomic@ reg = ;

: ROW-SLOT? ( n n -- bool ) {: mask:n k:n :}
   mask 1 k lshift and 0 <> ;

\ Hands the rows of a mask back, BUSY -> 0.
: ROWS-RELEASE ( n -- ) {: mask:n :}
   CB-POOL 0 ?do
      mask i ROW-SLOT? if 0 i ROW ROW-OWNER atomic! then
   loop ;

\ Moves every row naming the region 0 -> BUSY and answers them as a mask and
\ true. A row that is not idle - a thread inside, or a bind, unbind or drop
\ moving it - refuses the whole claim: the rows already moved go back and the
\ answer is 0 and false. A row that moved to another region between the read
\ and the claim is handed back: the region is read again under BUSY, where
\ nothing else can move it, so the claim holds no row that does not name it.
: ROWS-CLAIM ( n -- n bool ) {: reg:n :}
   0 CB-POOL 0 ?do
      i reg ROW-NAMES? if
         0 CB-ROW-BUSY i ROW ROW-OWNER atomic-cas 0 <> if
            ROWS-RELEASE 0 false unloop exit
         then
         i reg ROW-NAMES? if
            1 i lshift or
         else
            0 i ROW ROW-OWNER atomic!
         then
      then
   loop
   true ;

\ The region and its count first, then the owner, each a release store: a thunk
\ that claims the idle row afterward reads 0 and refuses.
: ROWS-DROP ( n -- ) {: mask:n :}
   CB-POOL 0 ?do
      mask i ROW-SLOT? if
         0 i ROW-NAME!
         0 i ROW ROW-OWNER atomic!
      then
   loop ;

\ Whether the thread a refused claim saw is inside row k by region reg. A
\ thread value comes only from that thread's own claim, and the thread entered
\ by the region the row named then, which nothing moves until it leaves: bind,
\ unbind, a claim and another task's end all take the row from 0, a drop moves
\ only rows its caller holds, and the thunk only reads the region. The count
\ is read before the region and again after the owner, each an acquire load.
\ Every region store is counted before its mover hands the row back, and a
\ claim acquires that hand-back, so an unchanged count leaves no store between
\ the one the thread entered by and the one read here: they are the same. The
\ row alone cannot tell: a thread that left and entered again by another
\ region between the two reads, past a bind that named this region while it
\ held the row BUSY, reads as inside it. A thread entering again by the same
\ region stores nothing, so the thunk takes no part.
: ROW-INSIDE? ( n n n -- bool ) {: k:n reg:n seen:n :}
   k ROW-MOVES atomic@ {: before:n :}
   k reg ROW-NAMES?
   k ROW ROW-OWNER atomic@ seen = and
   k ROW-MOVES atomic@ before = and ;

\ A task's rows die with it, before its one DONE signal, so a DONE region is
\ never named and no release path has to refuse. A thread inside a row naming
\ the region is a contract breach the process ends on: a foreign thread that
\ entered while this task ran is already dying 106 in the thunk (the context is
\ busy, or outside any foreign call) and never releases the row. The task's
\ own thread is never inside a callback here - its body returns only after C
\ has, and PAUSE does not end a task while its region holds a callback frame -
\ so for that thread the die is a backstop: an ending beneath C's frames would
\ leave C's call undrained.
\ A refused claim is not that breach until ROW-INSIDE? finds the thread it saw
\ inside by this region: the row may have moved since the first read, and the
\ thread may have left it and entered again by another. A BUSY row is held by
\ a mover for a few steps that wait on nothing, so the task yields and reads it
\ again. A row that no longer names the region is skipped, and one that moved
\ before a claim that succeeds is handed back as ROWS-CLAIM hands it.
: ROW-END ( n n -- ) {: k:n reg:n :}
   begin k reg ROW-NAMES? while
      0 CB-ROW-BUSY k ROW ROW-OWNER atomic-cas {: seen:n :}
      seen 0 = if
         k reg ROW-NAMES? if 0 k ROW-NAME! then
         0 k ROW ROW-OWNER atomic!
         exit
      then
      seen CB-ROW-BUSY = if
         SCHED-YIELD-CALL TASK-RC0
      else
         k reg seen ROW-INSIDE? if
            s" task: a callback slot naming the ending task is in flight"
            ENGINE-ERROR:CALLBACK die
         then
      then
   repeat ;

: ROWS-END ( n -- ) {: reg:n :}
   CB-POOL 0 ?do i reg ROW-END loop ;

\ Every way a task's thread ends passes here: the body returning, the body
\ throwing, and a halted body leaving at TASK:PAUSE. The stop mark goes first,
\ so a cleanup that pauses yields rather than entering the halted exit
\ (HALT-PENDING?). The rows go before the signal, which is last, so a joiner
\ that wakes finds the cleanup finished, the outcome rows final and no slot
\ naming the region it is about to release.
: TASK-END ( -- )
   1 TASK-SELF TCB.STOP !
   TASK-RUN-EXIT
   data-base FFI:>CELL ROWS-END
   TASK-SELF TASK-DONE-SEM SEM-SIGNAL ;

\ One shared cell counted from several threads: an owner releasing one task can
\ run while another thread activates a second, so each side moves the count in
\ ONE atomic step. No clamp below zero - every release follows an activation, so
\ a count below zero is a pairing defect, and a nonzero count already refuses
\ dictionary mutation, which is where it must surface (exit $4F) rather than in
\ a clamp that hides it. The count is the main region's, where the compiler
\ reads it (habu1.f B-TASK-LIVE-GUARD), so a worker activating or exposing a
\ task counts there too.
: TASK-LIVE+ ( -- )
   1 MAIN-BASE TASKS-LIVE-CELL + atomic-add drop ;

: TASK-LIVE- ( -- )
   -1 MAIN-BASE TASKS-LIVE-CELL + atomic-add drop ;

: TASK-MUNMAP-SPAN ( ptr n n -- )
   MUNMAP-CALL TASK-RC0 ;

\ AN EMPTY TCB HOLDS NOTHING THIS PROCESS TOOK BUT ITS PARK, which outlives
\ every release until the capture sweep (PARK-CREATE). The mailbox and the done
\ semaphore are destroyed, the four mappings go back to the system, and the
\ five cells cleared last are what the run itself acquired: the pthread_t
\ pthread_create stored, the value the join wrote, and PREPARE's record
\ of this process's data base, record count and code end. An image restores none
\ of them, and a stripped link refuses a captured cell holding one - the first of
\ them unnamed, because a `create … does>` body spells only cell 0 and the
\ refusal has no name for the rest (measured on a load-time activate/join/kill:
\ `holds a pointer into memory the build mapped word=<unknown> data-off=12219368`
\ for TCB.THREAD, then `holds an undeclared code/dict pointer` for TCB.DBASE).
\ Clearing them changes no run: ACTIVATE writes THREAD again, the join writes RET
\ and PREPARE writes the other three.
: TASK-RELEASE-MEM ( ptr n -- ) {: tcb:ptr :}
   tcb MBOX-DESTROY
   tcb DONE-DESTROY
   tcb TCB.STACK-U @ 0 <> if
      tcb TCB.STACK @ tcb TCB.STACK-U @ MEM-RELEASE-GUARDED
      TASK-NULL tcb TCB.STACK !
      0 tcb TCB.STACK-U !
   then
   tcb TCB.RSTACK-U @ 0 <> if
      tcb TCB.RSTACK @ tcb TCB.RSTACK-U @ MEM-RELEASE-GUARDED
      TASK-NULL tcb TCB.RSTACK !
      0 tcb TCB.RSTACK-U !
   then
   tcb TCB.LSTACK-U @ 0 <> if
      tcb TCB.LSTACK @ tcb TCB.LSTACK-U @ MEM-RELEASE-GUARDED
      TASK-NULL tcb TCB.LSTACK !
      0 tcb TCB.LSTACK-U !
   then
   tcb TCB.REGION-U @ 0 <> if
      tcb TCB.REGION @ tcb TCB.REGION-U @ TASK-MUNMAP-SPAN
      TASK-NULL tcb TCB.REGION !
      0 tcb TCB.REGION-U !
   then
   0 tcb TCB.THREAD !
   0 tcb TCB.RET !
   0 tcb TCB.DBASE !
   0 tcb TCB.NDICT !
   0 tcb TCB.CP ! ;

\ ---- every declared TCB, and the sweep a capture runs over them ---------------
\ ONE HEAD REACHES EVERY TCB THE PROGRAM DECLARED, because a capture has to reach
\ them all: a task that was prepared holds this process's mappings and a task
\ that is running holds its thread, and an image restores neither. The head is a
\ declared pointer cell, so the stripped link relocates it through its data map;
\ each TCB's link is the raw cell one past its body, holding the previous TCB's
\ address, and 0 ends the chain. Both kinds of cell sit in the capture window,
\ which an image restores at the addresses it was captured from, so an
\ undeclared cell holding one of those addresses is shipped verbatim
\ (src/habu/aot-lib.f SCAN-DATA-CELL refuses a text pointer and a pointer into
\ memory the build mapped; a window address is neither). A TCB address is not an
\ offset from `dbase@` either - measured on this engine, the window is at
\ $340000000 and `dbase@` at $1410000 - so the link holds the address itself.
PERSISTED-PTR-VARIABLE TASK-CHAIN

: TASK-CHAIN@ ( -- ptr n )
   TASK-CHAIN @ ;

: TASK-CHAIN! ( ptr n -- )
   TASK-CHAIN ! ;

\ The link cell sits one cell past TCB.DONE. Nothing after the TCB belongs to
\ TASK-ABI, so the layout check above answers for the structure unchanged.
: TASK-LINK@ ( ptr n -- n )
   TASK-TCB-BYTES + @ ;

\ A PREPARED TASK IS RELEASED AT CAPTURE, the same rule RESET-SYMBOLS keeps for
\ the dlsym cells: process-local state is dropped here and taken afresh by the
\ next operation, which for a task is the ACTIVATE that prepares it again. So
\ is every park, an EMPTY task's included: it outlives the release
\ (PARK-CREATE), and a guard captured live would have the restored image skip
\ creating its own. No other thread can post a park while the capture goes on:
\ a task's thread ends the process below, and ROWS-SWEEP refused a foreign
\ thread inside a slot. An ACTIVATED task is the program's defect - a thread
\ cannot be carried by an image - so this names the module, the state and the
\ field the linker would otherwise refuse with no name for it. `die` and not
\ `throw`: a hook may throw and PREPARE propagates it, but hb-build would then
\ end on a bare code, while the linker's own refusals print their reason and
\ exit (src/habu/aot-closure.f REFUSE-DATA-CELL).
: TASK-SWEEP-ONE ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-STATE@ {: st:n :}
   st TASK-EXPOSED = if
      s" task: exposed task at capture - unexpose it before the build or snapshot captures (CB-OWNER)"
      E-TASK-STATE die
   then
   st TASK-EMPTY <> st TASK-CONSTRUCTED <> and if
      s" task: activated task at capture - kill it before the build or snapshot captures (TCB.THREAD)"
      E-TASK-STATE die
   then
   st TASK-CONSTRUCTED = if
      tcb TASK-RELEASE-MEM
      TASK-EMPTY tcb TASK-STATE!
   then
   tcb PARK-DESTROY ;

variable SWEEP-ARMED

\ The flag is cleared last, so a walk that threw part way - TASK-RELEASE-MEM
\ answers a refusing munmap with E-TASK-THREAD - leaves the hook registered for
\ the retry IMAGE-LIFECYCLE:PREPARE keeps a throwing callback for.
\ A binding is process state: idle rows are dropped, and a row a thread is
\ inside names a foreign call in progress, which no image can carry.
: ROWS-SWEEP ( -- )
   CB-POOL 0 ?do
      i ROW ROW-OWNER atomic@ 0 <> if
         s" task: callback slot in flight at capture - return from the foreign call before the build or snapshot captures"
         E-TASK-STATE die
      then
      0 i ROW-NAME!
   loop ;

: TASK-CAPTURE-SWEEP ( -- )
   ROWS-SWEEP
   TASK-CHAIN@ FFI:>CELL
   begin dup 0 <> while
      TASK-N>PTR dup TASK-SWEEP-ONE TASK-LINK@
   repeat
   drop
   0 SWEEP-ARMED ! ;

\ THE SWEEP IS A ONE-SHOT CLEANUP, ARMED BY THE WORD THAT TAKES WHAT IT GIVES
\ BACK. It munmaps, so it has to run in the phase lib/image-lifecycle.f reserves
\ for cleanup that calls foreign functions, before the persistent hooks forget
\ their addresses: registered with REGISTER-PERSISTENT it ran after
\ RESET-SYMBOLS instead, re-resolved munmap through TASK-SYMBOLS, and the build
\ then refused this module's own cell (measured on a subject that only PREPAREs:
\ `holds a pointer into memory the build mapped word=MUNMAP-XT`). A run of the
\ sweep disarms it, and the next PREPARE - in this process or in the restored
\ image - arms it again, so an image that captures again sweeps again. Two owners
\ preparing at once may register two hooks; the second walk finds every task
\ EMPTY and does nothing.
: TASK-ARM-SWEEP ( -- )
   SWEEP-ARMED @ 0= if
      [: TASK-CAPTURE-SWEEP ;] IMAGE-LIFECYCLE:REGISTER
      1 SWEEP-ARMED !
   then ;

: TASK-COPY-CELL ( ptr n ptr n n -- )
   {: src:ptr dst:ptr off:n :}
   src off + @ dst off + ! ;

: TASK-COPY-SPAN ( ptr n ptr n n n -- )
   {: src:ptr dst:ptr off:n bytes:n :}
   bytes 0 ?do src dst off i + TASK-COPY-CELL CELL +loop ;

: TASK-PTR-SLOT ( ptr n n -- ptr ptr n )
   + TASK-CELL>PTR-SLOT ;

: TASK-PTR! ( ptr n ptr n n -- )
   TASK-PTR-SLOT ! ;

: TASK-PTR@ ( ptr n n -- ptr n )
   TASK-PTR-SLOT @ ;

: TASK-REGION-INIT ( ptr n -- ) {: tcb:ptr :}
   tcb TCB.REGION @ {: reg:ptr :}
   data-base reg ARGC-CELL TASK-COPY-CELL
   data-base reg ARGV-CELL TASK-COPY-CELL
   data-base reg ENVP-CELL TASK-COPY-CELL
   \ A task starts on its creator's I/O devices (docs/genio.md). These cells
   \ hold a device INDEX, not an address, which is what makes copying them into
   \ a freshly mapped region sound: nothing here needs relocating. The funnel's
   \ re-entrancy guard and the active-device cell start clear, because the new
   \ task is not inside an operation.
   data-base reg GENIO-ABI:OUT-CELL TASK-COPY-CELL
   data-base reg GENIO-ABI:IN-CELL TASK-COPY-CELL
   data-base reg GENIO-ABI:WRITE-OFF GENIO-ABI:DEVICES cells TASK-COPY-SPAN
   0 reg GENIO-ABI:BUSY-CELL + !
   0 reg GENIO-ABI:ACTIVE-CELL + !
   rbase reg RBASE-CELL + !
   tcb TCB.STACK @ reg STACK-ABI:BASE-CELL TASK-PTR!
   tcb TCB.STACK-U @ reg STACK-ABI:CAP-CELL + !
   tcb TCB.RSTACK @ reg STACK-ABI:RETURN-BASE-CELL TASK-PTR!
   tcb TCB.LSTACK @ reg STACK-ABI:LOOP-BASE-CELL TASK-PTR!
   0 reg RSP-CELL + !
   0 reg LOOPSP-CELL + !
   0 reg LVD-CELL + !
   0 reg HND-CELL + !
   0 reg EVALD-CELL + !
   0 reg EVALERR-CELL + !
   tcb reg TASK-TCB-CELL TASK-PTR!
   1 reg TASKS-LIVE-CELL + ! ;

: TASK-CONSTRUCTED? ( ptr n -- bool )
   TASK-STATE@ TASK-EMPTY <> ;

\ Every stack a task runs on is a guarded mapping (lib/memory.f), so the
\ requested data-stack size rounds up to whole guard pages.
: TASK-STACK-BYTES ( n -- n ) {: want:n :}
   want STACK-ABI:PAGE-BYTES 1 - + STACK-ABI:PAGE-BYTES negate and ;

: PREPARE ( ptr n -- ) {: tcb:ptr :}
   TASK-ARM-SWEEP
   tcb TASK-CONSTRUCTED? if exit then
   tcb TCB.SIZE @ TASK-CHECK-SIZE
   tcb TCB.SIZE @ TASK-STACK-BYTES MEM-ALLOC-GUARDED tcb TCB.STACK-U ! tcb TCB.STACK !
   STACK-ABI:RETURN-BYTES MEM-ALLOC-GUARDED tcb TCB.RSTACK-U ! tcb TCB.RSTACK !
   STACK-ABI:LOOP-BYTES MEM-ALLOC-GUARDED tcb TCB.LSTACK-U ! tcb TCB.LSTACK !
   TASK-REGION-BYTES 8 / >COUNT MEM-ALLOC-CELLS
   TASK-REGION-BYTES tcb TCB.REGION-U !
   tcb TCB.REGION !
   dbase@ tcb TCB.DBASE !
   ndict@ tcb TCB.NDICT !
   cp@ tcb TCB.CP !
   \ This clear and ACTIVATE's are plain stores on purpose: both run in the
   \ owner's thread before pthread_create, which orders everything written here
   \ into the new thread, and once that thread runs it alone writes the cell
   \ (HALT-PENDING?).
   0 tcb TCB.STOP !
   tcb TASK-REGION-INIT
   tcb MBOX-INIT
   tcb PARK-CREATE
   tcb DONE-INIT
   TASK-CONSTRUCTED tcb TASK-STATE! ;

\ This is a foreign C entry address with TASK-ABI's fixed argument contract.
TRUSTED: PTHREAD-ENTRY ( -- n ) task-entry ;

: TASK-PTHREAD-CREATE-RC ( ptr n -- n ) {: tcb:ptr :}
   tcb TCB.THREAD 0 PTHREAD-ENTRY tcb PTHREAD-CREATE-CALL ;

: TASK-PTHREAD-JOIN-CALL ( ptr n -- ) {: tcb:ptr :}
   tcb TCB.THREAD @ tcb TCB.RET PTHREAD-JOIN-CALL TASK-RC0 ;

: TASK-READY ( -- )
   TASK-SYMBOLS ;

: TASK-JOIN-RELEASE ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-PTHREAD-JOIN-CALL
   TASK-LIVE-
   tcb TASK-RELEASE-MEM
   TASK-EMPTY tcb TASK-STATE! ;

: TASK-RUN-USER ( -- n )
   TASK-SELF TCB.USER-XT @ catch ;

\ A scope that finished cleanup while HALT is pending must publish the error
\ selected by finally before PAUSE ends this worker outside TASK-RUNNER's catch.
\ Keep an earlier task failure, as the exit callback chain does.
: TASK-EXIT-FAILURE ( n -- ) {: rc:n :}
   rc 0= if exit then
   TASK-SELF-N dup 0= if drop E-TASK-STATE throw then
   TASK-N>PTR {: self:ptr :}
   self TASK-THROW@ 0= if rc self TASK-THROW! then ;

\ An uncaught worker throw ends this task only: record the code and return, so
\ the entry marks the task DONE and the other tasks keep running. A worker
\ `die` is not catchable and still exits the process with its own status.
\ Either way the task ends through TASK-END, which runs the cleanup and releases
\ whoever is joining.
: TASK-RUNNER ( -- )
   TASK-RUN-USER dup 0= if drop else TASK-SELF TASK-THROW! then
   TASK-END ;

: ACTIVATE ( [ -- ] ptr n -- ) {: xt tcb:ptr :}
   TASK-READY
   tcb TASK-STATE@ TASK-RUNNING = if E-TASK-STATE throw then
   tcb TASK-STATE@ TASK-HALT-REQ = if E-TASK-STATE throw then
   tcb TASK-STATE@ TASK-EXPOSED = if E-TASK-STATE throw then
   tcb TASK-STATE@ TASK-DONE = if tcb TASK-JOIN-RELEASE then
   tcb PREPARE
   \ Every run's park opens at zero. The park outlives this TCB's earlier runs
   \ and exposures (PARK-CREATE), so it may hold a hint from any of them: one
   \ posted while exposed, after a run ended, or around a create that failed.
   tcb PARK-DRAIN
   xt tcb TCB.USER-XT !
   ['] TASK-RUNNER tcb TCB.XT !
   0 tcb TCB.STOP !
   0 tcb TASK-THROW!
   tcb TASK-OUTCOME-RESET
   \ The state the new thread inherits: this store precedes the create and the
   \ entry adds none of its own, so a HALT in the create window stands.
   TASK-RUNNING tcb TASK-STATE!
   TASK-LIVE+
   tcb TASK-PTHREAD-CREATE-RC dup 0 <> if
      TASK-LIVE-
      TASK-CONSTRUCTED tcb TASK-STATE!
      E-TASK-THREAD throw
   then
   drop ;

\ HALT'S REQUEST IS THE STATUS ITSELF: HALT-REQ, which only HALT's CAS writes,
\ over RUNNING. The CAS is a CASAL and TASK-STATE@ an LDAR, so the request and
\ the worker's stop read here are a release/acquire pair. TCB.STOP is the run's
\ own mark that it is ending: TASK-END sets it first, for all three endings, so
\ a cleanup that pauses yields instead of entering the halted exit, and PREPARE
\ and ACTIVATE clear it before the thread exists. No other thread writes it, so
\ it is a plain cell, and a second HALT racing the exit cannot re-arm it.
: HALT-PENDING? ( ptr n -- bool ) {: tcb:ptr :}
   tcb TASK-STATE@ TASK-HALT-REQ = tcb TCB.STOP @ 0= and ;

\ A halted task leaves here rather than through the runner, so this is the third
\ way a task ends and it runs the same TASK-END, whose stop mark makes a cleanup
\ that pauses yield rather than re-enter the exit it is part of.
\
\ The DONE goes out with the same release the entry's STLR makes for a body that
\ returned: this is the worker's own thread publishing its last state, and every
\ write it made - the body's and TASK-END's cleanup - precedes it.
\
\ Inside a callback it only yields and the request stays pending: leaving there
\ would end the thread beneath C's frames, holding the slot it entered through,
\ with C's call never drained. A live marshal frame on the region (CB-FRAME) is
\ that case; the thunk restores the cell as each callback returns, so it reads
\ 0 once C is off the stack. The body returns to C, C returns to the task, and
\ the task ends at its first PAUSE outside a callback and outside a DEFER-ENTER
\ section: a HALT or KILL of a task parked in a callback takes effect after C
\ returns. A body that must answer the request sooner reads TASK:HALTED? and
\ returns.
: IN-CALLBACK? ( -- bool )
   data-base CB-FRAME + @ 0 <> ;

: PAUSE ( -- )
   TASK-SELF-N dup 0= if drop SCHED-YIELD-CALL TASK-RC0 exit then
   TASK-N>PTR dup HALT-PENDING? IN-CALLBACK? 0= and if
         TASK-DEFER-DEPTH @ 0= if
            TASK-END
            TASK-DONE over TCB.STATUS atomic!
            0 PTHREAD-EXIT-CALL
         then
   then
   drop
   SCHED-YIELD-CALL TASK-RC0 ;

\ The request is observed at TASK:PAUSE, so a task parked in TASK:STOP would not
\ see it until somebody else woke it. HALT wakes it itself: the task returns from
\ its STOP, re-checks its state as the hint protocol requires, and ends at its
\ next PAUSE.
\
\ The move RUNNING -> HALT-REQ is one atomic-cas on the status cell, the shape
\ TASK-JOIN-CHECK uses on the joiner claim, and its answer - the state as it was
\ - is read once. THE CAS IS THE WHOLE REQUEST (HALT-PENDING?), so a HALT
\ reaches exactly the run its CAS saw. The status carries no request into
\ another run: only this CAS writes HALT-REQ, and the run's end replaces it -
\ the DONE an ended run publishes, or the CONSTRUCTED a failed create leaves -
\ before ACTIVATE writes the RUNNING the next run starts from.
\
\ A task ends under its own owner: the body's last write comes before TASK-END
\ and before the entry's DONE store, so a halt ordinarily arrives while the
\ task is finishing. An answer that is neither RUNNING nor HALT-REQ means there
\ is nothing to halt - never activated, only prepared, or ended - and that
\ task's state is left alone: a task that has ended has no PAUSE to reach, and
\ DONE is never overwritten by a request nobody can observe.
\
\ The park is posted DIRECTLY rather than through TASK-WAKE, which refuses an
\ ended task: that refusal is TASK:WAKE's own contract, while here the task may
\ end between the CAS and the post and a hint nobody will wait for is harmless.
\ It may be released too, and the post still meets a live park (PARK-CREATE).
\ A count posted before the next run or exposure opens dies there, since each
\ opens its park at zero; one still in flight as it opens is the one stale
\ hint its re-check absorbs. The post is the only step after the CAS, so that
\ hint is all a HALT can leave a run its CAS did not see.
: HALT ( ptr n -- ) {: tcb:ptr :}
   TASK-RUNNING TASK-HALT-REQ tcb TCB.STATUS atomic-cas {: st:n :}
   st TASK-RUNNING <> st TASK-HALT-REQ <> and if exit then
   tcb TASK-PARK-SEM SEM-SIGNAL ;

\ ---- exposed tasks: regions C enters through a callback ----------------------
\ An EXPOSED task runs no body of its own: its prepared region is a context a
\ foreign thread enters through a callback slot bound to it. The resting frame
\ is the one BTASK-ENTRY would start the task on - the base of its data stack
\ and the dictionary PREPARE recorded - so a callback arriving with no outbound
\ call in flight still has a VM to run on. The region counts as live on main
\ while exposed, as an activated task does, so no definition moves the
\ dictionary under it. EXPOSED goes out with a release store: another task's
\ CONTEXT-BIND reads it. An exposure is woken like a run, so it opens its park
\ at zero as ACTIVATE does: a hint left by an earlier run or exposure of this
\ TCB is not this exposure's.
: EXPOSE ( ptr n -- ) {: tcb:ptr :}
   TASK-READY
   tcb PREPARE
   tcb TASK-STATE@ TASK-CONSTRUCTED <> if E-TASK-STATE throw then
   tcb PARK-DRAIN
   tcb TCB.REGION @ {: reg:ptr :}
   tcb TCB.STACK @ FFI:>CELL reg CB-XDS + !
   tcb TCB.DBASE @ reg CB-DBASE + !
   tcb TCB.NDICT @ reg CB-NDICT + !
   tcb TCB.CP @ reg CB-CP + !
   0 reg CB-FRAME + !
   0 reg CB-OWNER + !
   TASK-LIVE+
   TASK-EXPOSED tcb TCB.STATUS atomic! ;

\ A refused UNEXPOSE puts EXPOSED back by CAS before it throws; a state that
\ moved meanwhile is a second lifecycle owner, which ends the process.
: UNEXPOSE-REFUSE ( ptr n -- ) {: tcb:ptr :}
   TASK-CONSTRUCTED TASK-EXPOSED tcb TCB.STATUS atomic-cas TASK-CONSTRUCTED <> if
      s" task: a task moved under its refused UNEXPOSE (TCB.STATUS)" E-TASK-STATE die
   then
   E-TASK-STATE throw ;

\ Refused while a thread is inside or a row naming the region is moving, and a
\ refusal leaves the state and every binding as it was. The state leaves
\ EXPOSED first, EXPOSED -> CONSTRUCTED in one CAS, because a CONTEXT-BIND on
\ another thread may have checked EXPOSED and not yet named its row. The bind
\ names the row (STLR) and then reads the state again (LDAR); this word writes
\ the state (CASAL) and then reads the rows (LDAR). An acquire load never
\ moves above an earlier release write, so at least one side sees the other's
\ write: the bind sees the state gone and hands its row back, or the claim
\ below sees the row and claims it or refuses on it. No row is left naming a
\ region this word unexposed. Another thread can read CONSTRUCTED until a
\ refusal puts EXPOSED back. The CAS also refuses a second UNEXPOSE.
\ The rows are claimed together, before any is dropped, so a refusal hands
\ every one back. A thread inside holds the row it entered by, and it holds the
\ region's owner only while it holds that row - the thunk takes its row before
\ the owner and releases it after - so a thread holding either refuses the
\ claim, and a claim of every row finds the owner free.
: UNEXPOSE ( ptr n -- ) {: tcb:ptr :}
   TASK-EXPOSED TASK-CONSTRUCTED tcb TCB.STATUS atomic-cas
   TASK-EXPOSED <> if E-TASK-STATE throw then
   tcb TCB.REGION @ {: reg:ptr :}
   reg FFI:>CELL ROWS-CLAIM {: mask:n claimed:bool :}
   claimed 0= if tcb UNEXPOSE-REFUSE then
   mask ROWS-DROP
   0 reg CB-XDS + !
   0 reg CB-DBASE + !
   0 reg CB-NDICT + !
   0 reg CB-CP + !
   0 reg CB-FRAME + !
   0 reg CB-OWNER + atomic!
   TASK-LIVE- ;

: CONTEXT ( ptr n -- n ) {: tcb:ptr :}
   tcb TASK-STATE@ TASK-EXPOSED <> if E-TASK-STATE throw then
   tcb TCB.REGION @ FFI:>CELL ;

: SELF-CONTEXT ( -- n )
   data-base FFI:>CELL ;

\ The state first, by acquire load, then the region PREPARE wrote before the
\ release store that published EXPOSED.
: TCB-EXPOSES? ( n ptr n -- bool ) {: ctx:n tcb:ptr :}
   tcb TASK-STATE@ TASK-EXPOSED =
   tcb TCB.REGION @ FFI:>CELL ctx = and ;

\ The exposed task whose region is context n, as a TCB number, or 0.
: EXPOSED-TCB ( n -- n ) {: ctx:n :}
   TASK-CHAIN@ FFI:>CELL
   begin dup 0 <> while
      dup TASK-N>PTR ctx swap TCB-EXPOSES? if exit then
      TASK-N>PTR TASK-LINK@
   repeat ;

\ A context is the main region, the caller's own, or an exposed task's. The
\ last answers its task, the one an UNEXPOSE on another thread can end during
\ the bind; the others answer 0.
: CONTEXT-CHECK ( n -- n ) {: ctx:n :}
   ctx MAIN-BASE FFI:>CELL = if 0 exit then
   ctx SELF-CONTEXT = if 0 exit then
   ctx EXPOSED-TCB dup 0= if E-TASK-STATE throw then ;

: SLOT-CHECK ( n -- ) {: k:n :}
   k 0 < k CB-POOL >= or if E-TASK-STATE throw then ;

\ Binding publishes the row table first and arms the capture sweep. The row
\ moves 0 -> BUSY before it is read, and the bind decides on what it reads
\ under BUSY, where nothing else moves the row. A row that is not idle - a
\ thread inside, or another bind, unbind, claim or drop moving it - refuses
\ the bind, even when it names this context: a mover can still drop the row it
\ holds, so its binding is not final. A named row goes back as found, a no-op
\ when it names this context and refused when it names another. So a bind
\ answers only a binding that is published and final, and names only a row
\ that was free under its own BUSY, named and released each by release store.
\ An exposed task is read again between the naming and the release, the other
\ half of UNEXPOSE's protocol: the re-read (LDAR) follows the naming (STLR), so
\ either it sees the state UNEXPOSE took out of EXPOSED and the row goes back
\ to 0 with the bind refused, or UNEXPOSE's claim sees the row, still BUSY or
\ already bound, and refuses on it or drops it. Held BUSY until the re-read
\ passes, the row admits no thunk into a region an UNEXPOSE has already swept.
\ The re-read checks the region too, so the task unexposed, released and
\ exposed again elsewhere is refused as well. The main region is never
\ unexposed, and a thread binding its own region holds a row naming it, which
\ refuses UNEXPOSE.
: CONTEXT-BIND ( n n -- ) {: ctx:n k:n :}
   ctx CONTEXT-CHECK {: host:n :}
   k SLOT-CHECK
   0 ROW FFI:>CELL MAIN-BASE CB-ROWS + atomic!
   TASK-ARM-SWEEP
   0 CB-ROW-BUSY k ROW ROW-OWNER atomic-cas 0 <> if E-TASK-STATE throw then
   k ROW ROW-REGION atomic@ {: held:n :}
   held 0 <> if
      0 k ROW ROW-OWNER atomic!
      held ctx = if exit then
      E-TASK-STATE throw
   then
   ctx k ROW-NAME!
   host 0 <> if
      ctx host TASK-N>PTR TCB-EXPOSES? 0= if 1 k lshift ROWS-DROP E-TASK-STATE throw then
   then
   0 k ROW ROW-OWNER atomic! ;

: CONTEXT-UNBIND ( n -- ) {: k:n :}
   k SLOT-CHECK
   0 CB-ROW-BUSY k ROW ROW-OWNER atomic-cas 0 <> if E-TASK-STATE throw then
   0 k ROW-NAME!
   0 k ROW ROW-OWNER atomic! ;

\ ONE read of the state decides, because the state moves while the owner is
\ reading it: a task that answers RUNNING to the first test can be DONE by the
\ next, and re-reading per arm made every arm false - the task was neither
\ halted nor joined, TASK-LIVE- never ran, and it stayed counted live for the
\ life of the image (the next dictionary mutation then exits $4F).
\
\ EMPTY has nothing to release and CONSTRUCTED has memory but no thread. Every
\ other state was activated and is released the same way, whichever state it
\ reaches first: HALT unless the read already said DONE - it is the no-op above
\ on a task that ended in between - and then the join, which waits for the
\ thread however the task ends.
: TASK-KILL ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-STATE@ {: st:n :}
   st TASK-EMPTY = if exit then
   st TASK-EXPOSED = if tcb UNEXPOSE then
   st TASK-CONSTRUCTED = st TASK-EXPOSED = or if
      tcb TASK-RELEASE-MEM
      TASK-EMPTY tcb TASK-STATE!
      exit
   then
   st TASK-DONE <> if tcb HALT then
   tcb TASK-JOIN-RELEASE ;

: TASK-DONE? ( ptr n -- bool )
   TASK-STATE@ TASK-DONE = ;

\ A join needs a task that is running or has ended, and exactly one joiner: the
\ claim moves 0 -> 1 in one step, so a second joiner is refused instead of being
\ made to wait for a signal the first has already taken. A task that was never
\ activated and a task whose join has released it are the same EMPTY task and
\ get the same refusal.
: TASK-JOIN-CHECK ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-STATE@ {: st:n :}
   st TASK-RUNNING <> st TASK-HALT-REQ <> and st TASK-DONE <> and
      if E-TASK-JOIN throw then
   0 1 tcb TCB.JOINER atomic-cas 0 <> if E-TASK-JOIN throw then ;

\ An error beats a value: a body that returned and then failed in its cleanup did
\ not finish, and the code that ended it is the honest answer. A task that ended
\ without calling TASK:RETURN has no value to give, and saying so with a named
\ code keeps ok meaning "the worker answered".
: TASK-JOIN-ANSWER ( ptr n -- result<n,n> ) {: tcb:ptr :}
   tcb TASK-THROW@ dup 0 <> if RESULT:ERR exit then
   drop
   tcb TASK-RESULT? if tcb TCB.RESULT @ RESULT:OK exit then
   E-TASK-NO-RESULT RESULT:ERR ;

\ Blocks in the done semaphore, so a joiner is parked in the kernel rather than
\ polling DONE?. The release is the one TASK:KILL does, which is why a joined
\ task needs no kill; the outcome is read after it, from TCB rows it leaves
\ alone.
: TASK-JOIN ( ptr n -- result<n,n> ) {: tcb:ptr :}
   tcb TASK-JOIN-CHECK
   tcb TASK-DONE-SEM SEM-WAIT
   tcb TASK-JOIN-RELEASE
   tcb TASK-JOIN-ANSWER ;

\ ---- the three storage definers, and why only one converted -------------------
\ TWO ADDRESS KINDS LIVE HERE, and only one of them is expressible as generated
\ source today. A `create ... does> ( -- ptr n ) ;` word pushes the ABSOLUTE
\ address of its dictionary storage, which is the same address in every thread.
\ A generated `data-base <off> +` word resolves against the CALLING thread's data
\ region - register 20, which the thread entry below points at the task's own
\ 64 KiB region (TCB.REGION) - so inside a task it names a different object.
\
\ TASK and FACILITY publish SHARED storage: one TCB per task and one pthread
\ mutex every task locks. They keep `create ... does>` because the tree has no
\ checked source expression for a thread-invariant address (`dbase@`, register 26,
\ holds it but is typed `-- n`, so writing it would trade a does> for a new
\ pointer cast). Measured, not assumed: converting FACILITY makes each task lock
\ its own uninitialised copy - lib/task-test.f dies E-TASK-THREAD (rc 237) - and
\ converting TASK faults reading a TCB at region-base + dictionary-offset. The
\ missing piece is one thread-invariant `( -- ptr n )` origin; until it exists,
\ generated `data-base` accessors are for region-local storage only.
\
\ +USER is the other kind and converts cleanly: its storage IS a task-local slot,
\ its does> body already read `@ data-base +`, and the offsets it hands out are
\ bounded by TASK-USER-END, inside every region.

\ CREATE/DOES> publishes a typed TCB address. The body is still
\ TASK-TCB-BYTES; the cell after it is this TCB's chain link,
\ taking the head the definition found and leaving its own address there.
: TASK ( n -- )
   dup TASK-CHECK-SIZE
   TASK-ALIGN8
   create
      here CELL-VIEW >r
      ,  TASK-TCB-BYTES 8 / 1 - 0 do 0 , loop
      TASK-CHAIN@ FFI:>CELL ,
      r> TASK-CHAIN!
   does> ( -- ptr n ) ;

: #USER ( -- n )
   TASK-USER-NEXT @ dup 0= if drop TASK-USER-BASE then ;

: USER-NEXT ( n n -- n ) {: off:n size:n :}
   off TASK-USER-BASE < if E-TASK-USER throw then
   off TASK-USER-END > if E-TASK-USER throw then
   size 0 < if E-TASK-USER throw then
   size TASK-USER-END off - > if E-TASK-USER throw then
   off size + ;

\ The slot's offset is fixed when the slot is declared, so it is baked into the
\ generated body; `data-base` is read at CALL time, which is what makes the word
\ answer the RUNNING task's own region - the whole point of a user slot, and what
\ the retired does> body (`@ data-base +`) did with an extra load.
\
\ The text is built with package CODEGEN's append machinery rather than a fifth
\ private copy of it. The calls are qualified rather than imported: a bare RESET
\ under `using CODEGEN` collides with the global RESET (E-USING-SHADOW-GLOBAL),
\ which is the collision docs/forth.md names as the case for qualifying.
$60 constant SLOT-GEN-CAP
SLOT-GEN-CAP CODEGEN:BUFFER SLOT-GEN

: SLOT-NAME ( -- ptr u8 n )
   parse-name dup 0= if E-TASK-USER throw then ;

: +USER ( n n -- n ) {: off:n size:n :}
   off size USER-NEXT {: next:n :}
   next TASK-USER-NEXT !
   SLOT-NAME {: name:ptr nameu:n :}
   SLOT-GEN CODEGEN:RESET
   s" : " SLOT-GEN CODEGEN:APPEND-STRING
   name nameu SLOT-GEN CODEGEN:APPEND-STRING
   s"  ( -- ptr n ) data-base " SLOT-GEN CODEGEN:APPEND-STRING
   off SLOT-GEN CODEGEN:APPEND-DECIMAL
   s"  + ;" SLOT-GEN CODEGEN:APPEND-STRING
   SLOT-GEN CODEGEN:CONTENTS INCLUDE-EVALUATE
   next ;

: HIS ( ptr n ptr n -- ptr n ) {: tcb:ptr cur:ptr :}
   cur data-base - tcb TCB.REGION @ + ;

\ CREATE/DOES> publishes owner-tracked pthread mutex storage, shared by every
\ task (see the address-kind note above).
: FACILITY ( -- )
   TASK-ALIGN8
   create TASK-FACILITY-BYTES allot
   does> ( -- ptr n ) ;

: FACILITY-OWNER ( ptr n -- ptr n )
   TASK-FACILITY-OWNER-OFF + ;

: FACILITY-MUTEX ( ptr n -- ptr n )
   TASK-FACILITY-MUTEX-OFF + ;

: TASK-OWNER ( -- n )
   TASK-SELF-N dup 0= if drop data-base FFI:>CELL then ;

: FACILITY-OWNER@ ( ptr n -- n )
   FACILITY-OWNER atomic@ ;

: FACILITY-OWNER! ( n ptr n -- )
   FACILITY-OWNER atomic! ;

: FACILITY-INIT ( ptr n -- )
   0 over FACILITY-OWNER!
   FACILITY-MUTEX 0 MUTEX-INIT-CALL TASK-RC0 ;

: GET ( ptr n -- ) {: f:ptr :}
   TASK-OWNER {: owner:n :}
   f FACILITY-OWNER@ owner = if exit then
   f FACILITY-MUTEX MUTEX-LOCK-CALL TASK-RC0
   owner f FACILITY-OWNER! ;

: RELEASE ( ptr n -- ) {: f:ptr :}
   TASK-OWNER {: owner:n :}
   f FACILITY-OWNER@ owner <> if exit then
   0 f FACILITY-OWNER!
   f FACILITY-MUTEX MUTEX-UNLOCK-CALL TASK-RC0 ;

\ Shared counted-semaphore storage, one record per definition (see the
\ address-kind note above). Unlike TASK and FACILITY this definer needs no
\ TRUSTED:: the child's `does>` body converts the record address to the handle
\ in checked code, and both converters are private to this package, so the only
\ address a caller ever sees is already a SEM and no package outside can mint a
\ handle over memory of its own.
: SEMAPHORE ( -- )
   TASK-ALIGN8
   create TASK-SEMAPHORE-BYTES allot
   does> ( -- sem ) FFI:>CELL >SEM ;

\ ---- sleeping ----------------------------------------------------------------
\ The semaphores above park a task until something happens; SLEEP parks it until
\ a time arrives. It is what replaces a TASK:PAUSE loop against mono-ns: the
\ loop burns a core for the whole wait and the sleep burns none.
\
\ Storage class: task-local. The request and the remainder are one
\ TASK-SLEEP-BYTES row of the per-task user band, so the main task and every
\ worker may be asleep at the same moment over their own pair of timespecs. A
\ sleeping task therefore holds nothing: the row is its own, and the argument
\ tables the generated call stages sit in its own DATA region, exactly as a
\ blocked sem_wait's do.
#USER 7 + TASK-CELL-MASK and TASK-SLEEP-BYTES +USER SLEEP-SPECS drop

: SLEEP-REQ ( -- ptr n )
   SLEEP-SPECS ;

: SLEEP-REM ( -- ptr n )
   SLEEP-SPECS TASK-TIMESPEC-BYTES + ;

\ Nanoseconds as a timespec, and back. The kernel's remainder is read the way the
\ request was written, so nothing outside these two words handles the fields.
: SPEC! ( n ptr n -- ) {: ns:n spec:ptr :}
   ns TASK-NS-PER-S /   spec TASK-SPEC-SEC-OFF + !
   ns TASK-NS-PER-S mod spec TASK-SPEC-NSEC-OFF + ! ;

: SPEC@ ( ptr n -- n ) {: spec:ptr :}
   spec TASK-SPEC-SEC-OFF + @ TASK-NS-PER-S *
   spec TASK-SPEC-NSEC-OFF + @ + ;

\ The deadline shape lib/process.f states as PROC-DEADLINE-AT and PROC-LEFT-MS,
\ in nanoseconds because a sleep resumes on the nanosecond the kernel hands back
\ and not on a millisecond boundary. Same arithmetic, same floor at zero.
: SLEEP-DEADLINE-AT ( ms -- n ) {: timeout:ms :}
   mono-ns timeout MS>N TASK-NS-PER-MS * + ;

: SLEEP-LEFT-NS ( n -- n ) {: deadline:n :}
   deadline mono-ns - dup 0 <= if drop 0 then ;

\ Zero, or the errno the call refused with.
: SLEEP-CALL ( -- n )
   SLEEP-REQ BYTE-VIEW SLEEP-REM BYTE-VIEW NANOSLEEP-CALL
   0= if 0 exit then
   FFI:ERRNO ;

\ A signal wakes the thread early, and nanosleep reports EINTR with the time it
\ did not serve in the second timespec: the retry asks for exactly that
\ remainder, so no signal can shorten the sleep. The remainder is capped by what
\ is left of an absolute deadline taken before the first call, so no storm of
\ signals can stretch the total either - the retries converge on the deadline
\ from below. Zero returns without entering the kernel; a duration below zero is
\ an operand nothing could serve.
: TASK-SLEEP ( ms -- ) {: timeout:ms :}
   timeout MS>N {: want:n :}
   want 0 < if E-TASK-SLEEP-MS throw then
   want 0= if exit then
   timeout SLEEP-DEADLINE-AT {: deadline:n :}
   want TASK-NS-PER-MS * SLEEP-REQ SPEC!
   begin
      SLEEP-CALL dup 0= if drop exit then
      TASK-EINTR <> if E-TASK-THREAD throw then
      SLEEP-REM SPEC@ deadline SLEEP-LEFT-NS min {: next:n :}
      next 0 <= if exit then
      next SLEEP-REQ SPEC!
   again ;

public

TASK-MIN-STACK constant MIN-STACK

: TASK ( n -- )
   TASK ;

: PREPARE ( ptr n -- )
   PREPARE ;

: ACTIVATE ( [ -- ] ptr n -- )
   ACTIVATE ;

: SELF ( -- ptr n )
   TASK-SELF ;

: SELF-N ( -- n )
   TASK-SELF-N ;

\ ---- callback contexts (docs/ffi-callback.md) ----------------------------------
\ The main region, from any task.
: MAIN-BASE ( -- ptr n )
   MAIN-BASE ;

\ A prepared task becomes a context C may enter from a thread of its own; it is
\ never activated. UNEXPOSE is refused while a thread is inside or a bind is
\ moving a slot naming it, and drops the bindings naming it; TASK:KILL of an
\ exposed task unexposes it first.
: EXPOSE ( ptr n -- )
   EXPOSE ;

: UNEXPOSE ( ptr n -- )
   UNEXPOSE ;

\ The context number of an exposed task, and of the calling task's own region.
: CONTEXT ( ptr n -- n )
   CONTEXT ;

: SELF-CONTEXT ( -- n )
   SELF-CONTEXT ;

\ Binds slot n to a context, which must be the main region, the caller's own
\ or an exposed task's. Binding an idle slot to the context it names is a
\ no-op; a slot naming another context, with a thread inside, or that another
\ bind, unbind, UNEXPOSE or task end is moving, is refused, and so is a bind
\ to an exposed task that an UNEXPOSE on another thread ends meanwhile.
\ A binding lives until CONTEXT-UNBIND, UNEXPOSE, the end of the task whose
\ region it names, or a capture.
: CONTEXT-BIND ( n n -- )
   CONTEXT-BIND ;

: CONTEXT-UNBIND ( n -- )
   CONTEXT-UNBIND ;

: PAUSE ( -- )
   PAUSE ;

\ Critical task-local resource transfers and disposal may call PAUSE while a
\ HALT is pending. The request remains pending until a later PAUSE outside it.
: DEFER-ENTER ( -- )
   TASK-DEFER-ENTER ;

: DEFER-LEAVE ( -- )
   TASK-DEFER-LEAVE ;

\ True once a TASK:HALT has been requested of this run. A STOP loop reads it
\ after every STOP, so a wait of its own can give its resources back before the
\ TASK:PAUSE that ends the task. A body parked inside a callback reads it to
\ return to C, because TASK:PAUSE there only yields. The main thread has no TCB,
\ is never halted, and answers false.
: HALTED? ( -- bool )
   TASK-SELF-N dup 0= if drop false exit then
   TASK-N>PTR TASK-STATE@ TASK-HALT-REQ = ;

\ Publish the code already selected by a finished scoped body and its cleanup
\ before that body services a pending HALT at PAUSE. Zero changes nothing.
: EXIT-FAILURE ( n -- )
   TASK-EXIT-FAILURE ;

\ Parks the calling task for at least ms milliseconds and returns as soon after
\ that as the scheduler allows, from the main task or from a worker. It burns no
\ CPU: the task is in the kernel rather than in a PAUSE loop, and it holds
\ nothing while it is there. A sleeping task observes no TASK:HALT until it
\ wakes, so TASK:KILL on one waits out the rest of the sleep. Zero returns at
\ once; a duration below zero is E-TASK-SLEEP-MS.
: SLEEP ( ms -- )
   TASK-SLEEP ;

\ ---- the task's own wake-up --------------------------------------------------
\ Parks the calling task until somebody WAKEs it, from a worker or from the main
\ thread, which parks on the one main record instead of a TCB. It burns no CPU
\ and observes no TASK:HALT while it is parked - TASK:HALT wakes its target, so a
\ halted task returns from its STOP and ends at its next TASK:PAUSE.
\
\ WAKE is a HINT, not a message: the count keeps a WAKE that arrives before the
\ STOP, so no hint is lost, but a STOP takes one hint whoever posted it. A caller
\ re-checks its own state after every STOP and stops again if the state it waits
\ for has not arrived:
\
\    begin  MY-STATE @ DONE = if exit then  TASK:STOP  again
\
\ A loop that must also answer TASK:HALT calls TASK:PAUSE in that loop.
: STOP ( -- )
   TASK-STOP ;

\ Posts the named task's park. The null TCB - what TASK:SELF answers on the main
\ thread - posts the main thread's park. An exposed task's park is the one a
\ thread inside its context stops on. A task that was never activated, one that
\ is only prepared and one that has ended are E-TASK-STATE.
: WAKE ( ptr n -- )
   TASK-WAKE ;

\ Requests the stop the target observes at its next TASK:PAUSE, and wakes it so
\ a task parked in TASK:STOP gets there. A task inside a callback observes it at
\ its first TASK:PAUSE after C returns. A task that has ended, one that was
\ never activated and one that is only prepared have no PAUSE to reach: halting
\ them is a no-op that leaves their state alone.
: HALT ( ptr n -- )
   HALT ;

\ Releases the task: a task that was activated is halted if it is still running,
\ joined, and its memory given back, whichever of those states it reaches first
\ - a body ending under the KILL is the teardown shape, not an error. A task
\ that was never activated or was already released is the no-op it is after a
\ TASK:JOIN.
: KILL ( ptr n -- )
   TASK-KILL ;

: DONE? ( ptr n -- bool )
   TASK-DONE? ;

\ Zero until this task's body ends with an uncaught throw; cleared by ACTIVATE.
: THROW@ ( ptr n -- n )
   TASK-THROW@ ;

\ ---- the task's outcome ------------------------------------------------------
\ The worker's answer, stored in its own TCB. Single-assignment: a second call,
\ including one from the task's cleanup, is E-TASK-STATE, and so is a call from a
\ thread that is not a task.
: RETURN ( n -- )
   TASK-RETURN ;

\ Waits for the task to end, releases it as TASK:KILL would, and answers ok with
\ the value the worker stored or err with the code that ended it - including
\ E-TASK-NO-RESULT when the worker ended without an answer. A task that was never
\ activated, a task already joined, and a second joiner are E-TASK-JOIN.
: JOIN ( ptr n -- result<n,n> )
   TASK-JOIN ;

\ Adds a cleanup quotation to that task's chain, run in the task's own thread
\ when it ends - body returned, body threw, or halted at TASK:PAUSE - before the
\ join is released, newest first. A quotation already in the chain is not added
\ again. A throw inside a cleanup becomes the task's error if it has none and
\ never leaves the task.
: AT-EXIT ( [ -- ] ptr n -- )
   TASK-AT-EXIT ;

: #USER ( -- n )
   #USER ;

: +USER ( n n -- n )
   +USER ;

: HIS ( ptr n ptr n -- ptr n )
   HIS ;

: FACILITY ( -- )
   FACILITY ;

: FACILITY-INIT ( ptr n -- )
   FACILITY-INIT ;

: GET ( ptr n -- )
   GET ;

: RELEASE ( ptr n -- )
   RELEASE ;

\ Defines one counted semaphore, shared by every task:
\    TASK:SEMAPHORE ITEMS       \ ITEMS ( -- TASK:sem )
\    0 ITEMS TASK:SEMAPHORE-INIT
: SEMAPHORE ( -- )
   SEMAPHORE ;

\ n is the initial count, 0..SEM_VALUE_MAX, on either host backend.
: SEMAPHORE-INIT ( n sem -- )
   SEM-INIT ;

\ Idempotent; the caller has already ended every waiter.
: SEMAPHORE-DESTROY ( sem -- )
   SEM-DESTROY ;

\ Blocks until the count is positive, then decrements it.
: WAIT ( sem -- )
   SEM-WAIT ;

\ Increments the count and wakes one waiter.
: SIGNAL ( sem -- )
   SEM-SIGNAL ;

\ Takes a count if one is there; false means it would have blocked.
: TRY-WAIT ( sem -- bool )
   SEM-TRY-WAIT ;

\ A semaphore for a caller that needs one at run time rather than at definition,
\ over a record from this package's pool: E-TASK-SEM-POOL when every record is
\ in use. It arrives uninitialized, like a defined one.
: NEW-SEMAPHORE ( -- sem )
   SEM-NEW ;

\ Destroys it if it is still live and returns the record to the pool. A handle
\ that did not come from the pool is E-TASK-SEM-POOL.
: FREE-SEMAPHORE ( sem -- )
   SEM-FREE ;

\ ---- messages ----------------------------------------------------------------
\ Blocks while the target still holds an unread message, then deposits this one
\ and wakes the target. Both ends are tasks: a target that is not running, the
\ sending task itself, and a caller with no TCB are E-TASK-MAILBOX.
: SEND-MESSAGE ( n ptr n -- )
   MBOX-SEND ;

\ Blocks until this task's mailbox holds a message, then answers it and the TCB
\ of the task that sent it. A caller with no TCB is E-TASK-MAILBOX.
: GET-MESSAGE ( -- n ptr n )
   MBOX-GET ;

\ Whether that task holds an unread message. Never blocks, never throws.
: MSG? ( ptr n -- bool )
   MBOX-MSG? ;

\ An engine that bakes TASK keeps these cells below a stripped application's
\ DATA window. Symbol names and the user-slot cursor are declarations; the
\ chain head points to the application's fixed-address TCBs. Foreign addresses
\ and the symbols handshake are process-local and start unresolved after the
\ capture lifecycle has run. A TASK loaded inside the application's window
\ owns its own cells, so the baked-state guards apply only below that window.
\ Callback rows are process state too: ROWS-SWEEP refuses a row in flight at
\ capture and unbinds the rest, so every row restarts at zero, and the store
\ counts beside them (MOVES) are compared only within one process.
: OWNED-CELLS ( n [ ptr u8 n -- ] [ ptr u8 n -- ] -- )
   {: window:n carry fresh :}
   TASK-SYM-PTHREAD-CREATE FFI:>CELL window < if
      TASK-SEM-POOL-N 0 ?do
         i SEM-POOL-USED @ 0<> if
            s" task: stripped image cannot carry a live semaphore" 74 die
         then
      loop
   then
   TASK-SYM-PTHREAD-CREATE
   TASK-USER-NEXT FFI:>CELL TASK-SYM-PTHREAD-CREATE FFI:>CELL -
   carry execute
   TASK-USER-NEXT BYTE-VIEW CELL carry execute
   TASK-CHAIN BYTE-VIEW CELL carry execute
   MUNMAP-XT BYTE-VIEW 8 cells fresh execute
   SYMBOLS-REGISTERED BYTE-VIEW CELL fresh execute
   SYMBOLS-READY BYTE-VIEW CELL fresh execute
   SWEEP-ARMED BYTE-VIEW CELL fresh execute
   TASK-SEM-USED TASK-SEM-POOL-N cells fresh execute
   TASK-SEM-POOL TASK-SEM-POOL-N TASK-SEMAPHORE-BYTES * fresh execute
   MAIN-PARK-REC TASK-SEMAPHORE-BYTES fresh execute
   MAIN-PARK-READY BYTE-VIEW CELL fresh execute
   ROWS CB-POOL CB-ROW-BYTES * fresh execute
   MOVES CB-POOL cells fresh execute
   0 TASK-EXIT-QT BYTE-VIEW TASK-EXIT-MAX cells carry execute
   TASK-EXIT-LINK TASK-EXIT-MAX cells carry execute
   TASK-EXIT-N BYTE-VIEW CELL carry execute
   TASK-EXIT-LOCK BYTE-VIEW CELL fresh execute
   TASK-EXIT-SCRATCH BYTE-VIEW CELL fresh execute ;

\ The linker relocates only the active declaration rows. Each destination is
\ inside its carried run, while the typed source keeps the quotation's proof.
: OWNED-EXIT-N ( -- n ) TASK-EXIT-N @ ;

: OWNED-EXIT-AT ( n -- ptr u8 [ -- ] ) {: idx:n :}
   0 TASK-EXIT-QT BYTE-VIEW idx cells +
   idx TASK-EXIT-QT @ ;

private

get-current prot-wid-add

public

get-current prot-wid-add

;package
