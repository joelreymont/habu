\ task.f - checked CPU tasking over pthread.

s" lib/errors.f" required
s" lib/memory.f" required
s" lib/ffi-abi.f" required
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
$20 constant TASK-SEM-BYTES              \ sem_t on an LP64 host
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
TASK-USER-BASE TASK-USER-NEXT !

\ The arena stops where USER-BAND does. Nothing engine-owned lies inside it -
\ src/habu/layout.f asserts that over every declared claim at build time - so
\ a refused row here means the band is full, not that a library was about to
\ overwrite the AOT window the way the old $41C8..$5000 bound allowed.
USER-BAND:END constant TASK-USER-END

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

\ ---- unnamed POSIX semaphore bindings ----------------------------------------
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

FUNCTION: SEM-INIT-CALL sem_init ( ptr u8 n n -- n )
   0 TASK-SEM-BYTES WRITES-BYTES          \ sem_t; then pshared, then the count
;FUNCTION

FUNCTION: SEM-WAIT-CALL sem_wait ( ptr u8 -- n )
   0 TASK-SEM-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: SEM-POST-CALL sem_post ( ptr u8 -- n )
   0 TASK-SEM-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: SEM-DESTROY-CALL sem_destroy ( ptr u8 -- n )
   0 TASK-SEM-BYTES WRITES-BYTES
;FUNCTION

\ sem_trywait is the same decrement without the block: EAGAIN is the answer
\ "would have blocked", not a failure, and it is what the queue's TRY-PUSH and
\ TRY-POP refuse on.
FUNCTION: SEM-TRYWAIT-CALL sem_trywait ( ptr u8 -- n )
   0 TASK-SEM-BYTES WRITES-BYTES
;FUNCTION

\ nanosleep is the other call here that blocks on purpose: it parks the calling
\ thread until a time arrives rather than until something happens. The first
\ timespec is the request, which the kernel only reads; the second is the one it
\ writes when a signal cuts the sleep short, so the extent is stated on that
\ argument alone and the request stays a read-only pointer.
FUNCTION: NANOSLEEP-CALL nanosleep ( ptr u8 ptr u8 -- n )
   1 TASK-TIMESPEC-BYTES WRITES-BYTES
;FUNCTION

: TASK-RC0 ( n -- )
   dup 0 <> if E-TASK-THREAD throw then
   drop ;

: TASK-CHECK-SIZE ( n -- )
   dup TASK-MIN-STACK < if E-TASK-SIZE throw then
   drop ;

: TASK-STATE@ ( ptr n -- n )
   TCB.STATUS @ ;

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

\ A nominal cannot be retyped straight to a pointer (E-CAST-CLASS), so the
\ handle crosses back through n and the module's existing raw-cell refinement.
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

\ Unnamed POSIX semaphores are a Linux facility: Darwin's sem_init is a
\ deprecated ENOSYS stub, which is why SwiftForth opens named semaphores there
\ (docs/tasking-models.md section 1).
: SEM-HOST-CHECK ( -- )
   HB-TARGET-LINUX? 0= if E-TASK-SEM-HOST throw then ;

: SEM-COUNT-CHECK ( n -- ) {: value:n :}
   value 0 < value TASK-SEM-MAX > or if E-TASK-SEM-COUNT throw then ;

: SEM-INIT ( n sem -- ) {: value:n s:sem :}
   SEM-HOST-CHECK
   value SEM-COUNT-CHECK
   s SEM-LIVE? if E-TASK-SEM-STATE throw then
   s SEM-OBJ 0 value SEM-INIT-CALL TASK-RC0
   s SEM>N s SEM-GUARD atomic! ;

\ POSIX leaves destroying a semaphore that still has blocked waiters undefined,
\ so the caller drains its waiters first. Destroying an inactive one is a no-op.
: SEM-DESTROY ( sem -- ) {: s:sem :}
   s SEM-LIVE? 0= if exit then
   0 s SEM-GUARD atomic!
   s SEM-OBJ SEM-DESTROY-CALL TASK-RC0 ;

\ Blocks inside the host call, so a waiting task needs no PAUSE loop - and
\ observes no TASK:HALT - until it is signalled. A signal interrupts the wait
\ without consuming a count, so EINTR retries.
: SEM-WAIT ( sem -- ) {: s:sem :}
   begin
      s SEM-CHECK
      s SEM-OBJ SEM-WAIT-CALL 0= if exit then
      FFI:ERRNO TASK-EINTR <> if E-TASK-THREAD throw then
   again ;

: SEM-SIGNAL ( sem -- ) {: s:sem :}
   s SEM-CHECK
   s SEM-OBJ SEM-POST-CALL TASK-RC0 ;

\ The decrement that never blocks: true when it took a count, false when the
\ count was zero. EAGAIN is that answer, EINTR retries as SEM-WAIT does.
: SEM-TRY-WAIT ( sem -- bool ) {: s:sem :}
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

\ Created with the task and destroyed with its memory, exactly like the mailbox
\ and the done semaphore.
: PARK-INIT ( ptr n -- ) {: tcb:ptr :}
   0 tcb TASK-PARK-SEM SEM-INIT ;

: PARK-DESTROY ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-PARK-SEM SEM-DESTROY ;

\ The main thread has no TCB - TASK:SELF answers the null TCB there - so its
\ park is this one record and a WAKE of the null TCB posts it, which is what
\ lets a program with no tasks of its own wait on a loop too. It is initialized
\ on first use rather than at load time because unnamed POSIX semaphores are a
\ Linux facility (SEM-HOST-CHECK) and this file still loads on Darwin; the
\ handshake is TASK-SYMBOLS', so two tasks waking the main thread at the same
\ moment initialize the record once.
TASK-ALIGN8
create MAIN-PARK-REC TASK-SEMAPHORE-BYTES 8 / TASK-ZERO-CELLS,
TASK-ALIGN8
variable MAIN-PARK-READY

: MAIN-PARK ( -- sem )
   MAIN-PARK-REC CELL-VIEW SEM-AT ;

: MAIN-PARK-INIT ( -- )
   begin
      MAIN-PARK-READY atomic@ 2 = if exit then
      0 1 MAIN-PARK-READY atomic-cas 0= if
         [: 0 MAIN-PARK SEM-INIT ;] catch dup 0 <> if
            0 MAIN-PARK-READY atomic! throw
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
\ activation is not entitled to, so all three are refused.
: TASK-WAKE ( ptr n -- ) {: tcb:ptr :}
   tcb FFI:>CELL 0= if MAIN-PARK-INIT MAIN-PARK SEM-SIGNAL exit then
   tcb TASK-STATE@ {: st:n :}
   st TASK-RUNNING <> st TASK-HALT-REQ <> and if E-TASK-STATE throw then
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

\ Every way a task's thread ends passes here: the body returning, the body
\ throwing, and a halted body leaving at TASK:PAUSE. The signal is last, so a
\ joiner that wakes finds the cleanup finished and the outcome rows final.
: TASK-END ( -- )
   TASK-RUN-EXIT
   TASK-SELF TASK-DONE-SEM SEM-SIGNAL ;

\ One shared cell counted from several threads: an owner releasing one task can
\ run while another thread activates a second, so each side moves the count in
\ ONE atomic step. No clamp below zero - every release follows an activation, so
\ a count below zero is a pairing defect, and a nonzero count already refuses
\ dictionary mutation, which is where it must surface (exit $4F) rather than in
\ a clamp that hides it.
: TASK-LIVE+ ( -- )
   1 data-base TASKS-LIVE-CELL + atomic-add drop ;

: TASK-LIVE- ( -- )
   -1 data-base TASKS-LIVE-CELL + atomic-add drop ;

: TASK-MUNMAP-SPAN ( ptr n n -- )
   MUNMAP-CALL TASK-RC0 ;

: TASK-RELEASE-MEM ( ptr n -- ) {: tcb:ptr :}
   tcb MBOX-DESTROY
   tcb PARK-DESTROY
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
   0 tcb TCB.STOP !
   tcb TASK-REGION-INIT
   tcb MBOX-INIT
   tcb PARK-INIT
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
   tcb TASK-STATE@ TASK-DONE = if tcb TASK-JOIN-RELEASE then
   tcb PREPARE
   xt tcb TCB.USER-XT !
   ['] TASK-RUNNER tcb TCB.XT !
   0 tcb TCB.STOP !
   0 tcb TASK-THROW!
   tcb TASK-OUTCOME-RESET
   TASK-RUNNING tcb TASK-STATE!
   TASK-LIVE+
   tcb TASK-PTHREAD-CREATE-RC dup 0 <> if
      TASK-LIVE-
      TASK-CONSTRUCTED tcb TASK-STATE!
      E-TASK-THREAD throw
   then
   drop ;

: TASK-STOP@ ( ptr n -- n )
   TCB.STOP @ ;

: TASK-STOP! ( n ptr n -- )
   TCB.STOP ! ;

\ A halted task leaves here rather than through the runner, so this is the third
\ way a task ends and it runs the same TASK-END. The stop flag is cleared first:
\ a cleanup that pauses must yield, not re-enter the exit it is part of.
: PAUSE ( -- )
   TASK-SELF-N dup 0= if drop SCHED-YIELD-CALL TASK-RC0 exit then
   TASK-N>PTR dup TASK-STOP@ 0 <> if
         0 over TASK-STOP!
         TASK-END
         TASK-DONE over TASK-STATE!
         0 PTHREAD-EXIT-CALL
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
\ - is read once. A task ends under its own owner: the body's last write comes
\ before TASK-END and before the entry's DONE store, so a halt ordinarily
\ arrives while the task is finishing. An answer that is neither RUNNING nor
\ HALT-REQ means there is nothing to halt - never activated, only prepared, or
\ ended - and that task's state and stop flag are left alone: a task that has
\ ended has no PAUSE to reach, and DONE is never overwritten by a request nobody
\ can observe.
\
\ The park is posted DIRECTLY rather than through TASK-WAKE, which refuses an
\ ended task: that refusal is TASK:WAKE's own contract, while here the task may
\ end between the CAS and the post and a hint nobody will wait for is harmless.
\ It cannot reach the next run either - TASK-RELEASE-MEM destroys the park
\ record with the task's memory and PREPARE's PARK-INIT opens the next one at
\ zero, so a stray count dies with the run that never took it.
: HALT ( ptr n -- ) {: tcb:ptr :}
   TASK-RUNNING TASK-HALT-REQ tcb TCB.STATUS atomic-cas {: st:n :}
   st TASK-RUNNING <> st TASK-HALT-REQ <> and if exit then
   1 tcb TASK-STOP!
   tcb TASK-PARK-SEM SEM-SIGNAL ;

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
   st TASK-CONSTRUCTED = if
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

\ CREATE/DOES> publishes a typed TCB address, outside checker inference.
\ Retirement owner: habu-typed-defining-words-aa224eb5.
TRUSTED: TASK ( n -- )
   dup TASK-CHECK-SIZE
   TASK-ALIGN8
   create
      ,  TASK-TCB-BYTES 8 / 1 - 0 do 0 , loop
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
\ Retirement owner: habu-typed-defining-words-aa224eb5.
TRUSTED: FACILITY ( -- )
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

: PAUSE ( -- )
   PAUSE ;

\ True while a TASK:HALT this task has not yet observed is pending. A STOP loop
\ reads it after every STOP, so a wait of its own can give its resources back
\ before the TASK:PAUSE that ends the task. The main thread has no TCB, is never
\ halted, and answers false.
: HALTED? ( -- bool )
   TASK-SELF-N dup 0= if drop false exit then
   TASK-N>PTR TASK-STOP@ 0 <> ;

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
\ thread - posts the main thread's park. A task that was never activated, one
\ that is only prepared and one that has ended are E-TASK-STATE.
: WAKE ( ptr n -- )
   TASK-WAKE ;

\ Requests the stop the target observes at its next TASK:PAUSE, and wakes it so
\ a task parked in TASK:STOP gets there. A task that has ended, one that was
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

\ One cleanup quotation for that task, run in the task's own thread when it ends
\ - body returned, body threw, or halted at TASK:PAUSE - before the join is
\ released. Registering again replaces it. A throw inside the cleanup becomes the
\ task's error if it has none and never leaves the task.
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

\ n is the initial count, 0..SEM_VALUE_MAX. Linux only.
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

private

get-current prot-wid-add

public

get-current prot-wid-add

;package
