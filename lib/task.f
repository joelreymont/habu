\ task.f - checked CPU tasking over pthread.

s" lib/errors.f" required
s" lib/memory.f" required
s" lib/ffi-abi.f" required
s" lib/type/deftype.f" required   \ DEFTYPE: the nominal semaphore handle
s" lib/image-lifecycle.f" required
s" lib/codegen.f" required        \ +USER builds its generated accessor with CODEGEN's buffer
require src/habu/task-abi.f
require src/habu/stack-abi.f

package TASK
public

\ The semaphore handle callers hold. A nominal cell, so a raw address - a
\ TASK:FACILITY, say, which is the same machine shape - cannot reach TASK:WAIT.
DEFTYPE SEM

private

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
   origin TCB.MSG-FULL origin TASK-ABI:MSG-FULL-OFF TASK-TCB-OFFSET ;

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

FFI:SCRATCH-END constant TASK-USER-BASE
TASK-USER-BASE TASK-USER-NEXT !

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

\ The handle of a semaphore record embedded in a larger record, which is how a
\ task's mailbox owns its two. The storage belongs to whoever holds the outer
\ record and its lifetime is that record's.
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

: TASK-LIVE+ ( -- )
   data-base TASKS-LIVE-CELL + dup @ 1 + swap ! ;

: TASK-LIVE- ( -- )
   data-base TASKS-LIVE-CELL + dup @ 1 - dup 0 < if drop 0 then swap ! ;

: TASK-MUNMAP-SPAN ( ptr n n -- )
   MUNMAP-CALL TASK-RC0 ;

: TASK-RELEASE-MEM ( ptr n -- ) {: tcb:ptr :}
   tcb MBOX-DESTROY
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
: TASK-RUNNER ( -- )
   TASK-RUN-USER dup 0= if drop exit then
   TASK-SELF TASK-THROW! ;

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

: PAUSE ( -- )
   TASK-SELF-N dup 0= if drop SCHED-YIELD-CALL TASK-RC0 exit then
   TASK-N>PTR dup TASK-STOP@ 0 <> if
         TASK-DONE over TASK-STATE!
         0 PTHREAD-EXIT-CALL
   then
   drop
   SCHED-YIELD-CALL TASK-RC0 ;

: HALT ( ptr n -- )
   TASK-HALT-REQ over TASK-STATE!
   1 swap TASK-STOP! ;

: TASK-KILL ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-STATE@ TASK-EMPTY = if exit then
   tcb TASK-STATE@ TASK-CONSTRUCTED = if
      tcb TASK-RELEASE-MEM
      TASK-EMPTY tcb TASK-STATE!
      exit
   then
   tcb TASK-STATE@ TASK-DONE = if
      tcb TASK-JOIN-RELEASE
      exit
   then
   tcb TASK-STATE@ TASK-RUNNING = tcb TASK-STATE@ TASK-HALT-REQ = or if
      tcb HALT
      tcb TASK-JOIN-RELEASE
   then ;

: TASK-DONE? ( ptr n -- bool )
   TASK-STATE@ TASK-DONE = ;

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
\ bounded by TXN-STATE-OFF, inside every region.

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
   off TXN-STATE-OFF > if E-TASK-USER throw then
   size 0 < if E-TASK-USER throw then
   size TXN-STATE-OFF off - > if E-TASK-USER throw then
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
\ in checked code, so the only address a caller ever sees is already a SEM. A
\ semaphore embedded in a larger record - a task's mailbox, a queue - is minted
\ by SEM-AT instead, and its owner owns its lifetime.
: SEMAPHORE ( -- )
   TASK-ALIGN8
   create TASK-SEMAPHORE-BYTES allot
   does> ( -- sem ) FFI:>CELL >SEM ;

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

: HALT ( ptr n -- )
   HALT ;

: KILL ( ptr n -- )
   TASK-KILL ;

: DONE? ( ptr n -- bool )
   TASK-DONE? ;

\ Zero until this task's body ends with an uncaught throw; cleared by ACTIVATE.
: THROW@ ( ptr n -- n )
   TASK-THROW@ ;

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

\ The bytes a record must reserve to embed one semaphore or one facility, for a
\ package that owns the storage and inits it through the words above.
TASK-SEMAPHORE-BYTES constant SEMAPHORE-BYTES
TASK-FACILITY-BYTES constant FACILITY-BYTES

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
