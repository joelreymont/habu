\ task.f - checked CPU tasking over pthread, package TASK: an xt runs on its own
\ thread, TCB, data stack and TASK:+USER region. THE MAIN THREAD COMPILES; TASKS
\ RUN COMPILED CODE. A definition, `allot`, `,`, `c,`, `align`, an
\ interpret-mode string literal that keeps its text, `evaluate`, a load and the
\ package keywords run on the main thread while tasks are live, and exit $4F on
\ a thread whose DATA holds a TCB in TASK-TCB-CELL: a task or a callback.
\ `cp!` and `ndict!` also exit $4F on main while TASKS-LIVE-CELL (the main
\ region's count, the AIO loop task included) is nonzero: lowering frees code a task may be running and
\ records it may reference. A task's DBASE, NDICT and CP are registers loaded
\ from its TCB as it starts, so a definition main has in progress is out of its
\ reach. A word main finishes later reaches a task only as an xt through a
\ channel that orders (a queue, an atomic cell, `is` on a defer):
\ main flushes the code before the xt exists, and each indirect entry
\ (`execute`, the catch family, a defer, a task's or a callback's start) runs
\ an ISB first, the first three only for an xt at or above its CP. Records
\ and defer cells change only by single aligned stores (`is`, a record's flags
\ or wordlist), so a task reads one old or new. A
\ `does>` given to a created word or a patch32 rewrites an instruction in
\ place: one a task is executing is outside the architecture's guarantee and
\ is not fenced.
\ LOOKUPS. A task's `search-wl`, `scope-find` and name lookups scan the records
\ below its NDICT linearly: its region's HIDXP-CELL stays 0, since only main's
\ appends build the hash index, so main's in-place compaction of the index
\ never meets a task's probe; every probe on main bounds each slot's index by
\ NDICT. A word published after a task started is not found by name in it. A
\ rollback while tasks are live retires each row, a package row included, by one
\ store of its wordlist cell, so a lookup that began before the store finds
\ only words whose code is kept.
\ TASK:NEW defines nothing. Task-local storage: a band in src/habu/layout.f for
\ a module the engine bakes, else TASK:+USER rows.
\ STORAGE CLASS. TASK-LOCAL: SLEEP's timespecs (a TASK:+USER row). PROCESS-WIDE:
\ semaphore pool, AT-EXIT rows, TCB chain and pool, callback rows; a CAS per move.
\
\ CLEANUPS. A task's cleanups are a chain of rows. Each quotation is stored AS a
\ quotation into storage declared to hold one: the checker's proven-quotation
\ store, never a cell cast back to code. A row's link holds the next row of the
\ chain plus one, and TCB.EXIT-SLOT the head row plus one; zero ends the chain
\ or means no cleanup. A row belongs to one (task, quotation) pair until
\ TASK:FREE gives the task back: a registration serves later activations and a
\ quotation never gets a second row, so a defined task's rows cannot leak.
\ TASK:FREE moves a NEW task's rows onto TASK-EXIT-FREE, which a registration
\ takes from first. A row's owed mark, 1 or 0, says its cleanup is owed a run
\ in this task's ending; every change to a row or its mark is made under
\ TASK-EXIT-LOCK. AT-EXIT of a quotation marks its row owed, taking a free row
\ when it has none. The cleanups run in the task's own thread, after the body
\ has returned or its throw has been recorded. The ending marks every row of
\ the chain owed, then clears the mark of the newest owed row and runs it,
\ until none is owed: a cleanup registered while they run runs in this ending,
\ one registered again after it ran runs again, and a registration runs once.
\ Every one of them runs, whatever the ones before it did. TASK:FREE leaves
\ each row it frees not owed.
\
\ CALLBACK ROWS. The engine's thunk claims a row before it reads the region,
\ so a region stays mapped while a row names it: TASK-END, the only word that
\ releases a named region, drops its rows first. A call that
\ meets BUSY waits in the thunk, so a mover holds BUSY only for steps that
\ call, wait on and throw nothing: every row word hands its rows back before
\ it throws. A declaration grows the row table before a task can run. No C
\ entry embeds a row address: the main region's CB-ROWS publishes the current
\ base for the thunk on every call.

s" src/lib/error.f" required
s" src/lib/memory.f" required
s" src/lib/ffi-abi.f" required
require src/lib/le.f
s" src/lib/adt/result.f" required
require src/core/quotation-storage.f
require src/habu/task-abi.f
require src/habu/stack-abi.f
require src/habu/boot-cells.f

package TASK
public

\ A nominal cell, so a TASK:FACILITY (the same machine shape) cannot reach
\ TASK:WAIT-SEM.
NEWTYPE sem

: PTHREAD-ENTRY ( -- n ) task-entry ;

private

\ Both converters stay private: no caller mints a handle over its own memory or
\ projects one back to an address.
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
$6E constant TASK-ETIMEDOUT              \ Linux
1 constant TASK-CLOCK-MONOTONIC          \ Linux clockid_t
0 constant TASK-SEM-GUARD-OFF
$8 constant TASK-SEM-OBJ-OFF
TASK-SEM-OBJ-OFF TASK-SEM-BYTES + constant TASK-SEMAPHORE-BYTES
$10 constant TASK-TIMESPEC-BYTES         \ struct timespec on an LP64 host
0 constant TASK-SPEC-SEC-OFF
$8 constant TASK-SPEC-NSEC-OFF
TASK-TIMESPEC-BYTES 2 * constant TASK-SLEEP-BYTES  \ the request and the remainder
1000000 constant TASK-NS-PER-MS
1000000000 constant TASK-NS-PER-S
$7FFFFFFFFFFFFFFF constant TASK-NS-MAX             \ the latest mono-ns deadline a cell holds
$FFFFFFFFFFFFFFF8 constant TASK-CELL-MASK

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
   \ The offsets the immutable engine entry loads, and the two this package
   \ reads as numbers.
   here CELL-VIEW {: origin:ptr :}
   origin TCB.XT-CELL origin TASK-ABI:XT-OFF TASK-TCB-OFFSET
   origin TCB.STACK origin TASK-ABI:STACK-OFF TASK-TCB-OFFSET
   origin TCB.REGION origin TASK-ABI:REGION-OFF TASK-TCB-OFFSET
   origin TCB.REGION-U origin TASK-ABI:REGION-U-OFF TASK-TCB-OFFSET
   origin TCB.DBASE origin TASK-ABI:DBASE-OFF TASK-TCB-OFFSET
   origin TCB.NDICT origin TASK-ABI:NDICT-OFF TASK-TCB-OFFSET
   origin TCB.CP origin TASK-ABI:CP-OFF TASK-TCB-OFFSET
   origin TCB.STATUS origin TASK-ABI:STATUS-OFF TASK-TCB-OFFSET
   origin TCB.PARK origin TASK-ABI:PARK-OFF TASK-TCB-OFFSET
   origin TCB.EXIT-SLOT origin TASK-ABI:EXIT-SLOT-OFF TASK-TCB-OFFSET ;

TASK-TCB-LAYOUT-CHECK

variable TASK-USER-NEXT

\ The band is this file's run of each region's header, with no engine cell.
USER-BAND:START constant TASK-USER-BASE
\ PAUSE precedes +USER's definer, so its one depth cell is the band's first
\ cell, not a slot.
TASK-USER-BASE constant TASK-DEFER-OFF

USER-BAND:END constant TASK-USER-END
TASK-USER-END CELL - constant TASK-AREA-OFF

: TASK-DEFER-DEPTH ( -- ptr n )
   data-base TASK-DEFER-OFF + ;

: TASK-DEFER-ENTER ( -- )
   1 TASK-DEFER-DEPTH +! ;

: TASK-DEFER-LEAVE ( -- )
   -1 TASK-DEFER-DEPTH +! ;

: TASK-NULL ( -- ptr n )
   NULL$ drop CELL-VIEW ;

\ An address this module keeps as a number: a TCB, DATA or a semaphore record.
CAST: TASK-N>PTR ( n -- ptr n )

CAST: TASK-CELL>PTR-SLOT ( ptr n -- ptr ptr n )

\ TCB.XT, TCB.USER-XT and an AT-EXIT row's EXIT-QT are quotation slots of the
\ fixed pthread entry ABI; every store and load uses the declared empty effect.
CAST: TASK-CELL>XT-SLOT ( ptr n -- ptr [ -- ] )

: TCB.XT ( ptr n -- ptr [ -- ] )
   TCB.XT-CELL TASK-CELL>XT-SLOT ;

: TCB.USER-XT ( ptr n -- ptr [ -- ] )
   TCB.USER-XT-CELL TASK-CELL>XT-SLOT ;

: TASK-ALIGN8 ( -- )
   here FFI:>CELL 7 and dup 0= if drop exit then
   8 swap - allot ;

\ ---- host semaphore bindings ----------------------------------------
\ sem_wait blocks inside the call; that is safe because the generated body
\ stages its argument tables in the calling task's own DATA region.
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

FUNCTION: SEM-TRYWAIT-CALL sem_trywait ( ptr u8 -- i32 )
   0 TASK-SEM-BYTES WRITES-BYTES
;FUNCTION

\ sem_clockwait (glibc 2.30) takes an absolute deadline on the named clock and
\ only reads it; clock_gettime writes its timespec.
FUNCTION: SEM-CLOCKWAIT-CALL sem_clockwait ( ptr u8 n ptr u8 -- i32 )
   0 TASK-SEM-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: CLOCK-GETTIME-CALL clock_gettime ( n ptr u8 -- i32 )
   1 TASK-TIMESPEC-BYTES WRITES-BYTES
;FUNCTION

\ Darwin's unnamed POSIX semaphore entry points are ENOSYS stubs. A Mach
\ semaphore is a process-owned port name stored in the same record. Its wait
\ result is a kernel return code, never errno. semaphore_timedwait takes a
\ relative mach_timespec_t { unsigned tv_sec; int tv_nsec } by value, one
\ register with tv_sec in the low half; a zero timespec tries once.
14 constant MACH-ABORTED
49 constant MACH-TIMED-OUT
0 constant MACH-FIFO
$FFFFFFFF constant MACH-SEC-MAX
FUNCTION: MACH-TASK task_self_trap ( -- u32 ) ;FUNCTION
FUNCTION: MACH-SEM-CREATE semaphore_create ( n ptr u8 n n -- i32 )
   1 4 WRITES-BYTES
;FUNCTION
FUNCTION: MACH-SEM-DESTROY semaphore_destroy ( n n -- i32 ) ;FUNCTION
FUNCTION: MACH-SEM-WAIT semaphore_wait ( n -- i32 ) ;FUNCTION
FUNCTION: MACH-SEM-SIGNAL semaphore_signal ( n -- i32 ) ;FUNCTION
FUNCTION: MACH-SEM-TIMEDWAIT semaphore_timedwait ( n n -- i32 ) ;FUNCTION

\ nanosleep only reads the request and writes the remainder when a signal cuts
\ the sleep short, so only the second argument states an extent.
FUNCTION: NANOSLEEP-CALL nanosleep ( ptr u8 ptr u8 -- i32 )
   1 TASK-TIMESPEC-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: SCHED-YIELD-CALL sched_yield ( -- i32 ) ;FUNCTION

\ munmap only reads the address it is given.
FUNCTION: MUNMAP-CALL munmap ( ptr u8 n -- i32 ) ;FUNCTION

\ pthread_create writes the pthread_t into the TCB's THREAD cell and passes the
\ TCB to the entry (TASK-ABI's code address, null attribute); pthread_join
\ writes the thread's exit value into the TCB's RET cell.
FUNCTION: PTHREAD-CREATE-CALL pthread_create ( ptr u8 n n ptr u8 -- i32 )
   0 8 WRITES-BYTES
;FUNCTION

FUNCTION: PTHREAD-JOIN-CALL pthread_join ( n ptr u8 -- i32 )
   1 8 WRITES-BYTES
;FUNCTION

\ A FACILITY's lock: one pthread_mutex_t of TASK-MUTEX-BYTES in its record,
\ which each call may write.
FUNCTION: MUTEX-LOCK-CALL pthread_mutex_lock ( ptr u8 -- i32 )
   0 TASK-MUTEX-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: MUTEX-UNLOCK-CALL pthread_mutex_unlock ( ptr u8 -- i32 )
   0 TASK-MUTEX-BYTES WRITES-BYTES
;FUNCTION

\ pthread_key_create writes the key into the cell, 8 bytes on macOS and 4 on
\ Linux; the second argument is the destructor, 0 for none.
FUNCTION: KEY-CREATE-CALL pthread_key_create ( ptr u8 n -- i32 )
   0 8 WRITES-BYTES
;FUNCTION

FUNCTION: KEY-DELETE-CALL pthread_key_delete ( n -- i32 ) ;FUNCTION
FUNCTION: SETSPECIFIC-CALL pthread_setspecific ( n n -- i32 ) ;FUNCTION
FUNCTION: GETSPECIFIC-CALL pthread_getspecific ( n -- n ) ;FUNCTION

: TASK-RC0 ( n -- )
   dup 0 <> if E-TASK-THREAD throw then
   drop ;

: TASK-CHECK-SIZE ( n -- )
   dup TASK-MIN-STACK < if E-TASK-SIZE throw then
   drop ;

\ Owners read the status cell while the worker moves it, so this is an acquire
\ load. The entry's STLR release-stores DONE, so an owner that polls DONE?
\ sees every write the body made before it ended.
: TASK-STATE@ ( ptr n -- n )
   TCB.STATUS atomic@ ;

\ Owner-only: PREPARE and ACTIVATE store before pthread_create orders the store
\ into the thread, TASK-KILL on a task with no thread. A worker publishes DONE,
\ and TASK-JOIN-RELEASE EMPTY, with a release store.
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

\ The main region, whichever task asks: DATA is mapped at the fixed DATA-VA.
\ A cross build's process is not mapped there, so the word is the target's.
target
: MAIN-BASE ( -- ptr n )
   DATA-VA VA>N TASK-N>PTR ;
host

: SEM-REC ( sem -- ptr n )
   SEM>N TASK-N>PTR ;

\ A record this package owns but did not define: a task's TCB holds its park
\ and done semaphores and the pool below holds a fixed row.
: SEM-AT ( ptr n -- sem )
   FFI:>CELL >SEM ;

: SEM-GUARD ( sem -- ptr n )
   SEM-REC TASK-SEM-GUARD-OFF + ;

: SEM-OBJ ( sem -- ptr u8 )
   SEM-REC BYTE-VIEW TASK-SEM-OBJ-OFF + ;

\ The guard cell holds the record's OWN
\ address exactly while the POSIX object behind it is live.
: SEM-LIVE? ( sem -- bool ) {: s:sem :}
   s SEM-GUARD atomic@ s SEM>N = ;

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
\ so the caller drains its waiters first.
: SEM-DESTROY ( sem -- ) {: s:sem :}
   s SEM-LIVE? 0= if exit then
   0 s SEM-GUARD atomic!
   HB-TARGET-MACOS? if
      MACH-TASK s SEM-OBJ LE:U32@ MACH-SEM-DESTROY TASK-RC0
      0 s SEM-OBJ LE:U32!
   else s SEM-OBJ SEM-DESTROY-CALL TASK-RC0 then ;

\ Observes no TASK:HALT until signalled. A signal interrupts the wait without
\ consuming a count, so an aborted wait retries.
: MACH-WAIT ( sem -- ) {: s:sem :}
   begin
      s SEM-OBJ LE:U32@ MACH-SEM-WAIT
      dup 0= if drop exit then
      MACH-ABORTED <> if E-TASK-THREAD throw then
   again ;

: SEM-WAIT ( sem -- ) {: s:sem :}
   HB-TARGET-MACOS? if s MACH-WAIT exit then
   begin
      s SEM-OBJ SEM-WAIT-CALL 0= if exit then
      FFI:ERRNO TASK-EINTR <> if E-TASK-THREAD throw then
   again ;

: SEM-SIGNAL ( sem -- ) {: s:sem :}
   HB-TARGET-MACOS? if s SEM-OBJ LE:U32@ MACH-SEM-SIGNAL
   else s SEM-OBJ SEM-POST-CALL then TASK-RC0 ;

\ True when it took a count. A timeout means the count was zero; an aborted
\ wait retries as MACH-WAIT does.
: MACH-TRY-WAIT ( sem -- bool ) {: s:sem :}
   begin
      s SEM-OBJ LE:U32@ 0 MACH-SEM-TIMEDWAIT
      dup 0= if drop true exit then
      dup MACH-TIMED-OUT = if drop false exit then
      MACH-ABORTED <> if E-TASK-THREAD throw then
   again ;

: SEM-TRY-WAIT ( sem -- bool ) {: s:sem :}
   HB-TARGET-MACOS? if s MACH-TRY-WAIT exit then
   begin
      s SEM-OBJ SEM-TRYWAIT-CALL 0= if 0 0= exit then
      FFI:ERRNO TASK-EAGAIN = if 0 0= 0= exit then
      FFI:ERRNO TASK-EINTR <> if E-TASK-THREAD throw then
   again ;

\ ---- the semaphore pool ------------------------------------------------------
\ The records live in chunks TASK owns, so a pooled handle still names a TASK
\ record. A chunk is a link, TASK-SEM-POOL-N used cells and as many records. The
\ first chunk is static; later ones are mapped and never move, since a handle is
\ the record's address. A freed record is reused before the pool grows. The
\ first chunk is scratch, so a program starts with every record free and no
\ chunk mapped.
$40 constant TASK-SEM-POOL-N
1 cells constant TASK-SEM-CHUNK-USED-OFF
TASK-SEM-POOL-N 1 + cells constant TASK-SEM-CHUNK-RECS-OFF
TASK-SEM-CHUNK-RECS-OFF TASK-SEM-POOL-N TASK-SEMAPHORE-BYTES * +
   constant TASK-SEM-CHUNK-BYTES

: TASK-ZERO-CELLS, ( n -- )
   0 ?do 0 , loop ;

TASK-ALIGN8
create TASK-SEM-POOL TASK-SEM-CHUNK-BYTES SCRATCH-ALLOT

\ Chunks are named by address, as n; 0 ends the chain.
: SEM-POOL-FIRST ( -- n )
   TASK-SEM-POOL CELL-VIEW FFI:>CELL ;

: SEM-CHUNK-LINK ( n -- ptr n )
   TASK-N>PTR ;

: SEM-CHUNK-NEXT ( n -- n )
   SEM-CHUNK-LINK atomic@ ;

: SEM-CHUNK-USED ( n n -- ptr n ) {: c:n idx:n :}
   c TASK-SEM-CHUNK-USED-OFF + idx cells + TASK-N>PTR ;

: SEM-CHUNK-REC ( n n -- n ) {: c:n idx:n :}
   c TASK-SEM-CHUNK-RECS-OFF + idx TASK-SEMAPHORE-BYTES * + ;

\ The used cell moves 0 -> 1 in one step, so racing claimers get different
\ records. Answers the record, or 0 when the chunk has none free.
: SEM-CHUNK-CLAIM ( n -- n ) {: c:n :}
   TASK-SEM-POOL-N 0 ?do
      0 1 c i SEM-CHUNK-USED atomic-cas 0= if c i SEM-CHUNK-REC unloop exit then
   loop
   0 ;

\ The used cell of the record that handle names, or 0 for a handle from
\ anywhere else: a defined semaphore and a task's own are not the pool's to
\ hand back out.
: SEM-CHUNK-SLOT ( sem n -- n ) {: s:sem c:n :}
   s SEM>N c 0 SEM-CHUNK-REC - {: off:n :}
   off 0 < if 0 exit then
   off TASK-SEM-POOL-N TASK-SEMAPHORE-BYTES * >= if 0 exit then
   off TASK-SEMAPHORE-BYTES mod 0 <> if 0 exit then
   c off TASK-SEMAPHORE-BYTES / SEM-CHUNK-USED FFI:>CELL ;

: SEM-POOL-SLOT ( sem -- n ) {: s:sem :}
   SEM-POOL-FIRST
   begin dup 0<> while
      s over SEM-CHUNK-SLOT dup 0<> if nip exit then drop
      SEM-CHUNK-NEXT
   repeat ;

\ Two tasks growing at once each append their own chunk; the loser walks on to
\ the new last.
: SEM-CHUNK-APPEND ( n -- ) {: c:n :}
   SEM-POOL-FIRST
   begin
      dup 0 c rot SEM-CHUNK-LINK atomic-cas
      dup 0<>
   while nip repeat
   2drop ;

\ A fresh mapping is zero: no record used, none live. Its first record is the
\ caller's before the chunk is published.
: SEM-GROW ( -- n )
   TASK-SEM-CHUNK-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop FFI:>CELL {: c:n :}
   1 c 0 SEM-CHUNK-USED !
   c SEM-CHUNK-APPEND
   c 0 SEM-CHUNK-REC ;

: SEM-NEW ( -- sem )
   SEM-POOL-FIRST
   begin dup 0<> while
      dup SEM-CHUNK-CLAIM dup 0<> if nip >SEM exit then drop
      SEM-CHUNK-NEXT
   repeat
   drop SEM-GROW >SEM ;

\ Destroys the semaphore if it is still live, so a recycled record never carries
\ a POSIX object into its next owner; the caller has already ended its waiters.
: SEM-FREE ( sem -- ) {: s:sem :}
   s SEM-POOL-SLOT {: slot:n :}
   slot 0= if E-TASK-SEM-POOL throw then
   s SEM-DESTROY
   0 slot TASK-N>PTR atomic! ;

: TASK-PARK-SEM ( ptr n -- sem )
   TCB.PARK SEM-AT ;

\ The park outlives every release of its TCB, TASK:FREE included: a post from
\ any thread (TASK-WAKE, HALT) is ordered before no KILL, JOIN or ACTIVATE.
: PARK-CREATE ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-PARK-SEM SEM-LIVE? if exit then
   0 tcb TASK-PARK-SEM SEM-INIT ;

\ A run opens at zero while the record stays live under any post in flight.
: PARK-DRAIN ( ptr n -- ) {: tcb:ptr :}
   begin tcb TASK-PARK-SEM SEM-TRY-WAIT while repeat ;

\ The main thread has no TCB, so a WAKE of the null TCB posts this record. It
\ and READY are scratch, so a program starts with no park and its first wake
\ makes one.
TASK-ALIGN8
create MAIN-PARK-REC TASK-SEMAPHORE-BYTES SCRATCH-ALLOT
TASK-ALIGN8
SCRATCH-VARIABLE MAIN-PARK-READY

: MAIN-PARK ( -- sem )
   MAIN-PARK-REC CELL-VIEW SEM-AT ;

\ The semaphore, Darwin's Mach port included, belongs to this process. A failed
\ MAIN-PARK-CREATE gives it back here, so the next wake makes the park again.
: MAIN-PARK-RESET ( -- )
   MAIN-PARK SEM-DESTROY
   0 MAIN-PARK-READY atomic! ;

\ A process just forked forgets the park and never destroys it: a park another
\ task was making holds MAIN-PARK-READY at 1 with no thread left to finish it,
\ and a made one is the parent's - on Darwin a Mach port name, which fork does
\ not copy, so the child's first wake would throw E-TASK-THREAD. The child's
\ first wake makes a park of its own.
: MAIN-PARK-FORGET ( -- )
   0 MAIN-PARK SEM-GUARD atomic!
   0 MAIN-PARK-READY atomic! ;

: MAIN-PARK-CREATE ( -- )
   0 MAIN-PARK SEM-INIT ;

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

: PARK-SELF ( -- sem )
   TASK-SELF-N dup 0= if
      drop MAIN-PARK-INIT MAIN-PARK exit
   then
   TASK-N>PTR TASK-PARK-SEM ;

\ Observes no TASK:HALT until something wakes it, which is why TASK:HALT wakes
\ its target itself.
: TASK-STOP ( -- )
   PARK-SELF SEM-WAIT ;

\ A task never activated, only constructed, or ended has no run to hint at: the
\ count would sit in a record its next activation is not entitled to, so all
\ three are refused. The task can end, be released and open again between the
\ state read and the post; the post still meets this TCB's one live park. A
\ hint before the next run opens is drained there; a WAKE in flight as it opens
\ lands in it, the one stale hint its re-check absorbs.
: TASK-WAKE ( ptr n -- ) {: tcb:ptr :}
   tcb FFI:>CELL 0= if MAIN-PARK-INIT MAIN-PARK SEM-SIGNAL exit then
   tcb TASK-STATE@ {: st:n :}
   st TASK-RUNNING <> st TASK-HALT-REQ <> and if E-TASK-STATE throw then
   tcb TASK-PARK-SEM SEM-SIGNAL ;

\ ---- the task's outcome ------------------------------------------------------
\ The outcome lives in the TCB, which outlives the thread and its memory, so
\ the answer survives the join's release, as TASK:THROW@ does after TASK:KILL.

\ THE ROWS GROW WITH THE TASKS THAT USE THEM. The first TASK-EXIT-TABLE-N rows
\ are this package's storage: TASK-EXIT-QT and TASK-EXIT-LINK, which a program
\ carries, and TASK-EXIT-OWED, scratch. Past them a registration maps a chunk of
\ EXIT-CHUNK-ROWS more, the growth unit and not a cap: the next chunk's address,
\ the index of its first row, then rows of a quotation cell, a link cell and an
\ owed cell. Rows never move while the process runs, so an index a chain holds
\ stays valid under a reader.
$80 constant TASK-EXIT-TABLE-N
TASK-EXIT-TABLE-N TYPED-BUFFER TASK-EXIT-QT [ -- ]
TASK-ALIGN8
create TASK-EXIT-LINK TASK-EXIT-TABLE-N TASK-ZERO-CELLS,
TASK-ALIGN8
create TASK-EXIT-OWED TASK-EXIT-TABLE-N cells SCRATCH-ALLOT
\ The first row never handed out: every row below it has been used, freed
\ rows included.
TASK-ALIGN8
variable TASK-EXIT-N
\ The first row TASK:FREE gave back, plus one; 0 is none. A free row's link
\ names the next free row the same way.
TASK-ALIGN8
variable TASK-EXIT-FREE
TASK-ALIGN8
SCRATCH-VARIABLE TASK-EXIT-LOCK
\ The newest chunk, as a number; 0 is none. Declared an address cell, so the
\ linker refuses a file whose top level leaves a chunk mapped.
TASK-ALIGN8
variable EXIT-CHUNKS   EXIT-CHUNKS ptr-cell-mark
$4000 constant EXIT-CHUNK-BYTES
3 cells constant EXIT-ROW-BYTES
2 cells constant EXIT-CHUNK-HEAD
EXIT-CHUNK-BYTES EXIT-CHUNK-HEAD - EXIT-ROW-BYTES / constant EXIT-CHUNK-ROWS

\ The one cell a registration compares against. The checker refuses `=` on two
\ quotations (E-MISMATCH, "expected n n actual [ -- ] [ -- ]"), but a quotation
\ in declared storage reads as a cell through the views, and the same `['] W`
\ stored twice reads the same cell.
TYPED-VARIABLE TASK-EXIT-SCRATCH [ -- ]

: TASK-DONE-SEM ( ptr n -- sem )
   TCB.DONE SEM-AT ;

\ Created with the task and destroyed with its memory: a task never activated has nothing to signal, and a released task's record is gone
\ before the next PREPARE opens a fresh one.
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

\ Read and written under TASK-EXIT-LOCK, which orders the head and its rows,
\ but by TCB-CLEAR, where no other task reaches the record.
: TASK-EXIT-SLOT@ ( ptr n -- n )
   TCB.EXIT-SLOT @ ;

: EXIT-CHUNK-FIRST ( n -- n )
   CELL + TASK-N>PTR @ ;

: EXIT-ROWS-END ( -- n )
   EXIT-CHUNKS atomic@ dup 0= if drop TASK-EXIT-TABLE-N exit then
   EXIT-CHUNK-FIRST EXIT-CHUNK-ROWS + ;

\ The address of a row past the table. Chunks are newest first and each starts
\ where the one before it ended, so the first chunk starting at or below the
\ index holds it.
: EXIT-ROW-AT ( n -- n ) {: idx:n :}
   EXIT-CHUNKS atomic@
   begin dup EXIT-CHUNK-FIRST idx > while TASK-N>PTR @ repeat
   {: c:n :}
   c EXIT-CHUNK-HEAD + idx c EXIT-CHUNK-FIRST - EXIT-ROW-BYTES * + ;

: TASK-EXIT-LINK-AT ( n -- ptr n ) {: idx:n :}
   idx TASK-EXIT-TABLE-N < if TASK-EXIT-LINK CELL-VIEW idx cells + exit then
   idx EXIT-ROW-AT CELL + TASK-N>PTR ;

: EXIT-OWED-AT ( n -- ptr n ) {: idx:n :}
   idx TASK-EXIT-TABLE-N < if TASK-EXIT-OWED CELL-VIEW idx cells + exit then
   idx EXIT-ROW-AT 2 cells + TASK-N>PTR ;

: EXIT-OWE ( n -- )
   1 swap EXIT-OWED-AT ! ;

: EXIT-PAY ( n -- )
   0 swap EXIT-OWED-AT ! ;

: EXIT-OWED? ( n -- bool )
   EXIT-OWED-AT @ 0 <> ;

: EXIT-QT ( n -- ptr [ -- ] ) {: idx:n :}
   idx TASK-EXIT-TABLE-N < if idx TASK-EXIT-QT exit then
   idx EXIT-ROW-AT TASK-N>PTR TASK-CELL>XT-SLOT ;

: TASK-EXIT-QT-CELL ( n -- n )
   EXIT-QT BYTE-VIEW CELL-VIEW @ ;

\ Registration, growth, giving rows back, the owed marks and the scratch cell
\ above: TASK-RUN-EXIT reads its chain under it and runs a cleanup outside it.
: TASK-EXIT-LOCK-GET ( -- )
   begin 0 1 TASK-EXIT-LOCK atomic-cas 0= until ;

: TASK-EXIT-UNLOCK ( -- )
   0 TASK-EXIT-LOCK atomic! ;

\ The caller holds the lock, so the row it is handed is its own.
: EXIT-ROW-POP ( -- n )
   TASK-EXIT-FREE @ dup 0 <> if
      1 - dup TASK-EXIT-LINK-AT @ TASK-EXIT-FREE ! exit
   then
   drop
   TASK-EXIT-N @ dup 1 + TASK-EXIT-N atomic! ;

\ A row no chain holds, and true; false when every row the table and the chunks
\ hold is in a chain, the caller's cue to grow. The count is checked before it
\ moves, so TASK-EXIT-N never names a row past the storage.
: TASK-EXIT-NEXT ( -- n bool )
   TASK-EXIT-FREE @ 0= TASK-EXIT-N @ EXIT-ROWS-END >= and if 0 false exit then
   EXIT-ROW-POP true ;

\ The row of the task's chain that holds the scratch quotation, and true; false
\ when none does.
: TASK-EXIT-HELD ( ptr n -- n bool ) {: tcb:ptr :}
   TASK-EXIT-SCRATCH BYTE-VIEW CELL-VIEW @ {: want:n :}
   tcb TASK-EXIT-SLOT@
   begin dup 0 <> while
      1 -
      dup TASK-EXIT-QT-CELL want = if true exit then
      TASK-EXIT-LINK-AT @
   repeat
   false ;

\ Gives every row of the task's chain back to TASK-EXIT-FREE, for TASK:FREE. The
\ task has ended and been released, so no thread runs the chain; the lock
\ orders this against a registration on any task. A freed row is not owed and
\ holds the empty quotation: the link reads every address cell of a reached
\ file, so a cleanup left in a free row below TASK-EXIT-N would stay in the
\ program.
: EXIT-ROWS-FREE ( ptr n -- ) {: tcb:ptr :}
   TASK-EXIT-LOCK-GET
   tcb TASK-EXIT-SLOT@
   begin dup 0 <> while
      1 -
      [: ;] over EXIT-QT !
      dup EXIT-PAY
      dup TASK-EXIT-LINK-AT @ swap
      TASK-EXIT-FREE @ over TASK-EXIT-LINK-AT !
      1 + TASK-EXIT-FREE !
   repeat
   drop
   0 tcb TCB.EXIT-SLOT !
   TASK-EXIT-UNLOCK ;

\ A cleanup that throws ends nothing else: its code becomes the task's error when
\ the body left none, and is dropped when the body already failed or an earlier
\ cleanup already threw, so the FIRST failure is the one the join reports.
: TASK-RUN-EXIT-ONE ( n -- )
   EXIT-QT @ catch {: rc:n :}
   rc 0= if exit then
   TASK-SELF {: self:ptr :}
   self TASK-THROW@ 0 <> if exit then
   rc self TASK-THROW! ;

: EXIT-OWE-ALL ( ptr n -- ) {: tcb:ptr :}
   TASK-EXIT-LOCK-GET
   tcb TASK-EXIT-SLOT@
   begin dup 0 <> while
      1 - dup EXIT-OWE
      TASK-EXIT-LINK-AT @
   repeat
   drop
   TASK-EXIT-UNLOCK ;

\ The newest row of the task's chain still owed, its mark cleared, and true;
\ false when no row is owed.
: EXIT-TAKE ( ptr n -- n bool ) {: tcb:ptr :}
   TASK-EXIT-LOCK-GET
   tcb TASK-EXIT-SLOT@
   begin dup 0 <> while
      1 -
      dup EXIT-OWED? if dup EXIT-PAY TASK-EXIT-UNLOCK true exit then
      TASK-EXIT-LINK-AT @
   repeat
   TASK-EXIT-UNLOCK false ;

: TASK-RUN-EXIT ( -- )
   TASK-SELF {: self:ptr :}
   self EXIT-OWE-ALL
   begin self EXIT-TAKE while TASK-RUN-EXIT-ONE repeat
   drop ;


\ The pthread key whose value on a registered thread is that thread's region,
\ plus one; 0 before this process made one.
SCRATCH-VARIABLE CALLER-KEY

: KEY-RC0 ( n -- )
   0 <> if E-TASK-STATE throw then ;


\ The cell is zeroed first, so its 8 bytes read the key at either width.
: KEY-NEW ( ptr u8 NUM:alloc-byte-len -- n )
   drop {: cell:ptr :}
   0 cell CELL-VIEW !
   cell 0 KEY-CREATE-CALL KEY-RC0
   cell CELL-VIEW @ ;


: KEY-ENSURE ( -- n )
   CALLER-KEY atomic@ {: held:n :}
   held 0<> if held 1 - exit then
   1 cells MEM:BYTES-ALLOC-LEN [: KEY-NEW ;] MEM:WITH-BYTES {: key:n :}
   0 key 1 + CALLER-KEY atomic-cas {: seen:n :}
   seen 0= if key exit then
   key KEY-DELETE-CALL KEY-RC0
   seen 1 - ;


\ Once: a task's thread and the main thread keep one region for life.
: CALLER-REGISTER ( n -- ) {: key:n :}
   key GETSPECIFIC-CALL 0<> if exit then
   key data-base FFI:>CELL SETSPECIFIC-CALL KEY-RC0 ;


\ A row names the region a C call through its slot enters (CB-ROW-REGION, 0
\ unbound or bound to the caller's context), who is inside through it
\ (CB-ROW-OWNER) and the count of region stores. A caller row's region is 0, so
\ ROW-END never matches it.
\ The rows are scratch, so a program starts with every row unbound and idle.
2 cells constant ROW-MOVES-OFF
PERSISTED-PTR-VARIABLE ROW-TABLE
variable ROW-CAP
variable ROW-N

: ROW-ALLOC ( n -- ptr u8 ) {: size:n :}
   TASK-ALIGN8
   here BYTE-VIEW {: rows:ptr :}
   size SCRATCH-ALLOT
   rows ;

: ROW-GROW ( -- )
   data-base TASKS-LIVE-CELL + atomic@ 0<> if E-TASK-STATE throw then
   data-base CB-FRAME + @ 0<> if E-TASK-STATE throw then
   ROW-TABLE @ {: old:ptr :}
   ROW-CAP @ {: oldcap:n :}
   oldcap dup 0= if drop 1 else 2 * then {: cap:n :}
   cap CB-ROW-BYTES * ROW-ALLOC {: rows:ptr :}
   ROW-N @ 0 > if old rows ROW-N @ CB-ROW-BYTES * BYTE-COPY then
   rows ROW-TABLE !
   cap ROW-CAP !
   rows FFI:>CELL data-base CB-ROWS + atomic!
   \ No task or callback is running. After publication the former rows are
   \ unreachable, including their per-process region-store counters.
   oldcap CB-ROW-BYTES * CELL / 0 ?do 0 old i cells + CELL-VIEW ! loop ;

: ROW-DECLARE ( n -- ) {: k:n :}
   k ROW-N @ <> if E-TASK-STATE throw then
   k ROW-CAP @ >= if ROW-GROW then
   1 ROW-N +! ;

: ROW ( n -- ptr n )
   CB-ROW-BYTES * ROW-TABLE @ + CELL-VIEW ;

: ROW-REGION ( ptr n -- ptr n )
   CB-ROW-REGION + ;

: ROW-OWNER ( ptr n -- ptr n )
   CB-ROW-OWNER + ;

: ROW-MOVES ( n -- ptr n )
   ROW ROW-MOVES-OFF + ;

\ Names region r in row k, then counts the store, each a release store. Every
\ region store goes through here, by a mover that holds the row BUSY and hands
\ it back only after both.
: ROW-NAME! ( n n -- ) {: r:n k:n :}
   r k ROW ROW-REGION atomic!
   k ROW-MOVES atomic@ 1 + k ROW-MOVES atomic! ;

: ROW-NAMES? ( n n -- bool ) {: k:n reg:n :}
   k ROW ROW-REGION atomic@ reg = ;

\ Whether the thread a refused claim saw is inside row k by region reg. A
\ thread value comes only from that thread's own claim, and the thread entered
\ by the region the row named then, which nothing moves until it leaves: bind,
\ unbind and another task's end all take the row from 0, and the thunk only
\ reads the region.
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
\ again.
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
   ROW-N @ 0 ?do i reg ROW-END loop ;

\ Stop mark first, so a cleanup's PAUSE only yields (TCB-HALT-PENDING?); signal
\ last, so a woken joiner finds cleanups run, outcome final, no row on its region.
: TASK-END ( -- )
   1 TASK-SELF TCB.STOP !
   TASK-RUN-EXIT
   data-base FFI:>CELL ROWS-END
   TASK-SELF TASK-DONE-SEM SEM-SIGNAL ;

\ No clamp below zero: a negative count is a pairing defect, which must
\ surface as $4F.
: TASK-LIVE+ ( -- )
   1 MAIN-BASE TASKS-LIVE-CELL + atomic-add drop ;

: TASK-LIVE- ( -- )
   -1 MAIN-BASE TASKS-LIVE-CELL + atomic-add drop ;

: TASK-MUNMAP-SPAN ( ptr u8 n -- )
   MUNMAP-CALL TASK-RC0 ;

\ AN EMPTY TCB HOLDS NOTHING THIS PROCESS TOOK BUT ITS PARK, which outlives every
\ release (PARK-CREATE). The five cells cleared last are what the run
\ acquired; clearing them changes no run: ACTIVATE writes THREAD again, the
\ join writes RET, and ACTIVATE writes DBASE, NDICT and CP.
: TASK-RELEASE-MEM ( ptr n -- ) {: tcb:ptr :}
   tcb DONE-DESTROY
   tcb TCB.STACK-U @ 0 <> if
      tcb TCB.STACK @ tcb TCB.STACK-U @ MEM:RELEASE-GUARDED
      TASK-NULL tcb TCB.STACK !
      0 tcb TCB.STACK-U !
   then
   tcb TCB.RSTACK-U @ 0 <> if
      tcb TCB.RSTACK @ tcb TCB.RSTACK-U @ MEM:RELEASE-GUARDED
      TASK-NULL tcb TCB.RSTACK !
      0 tcb TCB.RSTACK-U !
   then
   tcb TCB.LSTACK-U @ 0 <> if
      tcb TCB.LSTACK @ tcb TCB.LSTACK-U @ MEM:RELEASE-GUARDED
      TASK-NULL tcb TCB.LSTACK !
      0 tcb TCB.LSTACK-U !
   then
   tcb TCB.REGION-U @ 0 <> if
      tcb TCB.REGION @ BYTE-VIEW MEM:RELEASE-SCOPES
      tcb TCB.REGION @ BYTE-VIEW tcb TCB.REGION-U @ TASK-MUNMAP-SPAN
      TASK-NULL tcb TCB.REGION !
      0 tcb TCB.REGION-U !
   then
   0 tcb TCB.THREAD !
   0 tcb TCB.RET !
   0 tcb TCB.DBASE !
   0 tcb TCB.NDICT !
   0 tcb TCB.CP ! ;

\ ---- every declared TCB ------------------------------------------------------
\ The head and each link (TASK-LINK@) are declared address cells; 0 ends. The
\ chain only grows, read unlocked, so a TASK:NEW record stays on TCB-POOL.
PERSISTED-PTR-VARIABLE TASK-CHAIN

\ The crash handler reads this chain after an instruction fetch faults in a
\ task's guard, where task-local RBP is unavailable, so the head is published in
\ root DATA, declared a DATA pointer so an image remaps it with the window.
: TASK-CHAIN-PUBLISH ( -- )
   TASK-CHAIN data-base TASK-CHAIN-CELL + 0 ptr-field ptr-cell! ;
TASK-CHAIN-PUBLISH

: TASK-CHAIN@ ( -- ptr n )
   TASK-CHAIN @ ;

: TASK-CHAIN! ( ptr n -- )
   TASK-CHAIN ! ;

\ Nothing after the TCB belongs to TASK-ABI, so the layout check above answers
\ for the structure unchanged.
: TASK-LINK@ ( ptr n -- n )
   TASK-TCB-BYTES + 0 ptr-field @ FFI:>CELL ;

\ A record is a TCB, its pool link where a chain link sits (the next record as a
\ number, 0 ends), and its claim: CLAIM-FREE, CLAIM-HELD while TASK:NEW has
\ handed it out, CLAIM-FREEING while a TASK:FREE moves it back. A chunk is the
\ link to the next chunk and TCB-POOL-N records, the growth unit and not a cap.
\ THE POOL ONLY GROWS while the process runs: TASK:FREE hands a record back by
\ its claim and TASK:NEW takes a free record before it maps another chunk, so
\ nothing is unlinked or unmapped under TCB-POOLED?, which walks it with no
\ lock, and a WAKE or HALT that read a TCB before TASK:FREE still
\ posts a live park. The heads are scratch, so a program starts with an empty
\ pool. The x86-64 crash handler walks TASK-CHAIN only, so
\ a guard-page fetch in a NEW task's stack reaches its generic dump instead of
\ the stack-bounds report.
TASK-ALIGN8
SCRATCH-VARIABLE TCB-POOL
TASK-ALIGN8
SCRATCH-VARIABLE TCB-CHUNKS
TASK-TCB-BYTES 2 cells + constant TCB-REC-BYTES
$4000 constant TCB-CHUNK-BYTES
TCB-CHUNK-BYTES CELL - TCB-REC-BYTES / constant TCB-POOL-N
0 constant CLAIM-FREE
1 constant CLAIM-HELD
2 constant CLAIM-FREEING

: TCB-REC ( n n -- n ) {: c:n idx:n :}
   c CELL + idx TCB-REC-BYTES * + ;

: TCB-POOL-LINK ( n -- ptr n )
   TASK-TCB-BYTES + TASK-N>PTR ;

: TCB-CLAIM ( ptr n -- ptr n )
   TASK-TCB-BYTES + CELL + ;

\ Maps outside the lock, since a mapping may block or throw, and appends under
\ it: the chunk starts where the rows end, and its header (first row index, link
\ to the chunk before) is stored before the release store that publishes it to
\ EXIT-ROW-AT, which TASK-RUN-EXIT-ONE calls outside the lock.
\ Two registrations growing at once append two chunks.
: EXIT-GROW ( -- )
   EXIT-CHUNK-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop FFI:>CELL {: c:n :}
   TASK-EXIT-LOCK-GET
   EXIT-ROWS-END c CELL + TASK-N>PTR !
   EXIT-CHUNKS @ c TASK-N>PTR !
   c EXIT-CHUNKS atomic!
   TASK-EXIT-UNLOCK ;

\ The ending scans its chain under the lock, so a registration from another
\ thread lands before a scan or after it; one landing after the last scan found
\ no row owed stays owed for the task's next activation. The lock covers the
\ scratch cell, so two threads registering on one task at once (AIO from the
\ task, the program from its starter) both land.
: TASK-AT-EXIT ( [ -- ] ptr n -- ) {: q tcb:ptr :}
   begin
      TASK-EXIT-LOCK-GET
      q TASK-EXIT-SCRATCH !
      tcb TASK-EXIT-HELD if EXIT-OWE TASK-EXIT-UNLOCK exit then
      drop
      TASK-EXIT-NEXT 0=
   while
      drop TASK-EXIT-UNLOCK EXIT-GROW
   repeat
   {: idx:n :}
   q idx EXIT-QT !
   idx EXIT-OWE
   tcb TASK-EXIT-SLOT@ idx TASK-EXIT-LINK-AT !
   idx 1 + tcb TCB.EXIT-SLOT !
   TASK-EXIT-UNLOCK ;

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
   \ Set even when the area is 0 bytes: a 0 here would select main's copies.
   reg TASK-REGION-BYTES + FFI:>CELL reg TASK-AREA-OFF + !
   \ DP-CHECK's bounds: zeroed cells would admit a task's `0 allot`.
   data-base reg BOOT:DP-FLOOR-CELL TASK-COPY-CELL
   data-base reg BOOT:DP-CEILING-CELL TASK-COPY-CELL
   \ The lookups' wids: a zero marker would key package lookups on wid 0.
   data-base reg BOOT:GLOBAL-WL-CELL TASK-COPY-CELL
   data-base reg BOOT:NAMESPACE-WL-CELL TASK-COPY-CELL
   data-base reg ARGC-CELL TASK-COPY-CELL
   data-base reg ARGV-CELL TASK-COPY-CELL
   data-base reg ENVP-CELL TASK-COPY-CELL
   data-base reg PROGRAM-FILE-CELL TASK-COPY-CELL
   \ The loader's record is the process's.
   data-base reg SRCLOC:SOURCE-CELL TASK-COPY-CELL
   \ So is the compile-reads record.
   data-base reg SRCLOC:READS-CELL TASK-COPY-CELL
   \ SCRIPT-ARGV picks the application convention from this cell; a task left
   \ at the region's zero reads the engine's and loses argument 0.
   data-base reg APP-ENTRY:XT-CELL TASK-COPY-CELL
   data-base reg POLICY-ABI:KEYWORD-CELL TASK-COPY-CELL
   \ A task starts on its creator's I/O devices. The cells
   \ hold a device INDEX, so copying them into a fresh region needs no
   \ relocation. The funnel's re-entrancy guard and the active-device cell start
   \ clear: the new task is not inside an operation.
   data-base reg GENIO-ABI:OUT-CELL TASK-COPY-CELL
   data-base reg GENIO-ABI:IN-CELL TASK-COPY-CELL
   data-base reg GENIO-ABI:WRITE-OFF GENIO-ABI:DEVICES cells TASK-COPY-SPAN
   0 reg GENIO-ABI:BUSY-CELL + !
   0 reg GENIO-ABI:ACTIVE-CELL + !
   rbase reg RBASE-CELL + !
   data-base reg REGION-ALIAS:DELTA-CELL TASK-COPY-CELL
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

\ Every stack a task runs on is a guarded mapping, so the requested data-stack
\ size rounds up to whole guard pages.
: TASK-STACK-BYTES ( n -- n ) {: want:n :}
   want STACK-ABI:PAGE-BYTES 1 - + STACK-ABI:PAGE-BYTES negate and ;

: TASK-REGION-TOTAL ( -- n )
   TASK-USER-NEXT @ STACK-ABI:PAGE-BYTES 1 - + STACK-ABI:PAGE-BYTES negate and
   TASK-REGION-BYTES + ;

: TASK-REGION-MAP ( n -- ptr a )
   8 / >COUNT MEM-ALLOC-CELLS ;

: TASK-PREPARE-RESOURCES ( ptr n -- ) {: tcb:ptr :}
   tcb TCB.SIZE @ TASK-STACK-BYTES MEM:ALLOC-GUARDED tcb TCB.STACK-U ! tcb TCB.STACK !
   STACK-ABI:RETURN-BYTES MEM:ALLOC-GUARDED tcb TCB.RSTACK-U ! tcb TCB.RSTACK !
   STACK-ABI:LOOP-BYTES MEM:ALLOC-GUARDED tcb TCB.LSTACK-U ! tcb TCB.LSTACK !
   TASK-REGION-TOTAL {: total:n :}
   total TASK-REGION-MAP
   total tcb TCB.REGION-U !
   tcb TCB.REGION !
   \ A plain store, as ACTIVATE's: pthread_create orders it into the new thread,
   \ the cell's only writer afterwards (TCB-HALT-PENDING?).
   0 tcb TCB.STOP !
   tcb TASK-REGION-INIT
   tcb PARK-CREATE
   tcb DONE-INIT
   TASK-CONSTRUCTED tcb TASK-STATE! ;

\ Every resource above records its owner as soon as it succeeds. A later mmap
\ or semaphore refusal leaves the task EMPTY, so no later release owns the
\ partial record; release it here before propagating the original error.
: PREPARE ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-CONSTRUCTED? if exit then
   tcb TCB.SIZE @ TASK-CHECK-SIZE
   tcb [: dup TASK-PREPARE-RESOURCES ;] catch {: rc:n :}
   drop
   rc 0<> if
      tcb TASK-RELEASE-MEM
      rc throw
   then ;

\ No thread is inside a CONSTRUCTED TCB's region and no row names it.
: TASK-REGION-FIT ( ptr n -- ) {: tcb:ptr :}
   tcb TCB.REGION-U @ TASK-REGION-BYTES - TASK-USER-NEXT @ >= if exit then
   tcb TCB.REGION @ {: old:ptr :}
   tcb TCB.REGION-U @ {: oldu:n :}
   TASK-REGION-TOTAL {: total:n :}
   total TASK-REGION-MAP tcb TCB.REGION !
   total tcb TCB.REGION-U !
   tcb TASK-REGION-INIT
   old BYTE-VIEW MEM:RELEASE-SCOPES
   old BYTE-VIEW oldu TASK-MUNMAP-SPAN ;

: TASK-PTHREAD-CREATE-RC ( ptr n -- n ) {: tcb:ptr :}
   tcb TCB.THREAD BYTE-VIEW 0 PTHREAD-ENTRY tcb BYTE-VIEW PTHREAD-CREATE-CALL ;

: TASK-PTHREAD-JOIN-CALL ( ptr n -- ) {: tcb:ptr :}
   tcb TCB.THREAD @ tcb TCB.RET BYTE-VIEW PTHREAD-JOIN-CALL TASK-RC0 ;

\ EMPTY is a release store, as DONE is: a TASK:FREE on another
\ thread that reads it (TASK-STATE@, an acquire load) clears the record, so
\ every store TASK-RELEASE-MEM made has to be visible before it.
: TASK-JOIN-RELEASE ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-PTHREAD-JOIN-CALL
   TASK-LIVE-
   tcb TASK-RELEASE-MEM
   TASK-EMPTY tcb TCB.STATUS atomic! ;

: TASK-RUN-USER ( -- n )
   [: KEY-ENSURE CALLER-REGISTER TASK-SELF TCB.USER-XT @ execute ;] catch ;

\ An uncaught worker throw ends this task only: the code is recorded, the entry
\ marks the task DONE and the other tasks keep running. A worker `die` is not
\ catchable and exits the process with its own status (Linux uses exit_group).
: TASK-RUNNER ( -- )
   TASK-RUN-USER dup 0= if drop else TASK-SELF TASK-THROW! then
   TASK-END ;

\ A create attempt that failed started no thread, so this takes back what
\ ACTIVATE published for one: the live count and RUNNING.
: TASK-CREATE-ROLLBACK ( ptr n -- ) {: tcb:ptr :}
   TASK-LIVE-
   TASK-CONSTRUCTED tcb TASK-STATE! ;

\ The code end a task takes as it goes live, in ACTIVATE: the lowest code not
\ yet flushed. An immediate word may activate inside an open definition, whose
\ body below CP is patched and flushed only at `;`, after the thread exists,
\ and CP may move back within it. So with a definition pending it is the
\ pending record's body start ([0] of the record PEND-CELL names), and an xt
\ into that body fences.
: LIVE-CP ( -- n )
   data-base PEND-CELL + @ {: pend:n :}
   pend 0= if cp@ else pend TASK-N>PTR @ then ;

: ACTIVATE ( [ -- ] ptr n -- ) {: xt tcb:ptr :}
   tcb TASK-STATE@ TASK-RUNNING = if E-TASK-STATE throw then
   tcb TASK-STATE@ TASK-HALT-REQ = if E-TASK-STATE throw then
   tcb TASK-STATE@ TASK-DONE = if tcb TASK-JOIN-RELEASE then
   tcb PREPARE
   tcb TASK-REGION-FIT
   \ A task may bind a slot this thread is called back through.
   KEY-ENSURE CALLER-REGISTER
   \ The dictionary the task runs on is taken here, as it goes live, not at
   \ PREPARE: main may still compile between a public PREPARE and this ACTIVATE,
   \ or after a failed create, and the task's NDICT and CP registers, loaded
   \ from these, bound what LIBC-OS:FORK copies on the task.
   dbase@ tcb TCB.DBASE !
   ndict@ tcb TCB.NDICT !
   LIVE-CP tcb TCB.CP !
   \ The park outlives this TCB's earlier runs (PARK-CREATE), so it may hold a
   \ hint posted after a run ended or around a create that failed.
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
   \ Every failure of the attempt returns the task to CONSTRUCTED and goes on
   \ with its code: a nonzero return is E-TASK-THREAD, and any other throw comes
   \ before pthread_create runs - the symbol resolves at its first call
   \ (E-FFI-DLSYM) and the arguments are staged and bounds-checked ahead of it.
   \ The caught body hands the TCB back, as checked catch needs a body that
   \ leaves the stack it took.
   tcb [: dup TASK-PTHREAD-CREATE-RC TASK-RC0 ;] catch nip
   dup 0 <> if tcb TASK-CREATE-ROLLBACK throw then
   drop ;

\ HALT'S REQUEST IS THE STATUS ITSELF: HALT-REQ, which only HALT's CAS writes,
\ over RUNNING. The CAS is a CASAL and TASK-STATE@ an LDAR, so the request and
\ the worker's stop read here are a release/acquire pair. TCB.STOP is the run's
\ own mark that it is ending: PAUSE sets it before it starts the halt unwind
\ and TASK-END sets it first for every ending, so a cleanup that pauses yields
\ instead of starting a second unwind, and PREPARE and ACTIVATE clear it before the
\ thread exists. No other thread writes it, so it is a plain cell, and a second
\ HALT racing the exit cannot re-arm it.
: TCB-HALT-PENDING? ( ptr n -- bool ) {: tcb:ptr :}
   tcb TASK-STATE@ TASK-HALT-REQ = tcb TCB.STOP @ 0= and ;

\ A halted task starts its unwind here, ONCE, from PAUSE or from a scope's end
\ (TASK:HALT-UNWIND): the stop mark goes first, so every cleanup the unwind runs
\ and the AT-EXIT chain may pause and only yield. With HALT-UNWIND-CELL set,
\ `n throw` runs each finally around it, innermost first, and rethrows past
\ every other catch, so no code after a catch observes the halt and a body that
\ swallows errors still ends.
\ TASK-RUNNER's catch takes n, or a code a cleanup threw during the unwind, or,
\ when the halt began inside a cleanup, the body's error that cleanup ran for.
\ PAUSE carries 0: the halt is not a body error.
\
\ Inside a callback the request stays pending: a throw there would unwind
\ through C's frames, holding the slot it entered through, with C's call never
\ drained. A live marshal frame on the region (CB-FRAME) is that case; the thunk
\ restores the cell as each callback returns, so it reads 0 once C is off the
\ stack. The task ends at its first PAUSE outside a callback and outside a
\ DEFER-ENTER section, so a HALT or KILL of a task parked in a callback takes
\ effect after C returns. A body that must answer sooner reads TASK:HALTED?.
: IN-CALLBACK? ( -- bool )
   data-base CB-FRAME + @ 0 <> ;

: HALT-UNWIND ( n -- ) {: rc:n :}
   TASK-SELF-N dup 0= if drop exit then
   TASK-N>PTR {: self:ptr :}
   self TCB-HALT-PENDING? IN-CALLBACK? 0= and TASK-DEFER-DEPTH @ 0= and if
      1 self TCB.STOP !
      1 data-base HALT-UNWIND-CELL + !
      rc throw
   then ;

: PAUSE ( -- )
   0 HALT-UNWIND
   SCHED-YIELD-CALL TASK-RC0 ;

\ The request is observed at TASK:PAUSE, so HALT wakes a task parked in
\ TASK:WAIT itself: the task re-checks its state as the hint protocol requires
\ and ends at its next PAUSE.
\
\ THE CAS RUNNING -> HALT-REQ IS THE WHOLE REQUEST (TCB-HALT-PENDING?): only it
\ writes HALT-REQ, and the run's end replaces it before ACTIVATE writes RUNNING.
\
\ Any other answer means nothing to halt (never activated, only prepared, or
\ ended) and the state is left alone: an ended task has no PAUSE to reach, and
\ DONE is never overwritten by a request nobody can observe.
\
\ The park is posted DIRECTLY, not through TASK-WAKE, whose refusal of an ended
\ task is TASK:WAKE's own contract: the task may end, or be released, between
\ the CAS and the post, and the post still meets a live park (PARK-CREATE). A
\ count posted before the next run opens dies there; one in flight as it opens
\ is the one stale hint its re-check absorbs. The post is the only
\ step after the CAS, so that hint is all a HALT can leave a run it did not see.
: HALT ( ptr n -- ) {: tcb:ptr :}
   TASK-RUNNING TASK-HALT-REQ tcb TCB.STATUS atomic-cas {: st:n :}
   st TASK-RUNNING <> st TASK-HALT-REQ <> and if exit then
   tcb TASK-PARK-SEM SEM-SIGNAL ;

: SELF-CONTEXT ( -- n )
   data-base FFI:>CELL ;

\ The caller's context: no region is at 2.
2 constant CALLER

\ A context is the main region or the caller's own.
: CONTEXT-CHECK ( n -- ) {: ctx:n :}
   ctx MAIN-BASE FFI:>CELL = if exit then
   ctx SELF-CONTEXT = if exit then
   E-TASK-STATE throw ;

: SLOT-CHECK ( n -- ) {: k:n :}
   k 0 < k ROW-N @ >= or if E-TASK-STATE throw then ;

\ The key and getter are stored before the owner, each a release store, so a
\ thunk that acquires a caller owner finds both.
: CALLER-BIND ( n -- ) {: k:n :}
   k SLOT-CHECK
   KEY-ENSURE {: key:n :}
   key CALLER-REGISTER
   k ROW ROW-OWNER atomic@ 3 and CB-ROW-CALLER = if exit then
   ROW-TABLE @ FFI:>CELL MAIN-BASE CB-ROWS + atomic!
   key MAIN-BASE CB-TLS-KEY + atomic!
   FFI:PROCESS-HANDLE s\" pthread_getspecific\z" drop FFI:DLSYM {: get:n :}
   get 0= if E-TASK-STATE throw then
   get MAIN-BASE CB-TLS-GET + atomic!
   begin
      k ROW ROW-OWNER atomic@
      dup 0= if drop 0 CB-ROW-BUSY k ROW ROW-OWNER atomic-cas then
      dup 3 and CB-ROW-CALLER = if drop exit then
      dup 0<>
   while
      CB-ROW-BUSY <> if E-TASK-STATE throw then
      SCHED-YIELD-CALL TASK-RC0
   repeat
   drop
   k ROW ROW-REGION atomic@ 0 <> if
      0 k ROW ROW-OWNER atomic!
      E-TASK-STATE throw
   then
   CB-ROW-CALLER k ROW ROW-OWNER atomic! ;

\ The row moves 0 -> BUSY before it is read, and the bind decides on what it
\ reads under BUSY, where nothing else moves the row. A row that is not idle - a
\ thread inside, or another bind, unbind or task end moving it - refuses
\ the bind, even when it names this context: a mover can still drop the row it
\ holds, so its binding is not final.
: CONTEXT-BIND ( n n -- ) {: ctx:n k:n :}
   ctx CALLER = if k CALLER-BIND exit then
   ctx CONTEXT-CHECK
   k SLOT-CHECK
   ROW-TABLE @ FFI:>CELL MAIN-BASE CB-ROWS + atomic!
   0 CB-ROW-BUSY k ROW ROW-OWNER atomic-cas 0 <> if E-TASK-STATE throw then
   k ROW ROW-REGION atomic@ {: held:n :}
   held 0 <> if
      0 k ROW ROW-OWNER atomic!
      held ctx = if exit then
      E-TASK-STATE throw
   then
   ctx k ROW-NAME!
   0 k ROW ROW-OWNER atomic! ;

: CONTEXT-UNBIND ( n -- ) {: k:n :}
   k SLOT-CHECK
   0 CB-ROW-BUSY k ROW ROW-OWNER atomic-cas {: seen:n :}
   seen CB-ROW-CALLER = if
      CB-ROW-CALLER CB-ROW-BUSY k ROW ROW-OWNER atomic-cas
      CB-ROW-CALLER <> if E-TASK-STATE throw then
   else
      seen 0 <> if E-TASK-STATE throw then
   then
   0 k ROW-NAME!
   0 k ROW ROW-OWNER atomic! ;

\ ONE read of the state decides: the state moves while the owner reads it, and
\ re-reading per arm can find every arm false, leaving the task unjoined and
\ counted live for the life of the image.
\
\ KILL discards the outcome and JOIN collects it; both release the task. A task
\ parked in WAIT, WAIT-SEM, SLEEP or a host call observes the halt only when
\ released, so KILL waits for it; an owner may KILL a body that signalled its
\ own end and is still finishing.
: TASK-KILL ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-STATE@ {: st:n :}
   st TASK-EMPTY = if exit then
   st TASK-CONSTRUCTED = if
      tcb TASK-RELEASE-MEM
      TASK-EMPTY tcb TCB.STATUS atomic!
      exit
   then
   st TASK-DONE <> if tcb HALT then
   tcb TASK-JOIN-RELEASE ;

: TASK-DONE? ( ptr n -- bool )
   TASK-STATE@ TASK-DONE = ;

\ A join needs a task that is running or has ended, and exactly one joiner: a
\ second joiner is refused instead of being made to wait for a signal the first
\ has already taken. A task that was never activated and a task whose join has
\ released it are the same EMPTY task and get the same refusal.
: TASK-JOIN-CHECK ( ptr n -- ) {: tcb:ptr :}
   tcb TASK-STATE@ {: st:n :}
   st TASK-RUNNING <> st TASK-HALT-REQ <> and st TASK-DONE <> and
      if E-TASK-JOIN throw then
   0 1 tcb TCB.JOINER atomic-cas 0 <> if E-TASK-JOIN throw then ;

\ An error beats a value: a body that returned and then failed in its cleanup did
\ not finish.
: TASK-JOIN-ANSWER ( ptr n -- result<n,n> ) {: tcb:ptr :}
   tcb TASK-THROW@ dup 0 <> if RESULT:ERR exit then
   drop
   tcb TASK-RESULT? if tcb TCB.RESULT @ RESULT:OK exit then
   E-TASK-NO-RESULT RESULT:ERR ;

\ The release is the one TASK:KILL does, which is why a joined task needs no
\ kill. The outcome is read before it: the
\ release stores EMPTY, and from then a TASK:FREE on another thread may clear
\ the record.
: TASK-JOIN ( ptr n -- result<n,n> ) {: tcb:ptr :}
   tcb TASK-JOIN-CHECK
   tcb TASK-DONE-SEM SEM-WAIT
   tcb TASK-JOIN-ANSWER
   tcb TASK-JOIN-RELEASE ;

\ ---- making and giving back pool tasks ---------------------------------------
\ The link takes the head before the head moves to the first entry, so a
\ walker reading the new head finds the whole run and the old list behind it.
: LIST-PUSH ( n ptr n ptr n -- ) {: first:n link:ptr head:ptr :}
   begin
      head atomic@ dup link !
      dup first head atomic-cas =
   until ;

\ A fresh mapping is zero: every record EMPTY and CLAIM-FREE. Its first record
\ is the caller's before the chunk is published.
: TCB-GROW ( -- n )
   TCB-CHUNK-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop FFI:>CELL {: c:n :}
   TCB-POOL-N 1 - 0 ?do
      c i 1 + TCB-REC c i TCB-REC TCB-POOL-LINK !
   loop
   CLAIM-HELD c 0 TCB-REC TASK-N>PTR TCB-CLAIM !
   c c TASK-N>PTR TCB-CHUNKS LIST-PUSH
   c 0 TCB-REC c TCB-POOL-N 1 - TCB-REC TCB-POOL-LINK TCB-POOL LIST-PUSH
   c 0 TCB-REC ;

: TCB-TAKE? ( n -- bool ) {: rec:n :}
   CLAIM-FREE CLAIM-HELD rec TASK-N>PTR TCB-CLAIM atomic-cas CLAIM-FREE = ;

: TCB-TAKE ( -- n )
   TCB-POOL atomic@
   begin dup 0 <> while
      dup TCB-TAKE? if exit then
      TASK-N>PTR TASK-LINK@
   repeat ;

: TASK-NEW ( n -- ptr n ) {: size:n :}
   size TASK-CHECK-SIZE
   TCB-TAKE dup 0= if drop TCB-GROW then
   TASK-N>PTR {: tcb:ptr :}
   size tcb TCB.SIZE !
   tcb ;

: TCB-POOLED? ( ptr n -- bool ) {: tcb:ptr :}
   tcb FFI:>CELL {: want:n :}
   TCB-POOL atomic@
   begin dup 0 <> while
      dup want = if drop true exit then
      TASK-N>PTR TASK-LINK@
   repeat
   drop false ;

\ Zeroes the TCB but its park, which a post in flight may still meet.
: TCB-CLEAR ( ptr n -- ) {: tcb:ptr :}
   TASK-ABI:PARK-OFF 0 ?do 0 tcb i + ! CELL +loop
   TASK-TCB-BYTES TASK-ABI:PARK-OFF TASK-SEMAPHORE-BYTES + ?do 0 tcb i + ! CELL +loop ;

\ Of two TASK:FREEs on one task only one proceeds and the other is refused; a
\ task in any state but EMPTY is refused. The record is cleared before its claim
\ says FREE, so TASK:NEW never takes a record holding the last task's rows.
: TASK-FREE ( ptr n -- ) {: tcb:ptr :}
   tcb TCB-POOLED? 0= if E-TASK-STATE throw then
   CLAIM-HELD CLAIM-FREEING tcb TCB-CLAIM atomic-cas CLAIM-HELD <> if
      E-TASK-STATE throw
   then
   tcb TASK-STATE@ TASK-EMPTY <> if
      CLAIM-HELD tcb TCB-CLAIM atomic!
      E-TASK-STATE throw
   then
   tcb EXIT-ROWS-FREE
   tcb TCB-CLEAR
   CLAIM-FREE tcb TCB-CLAIM atomic! ;

\ ---- the three storage definers -----------------------------------------------
\ A `create ... does> ( -- ptr n ) ;` word, as TASK and FACILITY make, answers
\ the absolute address of its storage, the same in every thread.

\ Every body cell but the size and the cleanup chain's head is this process's,
\ so a program starts with each defined task EMPTY.
: TASK ( n -- )
   dup TASK-CHECK-SIZE
   TASK-ALIGN8
   create
      here CELL-VIEW >r
      ,  TASK-ABI:EXIT-SLOT-OFF CELL - SCRATCH-ALLOT
      0 ,  TASK-TCB-BYTES TASK-ABI:EXIT-SLOT-OFF - CELL - SCRATCH-ALLOT
      here ptr-cell-mark 0 ,
      TASK-CHAIN@ r@ TASK-TCB-BYTES + 0 ptr-field !
      r> TASK-CHAIN!
   does> ( -- ptr n ) ;

: #USER ( -- n )
   TASK-USER-NEXT @ ;

\ The largest claim end whose TASK-REGION-TOTAL cannot wrap.
MEM-MAX-N TASK-REGION-BYTES - STACK-ABI:PAGE-BYTES - constant USER-END-MAX

\ Only main claims, and only while no task is live: a live task's area cannot
\ grow. A task's own region holds 1 in TASKS-LIVE-CELL, so its claim throws.
: USER-NEXT ( n n -- n ) {: off:n size:n :}
   off TASK-USER-NEXT @ < if E-TASK-USER throw then
   size 0 < if E-TASK-USER throw then
   size USER-END-MAX off - > if E-TASK-USER throw then
   data-base TASKS-LIVE-CELL + atomic@ 0<> if E-TASK-USER throw then
   off size + ;

\ A slot body: area offset, size, previous body (null ends), main's copy.
\ Only main's region holds 0 at TASK-AREA-OFF; a task's holds its area.
0 constant SLOT-OFF
CELL constant SLOT-SIZE
2 cells constant SLOT-PREV
3 cells constant SLOT-DATA

PERSISTED-PTR-VARIABLE SLOT-LAST

: SLOT-LAST@ ( -- ptr n )
   SLOT-LAST @ ;

: SLOT-LAST! ( ptr n -- )
   SLOT-LAST ! ;

: SLOT-LAY ( n n -- ) {: off:n size:n :}
   here CELL-VIEW {: body:ptr :}
   off , size ,
   here ptr-cell-mark 0 ,
   size SCRATCH-ALLOT
   SLOT-LAST@ body SLOT-PREV + 0 ptr-field !
   body SLOT-LAST! ;

public

: +USER ( n n -- n ) {: off:n size:n :}
   off size USER-NEXT {: next:n :}
   TASK-ALIGN8
   create off size SLOT-LAY
   next TASK-USER-NEXT !
   next
   does> ( -- ptr n ) {: body:ptr :}
   data-base TASK-AREA-OFF + @ dup 0= if
      drop body SLOT-DATA +
   else
      body SLOT-OFF + @ + TASK-N>PTR
   then ;

private

: SLOT-HOLDS? ( ptr n ptr n -- bool ) {: at:ptr b:ptr :}
   at b SLOT-DATA + - {: d:n :}
   d 0 >= d b SLOT-SIZE + @ < and ;

: MAIN-SLOT-OFF ( ptr n -- n ) {: at:ptr :}
   SLOT-LAST@
   begin
      dup FFI:>CELL 0= if E-TASK-USER throw then
      at over SLOT-HOLDS? 0=
   while
      SLOT-PREV + 0 ptr-field @
   repeat
   {: b:ptr :}
   b SLOT-OFF + @ at b SLOT-DATA + - + ;

\ cur lies in a slot of the calling thread's copy; the answer is the same place
\ in that task's copy.
: HIS ( ptr n ptr n -- ptr n ) {: tcb:ptr cur:ptr :}
   data-base TASK-AREA-OFF + @ {: area:n :}
   area 0= if cur MAIN-SLOT-OFF else cur FFI:>CELL area - then
   tcb TCB.REGION @ TASK-REGION-BYTES + + ;

\ The record is this process's lock and its holder, so every cell is a scratch
\ cell, which a link writes zero.
: FACILITY ( -- )
   TASK-ALIGN8
   create
   TASK-FACILITY-BYTES TASK-CELL / 0 ?do here i cells + scratch-cell-mark loop
   TASK-FACILITY-BYTES allot
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

\ PTHREAD_MUTEX_INITIALIZER: Darwin's _PTHREAD_MUTEX_SIG_init then zeros, glibc's zeros.
: FACILITY-INIT ( ptr n -- ) {: f:ptr :}
   0 f FACILITY-OWNER!
   f FACILITY-MUTEX {: m:ptr :}
   TASK-MUTEX-BYTES TASK-CELL / 0 ?do 0 m i cells + ! loop
   HB-TARGET-MACOS? if $32AAABA7 m ! then ;

\ A zero Darwin signature is no mutex: set the initializer's from 0 only, so a
\ lost race, or a signature a lock already wrote, leaves a held mutex alone.
: MUTEX-READY ( ptr n -- ) {: m:ptr :}
   HB-TARGET-MACOS? if 0 $32AAABA7 m atomic-cas drop then ;

: GET ( ptr n -- ) {: f:ptr :}
   TASK-OWNER {: owner:n :}
   f FACILITY-OWNER@ owner = if exit then
   f FACILITY-MUTEX MUTEX-READY
   f FACILITY-MUTEX BYTE-VIEW MUTEX-LOCK-CALL TASK-RC0
   owner f FACILITY-OWNER! ;

: RELEASE ( ptr n -- ) {: f:ptr :}
   TASK-OWNER {: owner:n :}
   f FACILITY-OWNER@ owner <> if exit then
   0 f FACILITY-OWNER!
   f FACILITY-MUTEX BYTE-VIEW MUTEX-UNLOCK-CALL TASK-RC0 ;

: SEMAPHORE ( -- )
   TASK-ALIGN8
   create TASK-SEMAPHORE-BYTES allot
   does> ( -- TASK:sem ) FFI:>CELL >SEM ;

\ ---- sleeping ----------------------------------------------------------------
\ The request and remainder timespecs are one slot: each task sleeps on its own.
#USER 7 + TASK-CELL-MASK and TASK-SLEEP-BYTES +USER SLEEP-SPECS drop

: SLEEP-REQ ( -- ptr n )
   SLEEP-SPECS ;

: SLEEP-REM ( -- ptr n )
   SLEEP-SPECS TASK-TIMESPEC-BYTES + ;

: SPEC! ( n ptr n -- ) {: ns:n spec:ptr :}
   ns TASK-NS-PER-S /   spec TASK-SPEC-SEC-OFF + !
   ns TASK-NS-PER-S mod spec TASK-SPEC-NSEC-OFF + ! ;

: SPEC@ ( ptr n -- n ) {: spec:ptr :}
   spec TASK-SPEC-SEC-OFF + @ TASK-NS-PER-S *
   spec TASK-SPEC-NSEC-OFF + @ + ;

\ The bound is tested by division before the multiply and the add, so neither
\ can wrap.
: SLEEP-DEADLINE-AT ( ms -- n ) {: timeout:ms :}
   timeout MS>N {: want:n :}
   mono-ns {: now:n :}
   want 0 <  want TASK-NS-MAX now - TASK-NS-PER-MS / > or
   if E-TASK-SLEEP-MS throw then
   want TASK-NS-PER-MS * now + ;

: SLEEP-LEFT-NS ( n -- n ) {: deadline:n :}
   deadline mono-ns - dup 0 <= if drop 0 then ;

: SLEEP-CALL ( -- n )
   SLEEP-REQ BYTE-VIEW SLEEP-REM BYTE-VIEW NANOSLEEP-CALL
   0= if 0 exit then
   FFI:ERRNO ;

\ nanosleep reports EINTR with the time it did not serve: the retry asks for
\ exactly that remainder, so no signal can shorten the sleep. The remainder is
\ capped by what is left of an absolute deadline taken before the first call,
\ so no storm of signals can stretch the total either.
: TASK-SLEEP ( ms -- ) {: timeout:ms :}
   timeout MS>N 0= if exit then
   timeout SLEEP-DEADLINE-AT {: deadline:n :}
   timeout MS>N TASK-NS-PER-MS * SLEEP-REQ SPEC!
   begin
      SLEEP-CALL dup 0= if drop exit then
      TASK-EINTR <> if E-TASK-THREAD throw then
      SLEEP-REM SPEC@ deadline SLEEP-LEFT-NS min {: next:n :}
      next 0 <= if exit then
      next SLEEP-REQ SPEC!
   again ;

\ ---- parking until woken or a deadline ---------------------------------------
\ A wait longer than tv_sec can hold asks for the most it can and leaves the
\ rest to the caller's next turn.
: MACH-SPEC ( n -- n ) {: ns:n :}
   ns TASK-NS-PER-S / MACH-SEC-MAX > if MACH-SEC-MAX exit then
   ns TASK-NS-PER-S /   ns TASK-NS-PER-S mod 32 lshift or ;

: MACH-WAIT-NS ( n sem -- bool ) {: ns:n s:sem :}
   s SEM-OBJ LE:U32@ ns MACH-SPEC MACH-SEM-TIMEDWAIT
   dup 0= if drop true exit then
   dup MACH-TIMED-OUT = swap MACH-ABORTED = or if false exit then
   E-TASK-THREAD throw ;

\ The deadline is CLOCK_MONOTONIC's own now plus ns, written into SLEEP's
\ request: a task is in SLEEP or here, never both. The caller measured ns
\ against its own mono-ns, so now plus ns may pass TASK-NS-MAX and is clamped
\ there; the caller judges its deadline by mono-ns and waits again if the
\ clamped wait ends before it.
: POSIX-WAIT-NS ( n sem -- bool ) {: ns:n s:sem :}
   TASK-CLOCK-MONOTONIC SLEEP-REQ BYTE-VIEW CLOCK-GETTIME-CALL TASK-RC0
   SLEEP-REQ SPEC@ {: now:n :}
   ns TASK-NS-MAX now - min now + SLEEP-REQ SPEC!
   s SEM-OBJ TASK-CLOCK-MONOTONIC SLEEP-REQ BYTE-VIEW SEM-CLOCKWAIT-CALL
   0= if true exit then
   FFI:ERRNO dup TASK-ETIMEDOUT = swap TASK-EINTR = or if false exit then
   E-TASK-THREAD throw ;

\ One bounded wait on a semaphore: true when it took a count, false when the
\ time ran out or a signal cut the wait short. Either false sends the caller
\ back to its own deadline, so neither needs telling apart here.
: SEM-WAIT-NS ( n sem -- bool )
   HB-TARGET-MACOS? if MACH-WAIT-NS exit then
   POSIX-WAIT-NS ;

\ Each wait asks for what is left of one absolute mono-ns deadline taken before
\ the first, so a signal that cuts a wait short neither shortens nor stretches
\ the total, and a host timer that fires early waits again for the rest: false
\ comes only once mono-ns reaches the deadline. A hint posted as the deadline
\ passes is either taken by the last wait or stays in the count for the next
\ WAIT or WAIT-MS.
: TASK-STOP-FOR ( ms -- bool ) {: timeout:ms :}
   timeout SLEEP-DEADLINE-AT {: deadline:n :}
   PARK-SELF {: park:sem :}
   begin
      deadline SLEEP-LEFT-NS park SEM-WAIT-NS if true exit then
      deadline SLEEP-LEFT-NS 0=
   until false ;

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

\ ---- callback contexts -------------------------------------------------------
\ The main region, from any task.
: MAIN-BASE ( -- ptr n )
   MAIN-BASE ;

\ The context number of the calling task's own region.
: SELF-CONTEXT ( -- n )
   SELF-CONTEXT ;

\ A slot bound to it runs each call in the calling thread's own region, from
\ inside a foreign call that thread made; any number of threads may be inside.
: CALLER-CONTEXT ( -- n ) CALLER ;

\ Binds slot n to a context: the main region, the caller's own or
\ CALLER-CONTEXT. Binding an idle slot to the context it names is a no-op; a
\ slot naming another context, with a thread inside, or that another bind,
\ unbind or task end is moving, is refused. A binding lives until
\ CONTEXT-UNBIND or the end of the task whose region it names; a program
\ starts with every slot unbound.
: CONTEXT-BIND ( n n -- )
   CONTEXT-BIND ;

: CONTEXT-UNBIND ( n -- )
   CONTEXT-UNBIND ;

\ Called by a CALLBACK: declaration, before any C pointer to this new row is
\ exposed. The table may grow while earlier idle bindings already exist.
: DECLARE-CALLBACK ( n -- )
   ROW-DECLARE ;

\ Yields the processor. In a task whose run a TASK:HALT has reached, outside a
\ callback and outside DEFER-ENTER, it unwinds the body through every finally
\ around it, once per run; a catch between it and the task's root never resumes.
: PAUSE ( -- )
   PAUSE ;

\ Serves this task's pending TASK:HALT here, carrying n, where PAUSE would: in a
\ task whose run a TASK:HALT has reached and has not yet begun to unwind,
\ outside a callback and outside DEFER-ENTER, it unwinds as PAUSE's halt does
\ but delivers n to the task's root, so THROW@ and JOIN answer n even when a
\ cleanup throws; 0 delivers what PAUSE does. Anywhere else it
\ drops n and returns. A scope that ends while a halt is pending uses it to
\ keep its own selected error as the task's.
: HALT-UNWIND ( n -- )
   HALT-UNWIND ;

\ Critical task-local resource transfers and disposal may call PAUSE while a
\ HALT is pending. The request remains pending until a later PAUSE outside it.
: DEFER-ENTER ( -- )
   TASK-DEFER-ENTER ;

: DEFER-LEAVE ( -- )
   TASK-DEFER-LEAVE ;

\ True for the whole of a halted run, cleanups included; a callback body reads
\ it to return to C, where TASK:PAUSE only yields.
: HALTED? ( -- bool )
   TASK-SELF-N dup 0= if drop false exit then
   TASK-N>PTR TASK-STATE@ TASK-HALT-REQ = ;

\ True from a halt's request until its unwind starts or the body has ended, in
\ a callback or a DEFER-ENTER section too.
: HALT-PENDING? ( -- bool )
   TASK-SELF-N dup 0= if drop false exit then
   TASK-N>PTR TCB-HALT-PENDING? ;

\ Parks the calling task in the kernel for at least ms milliseconds, holding
\ nothing. It observes no TASK:HALT until it wakes, so TASK:KILL on one waits
\ out the sleep. Zero returns at once; a duration below zero, or one whose
\ nanosecond deadline past mono-ns would not fit a cell, is E-TASK-SLEEP-MS.
: SLEEP ( ms -- )
   TASK-SLEEP ;

\ ---- the task's own wake-up --------------------------------------------------
\ Parks the calling task until somebody WAKEs it, from a worker or from the main
\ thread, which parks on the one main record instead of a TCB. It burns no CPU
\ and observes no TASK:HALT while it is parked - TASK:HALT wakes its target, so a
\ halted task returns from its WAIT and ends at its next TASK:PAUSE.
\
\ WAKE is a HINT, not a message: the count keeps a WAKE that arrives before the
\ WAIT, so no hint is lost, but a WAIT takes one hint whoever posted it. A caller
\ re-checks its own state after every WAIT and waits again if the state it waits
\ for has not arrived:
\
\    begin  MY-STATE @ DONE = if exit then  TASK:WAIT  again
\
\ A loop that must also answer TASK:HALT calls TASK:PAUSE in that loop and gives
\ its resources back in a finally around the wait.
: WAIT ( -- )
   TASK-STOP ;

\ Parks the calling task as WAIT does until somebody WAKEs it or ms
\ milliseconds pass, whichever comes first, and answers true when a hint woke
\ it, false at the deadline, never before it. It takes one hint as WAIT does,
\ so the caller re-checks its own state either way, and a TASK:HALT wakes it
\ as it wakes WAIT: it answers true and the halt ends the task at its next
\ TASK:PAUSE. The main thread parks on its one record. Zero takes a pending
\ hint without blocking; a duration below zero, or one whose nanosecond
\ deadline past mono-ns would not fit a cell, is E-TASK-SLEEP-MS.
: WAIT-MS ( ms -- bool )
   TASK-STOP-FOR ;

\ Posts the named task's park. The null TCB - what TASK:SELF answers on the main
\ thread - posts the main thread's park. A task that was never activated, one
\ that is only prepared and one that has ended are E-TASK-STATE.
: WAKE ( ptr n -- )
   TASK-WAKE ;

\ Requests the stop the target observes at its next TASK:PAUSE, and wakes it so
\ a task parked in TASK:WAIT gets there. A task inside a callback observes it at
\ its first TASK:PAUSE after C returns. A task that has ended, one that was
\ never activated and one that is only prepared have no PAUSE to reach: halting
\ them is a no-op that leaves their state alone.
: HALT ( ptr n -- )
   HALT ;

\ Releases the task: a task that was activated is halted if it is still running,
\ joined, and its memory given back, whichever of those states it reaches first
\ - a body ending under the KILL is the teardown shape, not an error. A task
\ only prepared gives its memory back; one never prepared or already released
\ is the no-op it is after a TASK:JOIN.
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
\ E-TASK-NO-RESULT when the worker ended without an answer. A halted worker
\ answers the n a TASK:HALT-UNWIND carried or the body's error the halt overtook
\ in a cleanup, else a code a cleanup threw during its unwind, else ok with a
\ value it stored before the halt, else E-TASK-NO-RESULT. A task that was never
\ activated, a task already joined, and a second joiner are E-TASK-JOIN.
: JOIN ( ptr n -- result<n,n> )
   TASK-JOIN ;

\ A task made at run time, EMPTY as a defined one is before its first
\ TASK:ACTIVATE, with a data stack of n bytes; it defines nothing, so any
\ thread may call it while tasks run. n below TASK:MIN-STACK is E-TASK-SIZE.
: NEW ( n -- ptr n )
   TASK-NEW ;

\ Gives back a TASK:NEW task that TASK:JOIN or TASK:KILL released, or that was
\ never prepared, with every cleanup TASK:AT-EXIT registered on it. A task in
\ any other state, a defined task, the null TCB and a task already given back
\ are E-TASK-STATE, and the task is left as it was. The handle is dead after it:
\ TASK:NEW may hand the record out again. The stacks went back at the join;
\ the record and its park stay in the pool for the next TASK:NEW.
: FREE ( ptr n -- )
   TASK-FREE ;

\ Adds a cleanup quotation to that task's chain, run in the task's own thread
\ when it ends - body returned or threw, a halt at TASK:PAUSE included - after
\ the body has unwound through its finally cleanups and before the join is
\ released, newest first. A quotation already in the chain is not added again.
\ One registered from the task's own thread while its cleanups run runs before
\ the task ends, again if it already ran there, so a cleanup that registers
\ itself each time it runs keeps the task ending. A throw inside a cleanup
\ becomes the task's error if it has none and never leaves the task.
: AT-EXIT ( [ -- ] ptr n -- )
   TASK-AT-EXIT ;

\ What a process just forked owes this runtime, before any code of its own:
\ tasks do not cross a fork.
\ fork copies only the thread that called it: the exit-chain lock another task
\ held stays held in the child with nobody to free it, and the main thread's
\ park is the parent's.
: CHILD-RESET ( -- )
   TASK-EXIT-UNLOCK
   MAIN-PARK-FORGET ;

: #USER ( -- n )
   #USER ;

: HIS ( ptr n ptr n -- ptr n )
   HIS ;

\ A facility excludes (owner-tracked mutex: GET is idempotent for its owner,
\ RELEASE a no-op for anyone else); a semaphore counts. Atomic cells shared
\ across tasks are 8-byte aligned.
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

: WAIT-SEM ( sem -- )
   SEM-WAIT ;

: SIGNAL-SEM ( sem -- )
   SEM-SIGNAL ;

\ Takes a count if one is there; false means it would have blocked.
: TRY-WAIT-SEM ( sem -- bool )
   SEM-TRY-WAIT ;

\ A semaphore from this package's pool, which grows when every record is in
\ use. It arrives uninitialized, like a defined one.
: NEW-SEMAPHORE ( -- sem )
   SEM-NEW ;

\ Destroys it if it is still live and returns the record to the pool. A handle
\ that did not come from the pool is E-TASK-SEM-POOL.
: FREE-SEMAPHORE ( sem -- )
   SEM-FREE ;

seal-package

;package
