# Habu Tasking

`lib/task.f` implements native CPU tasks over `pthread` on macOS/aarch64 and
Linux/aarch64 in package `TASK`. Load it with:

```forth
require lib/task.f
```

The module owns its dependencies (`lib/errors.f`, `lib/memory.f`, `lib/ffi-abi.f`);
callers and test suites must not encode that include order.

## Model

Tasks execute already-compiled Habu words in OS threads. The dictionary and code
region are shared read-only while tasks are live. Each task gets:

- a task control block (`TASK`);
- a private data stack;
- a private data/user region used by `+USER` variables;
- a pthread handle and return slot;
- a stop flag honored by `PAUSE`.

Compilation and dictionary mutation are forbidden while any task is live.
Compiler and dictionary mutation paths check `TASKS-LIVE-CELL` and exit with
code `$4F`, printing the rejected token. What the ban protects is the code
band's W^X flip: a definition holds the band writable from its colon to the `;`
flush, and the flip is aligned outward to the `PROT-PAGE-MAX` unit (`$10000`,
`src/habu/layout.f`), so every word already compiled in the 64K unit that holds
the code pointer loses execute permission while the definition is open, and a
task running one of them faults. Measured with the live count reset by hand:
`lib/aio.f` loaded last, its loop task and one client task ticking 1 ms
timeouts, then 2000 empty definitions - SIGSEGV at a pc 14.7K below the code
pointer, inside `lib/aio.f`; with 32K of definitions between `lib/aio.f` and the
loop start the code pointer sat in the next unit and the same run survived 21
client rounds. No task is exempt, the AIO loop task included: an exemption would
have to keep every live task's code out of the unit the code pointer is in,
which no later definition can promise, and the forget paths (`cp!`, `ndict!`)
free code under a task that may be running it. This bounds what a remote REPL
can do: a REPL served over a connection ([genio.md](genio.md)) can evaluate
while worker tasks run, but a colon definition sent to it is dictionary mutation
and ends the process. Define first, then start the tasks, the AIO loop last
([aio.md](aio.md)). Linux fatal exits use `exit_group`
(`94`), not thread-local `exit` (`93`), so an error in any thread terminates the
whole process instead of leaving worker threads behind.

Worker bodies run through a task wrapper. A worker `die` keeps its explicit exit
status and message and still ends the process. An uncaught worker `throw` ends
only that task, as SwiftForth's thread entry does: the wrapper records the throw
code in the task's TCB, the task reaches `DONE`, and the process and the other
live tasks continue. `TASK:THROW@` reads the code; it stays readable after
`TASK:KILL` has joined the task and is cleared by the next `TASK:ACTIVATE`. The
richer outcome - the value the worker meant to answer with, or the code that
ended it - is `TASK:JOIN` below; see [tasking-models.md](tasking-models.md) for
how SwiftForth, polyFORTH and VFX handle this.

## Public Words

```forth
TASK:TASK            ( n -- )          \ define a task TCB; n is stack bytes
TASK:MIN-STACK       ( -- n )
TASK:PREPARE         ( ptr a -- )      \ allocate task stack/region without starting
TASK:ACTIVATE        ( n ptr a -- )    \ run xt in a pthread-backed task
TASK:SELF            ( -- ptr a )
TASK:SELF-N          ( -- n )
TASK:PAUSE           ( -- )            \ yield; worker exits if HALT requested
TASK:SLEEP           ( ms -- )         \ park this task for a duration
TASK:STOP            ( -- )            \ park this task until somebody WAKEs it
TASK:WAKE            ( ptr a -- )      \ post that task's own wake-up
TASK:HALT            ( ptr a -- )      \ request stop at next PAUSE; wakes the target
TASK:KILL            ( ptr a -- )      \ join/release task memory
TASK:DONE?           ( ptr a -- bool )
TASK:THROW@          ( ptr a -- n )    \ uncaught throw code, 0 if none

TASK:RETURN          ( n -- )          \ the worker's answer, from inside the task
TASK:JOIN            ( ptr a -- result<n,n> ) \ wait for the end, take the outcome
TASK:AT-EXIT         ( [ -- ] ptr a -- )      \ add a cleanup run in the task when it ends

TASK:#USER           ( -- n )
TASK:+USER           ( n n -- n )      \ define task-local user variable
TASK:HIS             ( ptr a ptr a -- ptr a )

TASK:FACILITY        ( -- )            \ define pthread mutex storage
TASK:FACILITY-INIT   ( ptr a -- )
TASK:GET             ( ptr a -- )
TASK:RELEASE         ( ptr a -- )

TASK:SEMAPHORE       ( -- )            \ define one counted semaphore
TASK:SEMAPHORE-INIT  ( n TASK:sem -- ) \ n is the initial count
TASK:SEMAPHORE-DESTROY ( TASK:sem -- )
TASK:WAIT            ( TASK:sem -- )   \ block until positive, then decrement
TASK:SIGNAL          ( TASK:sem -- )   \ increment and wake one waiter
TASK:TRY-WAIT        ( TASK:sem -- bool ) \ decrement if positive; never blocks
TASK:NEW-SEMAPHORE   ( -- TASK:sem )   \ take one from the pool
TASK:FREE-SEMAPHORE  ( TASK:sem -- )   \ destroy it and give the record back

TASK:SEND-MESSAGE    ( n ptr a -- )    \ post one cell to that task
TASK:GET-MESSAGE     ( -- n ptr a )    \ take this task's message and its sender
TASK:MSG?            ( ptr a -- bool ) \ does that task hold an unread message
```

Use `TASK:KILL` for teardown of a task whose outcome nobody wants, and
`TASK:JOIN` when the outcome matters: the join releases the task too. A task that
loops must call `TASK:PAUSE` or block in a host call; `TASK:HALT` is cooperative
and is observed by `TASK:PAUSE`.

Both tolerate a task that ends under them, which is the ordinary case for this
teardown: the body's own "I am finished" write happens before the task reaches
`DONE`, so an owner woken by it arrives while the TCB still says `RUNNING`.
`TASK:KILL` of a task that was activated always joins and releases it, whichever
state it reaches first, and `TASK:HALT` of a task that has ended, was never
activated or is only prepared is a no-op that leaves its state alone. Only
`TASK:WAKE` refuses an ended task - a hint would sit in a record the next
activation is not entitled to.

The ending task release-publishes `DONE` and `TASK:DONE?` is an acquire load of
it, so an owner that sees it true without joining sees every write the body made
before it ended; `TASK:HALT`'s request and the worker's stop read are that same
pair the other way round.

The surface tracks the SwiftForth multitasking words captured in
`docs/swiftforth-task-api.md`. Habu keeps the task body typed by passing an XT to
`TASK:ACTIVATE` instead of parsing a following source body.

## STOP and WAKE

A task that waits for another task or for a completion loop parks on its OWN
wake-up: `TASK:STOP` blocks on the semaphore in the calling task's TCB and
`TASK:WAKE` posts the semaphore of the task it names. Nothing is shared between
the two ends but the TCB, so one loop can serve any number of waiters — which a
pooled semaphore cannot, since the pool holds `$40` records.

| Word | Effect | Blocks |
| --- | --- | --- |
| `TASK:STOP` | `( -- )` | until somebody WAKEs this task |
| `TASK:WAKE` | `( ptr a -- )` | never |

**WAKE is a hint, STOP waits for a hint, and the caller re-checks its own state
after every STOP.** The park is a counting semaphore, so a WAKE that arrives
before the STOP is not lost: the STOP it precedes returns at once. It also means
a STOP takes one hint whoever posted it and whatever they meant by it, so a
wake-up is never evidence that the state the caller wants has arrived:

```forth
: AWAIT-READY ( -- )
   begin
      READY @ 0 <> if exit then          \ the caller's own state, re-read
      TASK:STOP                          \ a hint; not the answer
   again ;
```

- The main thread has no TCB — `TASK:SELF` answers the null TCB there and
  `TASK:SELF-N` answers zero — so its STOP parks on one main record package
  `TASK` owns, and `TASK:WAKE` of the null TCB posts that record. A program with
  no tasks of its own can therefore wait on a loop too.
- The park record is created with the task and destroyed with its memory, like
  the mailbox and the done semaphore. A `TASK:WAKE` of a task that was never
  activated, one that is only prepared, and one that has ended are all
  `E-TASK-STATE`: the count would sit in a record the task's next activation is
  not entitled to.
- A stopped task observes no `TASK:HALT`, exactly as a sleeping or a
  semaphore-blocked one does not. So `TASK:HALT` wakes its target itself: the
  task returns from its STOP, re-checks as above, and ends at its next
  `TASK:PAUSE`. **A STOP loop that must answer a halt calls `TASK:PAUSE` in that
  loop**; one that never pauses is ended only by `TASK:KILL`'s join.
- The count is not cleared by `TASK:ACTIVATE`: a task reactivated without being
  released can see one stale hint, which the re-check above absorbs.

[aio.md](aio.md) builds on this: `AIO:AWAIT` is exactly the loop above over one
io_uring record, and the loop that drains the ring wakes each waiter by TCB.

## Joins and cleanups

A worker answers with `TASK:RETURN` and the task that started it collects that
answer with `TASK:JOIN`, taking VFX's task exit code and `AtTaskExit` cleanup
([tasking-models.md](tasking-models.md) sections 3 and 5) and returning
`result<ok,err>` from `lib/adt/result.f` rather than a bare code, so no caller can
read an outcome without deciding what to do about the failing arm.

```forth
: WORKER ( -- )                        \ inside a task
   TALLY @ TASK:RETURN ;               \ the task's one answer

\ A result is a layout value, so it is constructed and MATCHed inside a word.
: COLLECT ( -- )
   ['] CLOSE-FILES ADDER TASK:AT-EXIT  \ runs in ADDER's thread when it ends
   ['] WORKER ADDER TASK:ACTIVATE
   ADDER TASK:JOIN MATCH result        \ blocks; releases ADDER; no TASK:KILL
     ok  OF TOTAL ! ENDOF              \ the value the worker returned
     err OF REPORT ENDOF               \ the code that ended it
   ;MATCH ;
```

| Word | Effect | Blocks |
| --- | --- | --- |
| `TASK:RETURN` | `( n -- )` | never |
| `TASK:JOIN` | `( ptr a -- result<n,n> )` | until the task has ended |
| `TASK:AT-EXIT` | `( [ -- ] ptr a -- )` | never |

- `TASK:JOIN` waits on a semaphore the ending task signals, so a joiner is parked
  in the kernel rather than polling `TASK:DONE?`. The semaphore is created with
  the task and destroyed with its memory, like the mailbox.
- The `ok` arm carries the value the worker stored with `TASK:RETURN`. The `err`
  arm carries the code of the worker's uncaught throw - the same code
  `TASK:THROW@` keeps - or `E-TASK-NO-RESULT` when the task ended without
  answering at all. An error beats a value: a body that returned and then failed
  in its cleanup did not finish.
- `TASK:RETURN` writes a cell in the task's own TCB and is single-assignment: a
  second call, including one from the task's own cleanup, is `E-TASK-STATE`, and
  so is a call from a thread that is not a task. `TASK:ACTIVATE` clears the
  answer, so a reactivated task answers for its new run alone.
- `TASK:JOIN` releases the thread and its memory exactly as `TASK:KILL` does, so
  a joined task needs no kill and `TASK:KILL` on one is the no-op it is on any
  EMPTY task. The outcome is read after that release, from TCB rows the release
  leaves alone - the same storage that keeps `TASK:THROW@` readable.
- A task that was never activated, a task that was only prepared, and a task a
  join has already released have nothing to wait for: all three are
  `E-TASK-JOIN`. So is a second joiner of a live task - the right to join is
  claimed in one atomic step, so the loser is refused rather than waiting for a
  signal the winner has taken.
- `TASK:AT-EXIT` adds a cleanup quotation to that task's chain, run in the task's
  own thread when it ends, before the join is released. All three endings run the
  chain: the body returning, the body throwing, and a halted body leaving at
  `TASK:PAUSE`. It runs newest registration first; a quotation the task already
  holds is not registered a second time; and a registration belongs to the task
  definition, so it also serves the task's next activation.
- A throw inside a cleanup never leaves the task and never ends the process: the
  first cleanup that throws becomes the task's error when the body left none, a
  later throwing cleanup is dropped, and one is dropped altogether when the body
  already failed - so the first failure is the one the join reports. Every
  cleanup in the chain runs whatever the ones before it did.
- The cleanup quotations live in a typed row inside package TASK, stored as
  quotations into storage declared to hold them - the checker's proven-quotation
  store, not a cell cast back to code - with a scalar row beside it linking each
  task's chain. The rows hold `$80` registrations for one image; a task
  registering past that is `E-TASK-EXIT-TABLE`. A row belongs to its (task,
  quotation) pair for the life of the image, so the row cannot leak and needs no
  free list.
- The same POSIX rule as everything else here: end a task's joiners before you
  kill it. `TASK:KILL` destroys the semaphore a joiner may be parked in.

## Library storage classes

Every library states its class in its own header too, so a caller never has to
read the source to find out. There are three:

- **process-wide** — one set of state for the image, shared by every task. The
  library is single-task unless the caller holds a `TASK:FACILITY` across the
  whole sequence that uses it, not merely across each word.
- **task-local** — a `TASK:+USER` row, or a per-task cell of the DATA header.
  Each task's state is its own, so any number of tasks may use the library at
  once.
- **caller-owned** — the caller supplies the storage. Safe from any number of
  tasks, and the caller decides whether two of them share a buffer.

| library | class | what the class is about |
| --- | --- | --- |
| `lib/string.f` | task-local (SB) / caller-owned (`BUF-*`) | `SB-BUF` and `SB-LEN` are the STRING-ABI band of the per-task region; `BUF-RESET`/`BUF-APPEND`/`BUF-APPEND-C`/`BUF-LEN@` take the caller's buffer, capacity and length cell, for when two tasks must share one |
| `lib/fmt.f` | task-local | the integer render buffer and the `POW10I`/`SB-FRAC` scratch cells are the FMT-ABI band of the per-task region; they append into string's SB, which is task-local too, so two tasks formatting at once share nothing |
| `lib/fs.f` | task-local (the per-call slots) / caller-owned (the walk) | the descriptor, length, offset, path and stat buffer are the FS-ABI band of the per-task region, so any number of tasks may be in `READ-ALL`, `FILE-SIZE` or an `FS-*` predicate at once; the walk is fifteen times that band and cannot be per-task, so it is the CALLER's: `WALK-FILES-IN` takes a context of `FS-WALK-BYTES` holding the whole per-depth state, and any number of tasks walk at once by holding one each. `WALK-FILES` is the one-context form over the static `FS-WALK-CTX0` and is single-task; `E-FS-WALK-ACTIVE` is per context, so a callback walks a second tree through a SECOND context. Two walks in flight must cover disjoint trees: `getdirentries64` on the descriptor of a directory removed meanwhile answers -1, which the walk reports as `E-FS-DIR`. The data spans `READ-ALL`/`READ-LINK`/`WRITE-ALL` take are caller-owned |
| `lib/fs-mutate.f` | task-local (the staged paths) / caller-owned (the tree removal) / process-wide (the copy buffer, the cleanup registry) | the second NUL-padded path, `ATOMIC-WRITE-FILE`'s `.tmp` sibling and the name `MAKE-TEMP-DIR` builds and RETURNS a span into are the FS-MUT-ABI band of the per-task region, so two tasks renaming, symlinking, writing atomically or making a temporary directory at once share nothing and neither can replace the other's returned path; `COPY-FILE-STREAM`'s descriptors, counts and write cursor are locals of the call. The 8 KiB copy buffer is one set for the image, so `COPY-FILE` and `COPY-FILE-STREAM` are single-task. The cleanup registry is the PROCESS's: registering is safe from any task, because the slot is claimed with one `atomic-add` and the claimer writes only that slot, and `CLEANUP-RUN` belongs to the process. `REMOVE-TREE-IN` walks the caller's context exactly as `WALK-FILES-IN` does, so two tasks remove two trees at once by holding one each, and `REMOVE-TREE` is the single-task form over the same `FS-WALK-CTX0`; the removal claims no `E-FS-WALK-ACTIVE` flag, because `CLEANUP-RUN` removes trees at process exit whatever a task was doing |
| `lib/json-write.f` | caller-owned | the caller declares the writer (`TYPED-VARIABLE W JSON-WRITE:writer`) and the bytes `JSON-WRITE:OPEN` binds it to; no module state |
| `lib/json-read.f` | caller-owned | no module state; the caller allots `JR:STORAGE-BYTES` and owns the source span |
| `lib/memory.f` | caller-owned | every mapping belongs to its caller; `WITH-BYTES`'s scope stack is the one process-wide part |
| `lib/task.f` | task-local | `$20` row: the sleep request and remainder timespecs |
| `lib/process.f` | task-local | `$4A0` row: the NUL-path staging buffer, the three-slot pollfd array and the per-call capture slots (pids, descriptors, lengths, deadline, wait status). `PROC-REAP-ARM` is the one process-wide part: an installed policy, not per-call state |
| `lib/process-command.f` | caller-owned (`CMD`) / process-wide (`PROC-CMD`) | `CMD:COMMAND NAME` declares one command context — a `PTR-U8-TABLE` holding the argv and envp vectors plus the address of its own byte region (path, cwd, argument and environment bytes, 128K stdin, 32K/32K captures) — and every `CMD` word takes that handle, so any number of tasks may each build and run their own command. `PROC-CMD`'s words are the same surface over one static context, so its callers share one command and are single-task. Neither writes the argv/env staging below |
| `lib/process-argv.f`, `lib/process-env.f` | process-wide | the argv table and its byte buffer, the envp-sized environment table and buffer, and the inherited-default table are one set for the image: a task that prepares a spawn through them holds them until the spawn resets them. An owned `CMD` run passes its own vectors to the reset-free spawn cores and touches none of this |
| `lib/process-cwd.f` | process-wide | the working-directory copy buffer is one buffer for the image, used by the process-wide `RUN-ARGV-ENV-CWD-*` path; an owned `CMD` run copies into its own context and calls `SPAWN-ARGV-ENV-CWD-STDIN-CAPTURE-CORE` |
| `lib/net/tcp4.f` | task-local | `$18` row: sockaddr and socklen |
| `lib/net/udp4.f` | task-local | `$18` row: the endpoint, sockaddr and length |
| `lib/net/curl.f` | task-local (`PERFORM` and the option setters) / process-wide (the multiplexed loop) | `$18` row: per-call staging. `CURL:LOOP-START`'s table of 32 transfer records, its fd and ticket tables, its wake pipe and the multi handle are one set for the image, driven by one package-owned task; a record is claimed atomically and every other record access runs under one `TASK:FACILITY`, so any number of tasks may `START` and `AWAIT` at once |
| `lib/serial.f` | task-local | `$58` row: termios and saved termios |
| `lib/genio.f` | task-local (current device, scratch, line) / process-wide (the device table) | the input and output indices are per-task DATA cells `TASK-REGION-INIT` copies; the rows and their eight operations are shared |

### Which mechanism a task-local library uses

Which of the two a library reaches for is forced, not preferred:

- A module the ENGINE BAKES, or one the engine's own BUILD loads, takes a
  **declared band** in `src/habu/layout.f` and addresses it as
  `data-base <off> +`, the way `lib/ffi-abi.f` addresses `FFI-BUF` and package
  `GENIO-ABI` its device cells. `lib/string.f` has no choice: it sits below
  `lib/ffi-abi.f`, which requires it back, so `require lib/task.f` there fails
  the engine build with an undefined `STR=` inside ffi-abi's `TOK-IS?`.
  `lib/fmt.f` could have taken rows - it has no cycle, and an engine built that
  way compiles - but `src/habu/habu2.f` requires it for number text, so a
  require would have pulled pthread, mmap and the FFI staging tables into the
  base image for 52 bytes of scratch. `lib/fs.f` and `lib/fs-mutate.f` are not
  baked at all, but `tools/native-build-core.f` requires both to BUILD the
  engine, so a require there loads the task runtime into the build tool ahead
  of the target.
- A module LOADED LATER takes **`TASK:+USER` rows** out of USER-BAND, the way
  `lib/net/tcp4.f`, `udp4`, `curl`, `serial` and `genio` do.

A band a SOURCE-LOADED module reads costs a bootstrap the baked ones do not.
The PRODUCT IMAGE `tools/native-build.f` emits - what `bin/hb` is, and what
builds the next engine - carries this file's constants and `lib/errors.f`'s
codes from the tree it was BUILT in and does not re-read either; only the small
checked engine that `tools/build-fixpoint-refresh.f -- install` writes recompiles
the cold prefix at every launch, and that engine cannot run the native build on
this base. So an engine that predates a band cannot compile the module reading
it. `lib/fs.f` is the first such module: its build tool dies `E-UNDEFINED:
FS-ABI:STAT-BYTES`, rc 70, before any target work. `lib/fs-mutate.f` is the
second, and dies the same way on `FS-MUT-ABI:PATHZ2-BYTES`. Build one engine
from the layout and error declarations alone, build the tree with that engine,
and the third generation is byte-identical.

### The arena a task-local row comes from

`TASK:+USER` hands out offsets from `USER-BAND:START` ($5300) up to
`USER-BAND:END` — **6024 bytes for the whole image**. The band is one declared
run of the per-task header with no engine cell inside it, which
`src/habu/data-claims.f`'s assertion checks at engine build time, and
the declared bands directly above it are `FS-MUT-ABI` (3080 bytes), `FS-ABI`
(1328 bytes), `FMT-ABI` (56 bytes) and `STRING-ABI` (1032 bytes), none of which
is part of it.

The libraries above claim 1696 of those 6024 bytes when one image loads them
all — `lib/process.f`'s $4A0 row is the large one — so **4328 bytes are free**. A row
that would cross
`USER-BAND:END` is `E-TASK-USER` at its definition, not a store into whatever
lies above. Budget accordingly.

## Atomics

Shared cells used across tasks must be 8-byte aligned. `atomic@`, `atomic!`,
`atomic-add`, `atomic-cas`, and `fence` are native AArch64 primitives and are
checked in `src/core/checker.f`. Unaligned atomic cells can fault on LSE
hardware; align dictionary cells before sharing them.

## Sleeping

`TASK:SLEEP ( ms -- )` parks the calling task for at least `ms` milliseconds, in
`nanosleep` rather than in a loop. It is what a task that must wait a fixed time
uses: a `TASK:PAUSE` loop against `mono-ns` burns a core for the whole wait and
this burns none.

```forth
require lib/task.f

50 >MS TASK:SLEEP                     \ from the main task or from any worker
```

- The duration is `ms`, the role, not a bare cell, so a nanosecond count or a
  byte count cannot be passed by accident. Zero returns without entering the
  kernel; a duration below zero is `E-TASK-SLEEP-MS`, because no sleep serves it.
- `nanosleep` is declared with `FUNCTION:` over `PROCESS-SYMBOLS` beside the
  semaphore bindings, so package FFI owns its symbol resolution and its bounded
  staging and this module states only the prototype and the timespec extent.
- A signal cuts the sleep short and `nanosleep` reports `EINTR` with the time it
  did not serve, in its second timespec. The retry asks for exactly that
  remainder, so no signal can shorten the sleep; the remainder is capped by what
  is left of an absolute `mono-ns` deadline taken before the first call - the
  shape `lib/process.f` states as `PROC-DEADLINE-AT` and `PROC-LEFT-MS`, in
  nanoseconds - so no storm of signals stretches the total either. Any other
  errno is `E-TASK-THREAD`.
- Storage class: task-local. The request and the remainder are one 32-byte
  `TASK:+USER` row, so the main task and every worker may be asleep at the same
  moment, each over its own pair of timespecs.
- A sleeping task holds nothing: the row is its own, and the argument tables the
  call stages sit in its own DATA region, exactly as a blocked `TASK:WAIT`'s do.
- It observes no `TASK:HALT` until it wakes, for the same reason a task parked in
  `TASK:WAIT` does not. `TASK:KILL` on a sleeping task therefore blocks for the
  rest of the sleep and joins it at the next `TASK:PAUSE` - but unlike a waiter,
  a sleeper needs nothing to release it, because the duration does that.

## Semaphores

`TASK:SEMAPHORE` defines a counted semaphore over Linux's unnamed POSIX
semaphores or Darwin's Mach semaphores,
taking the SwiftForth word names and the VFX counted semantics
([tasking-models.md](tasking-models.md) sections 1 and 3). It is the blocking
primitive the facility and `TASK:PAUSE` do not provide: a task that waits for
work calls `TASK:WAIT` and the kernel parks it, so no PAUSE loop burns a core.

```forth
TASK:SEMAPHORE ITEMS                  \ ITEMS ( -- TASK:sem )

0 ITEMS TASK:SEMAPHORE-INIT           \ empty
\ producer                            \ consumer, inside a task
1 SLOT ! ITEMS TASK:SIGNAL             ITEMS TASK:WAIT SLOT @
ITEMS TASK:SEMAPHORE-DESTROY
```

- The handle is `TASK:sem`, a nominal cell, and it is the only thing the
  definition publishes. The family is a `NEWTYPE` and its two `CAST:` converters
  are defined in package TASK's private section, so the crossing between an
  address and a handle exists only inside this package: no caller can mint a
  handle over memory of its own, and none can project a handle back to an
  address. The conversion costs no instructions.
- A handle therefore always names a record package TASK owns - one a definition
  allotted, one of a task's two mailbox records, or one from the pool below.
- The type is the first line of defence: a bare `( ptr a )` is refused at the
  call site, so the record address a `TASK:FACILITY` also has cannot reach
  `sem_wait`.
- `TASK:SEMAPHORE-INIT` takes the initial count. A count below zero or above
  `SEM_VALUE_MAX` is `E-TASK-SEM-COUNT`, and initializing a live semaphore again
  is `E-TASK-SEM-STATE`.
- Under the type, the record's first cell is a guard holding the record's OWN
  address exactly while the POSIX object behind it is live. `TASK:WAIT` and
  `TASK:SIGNAL` read that guard first, so a semaphore that was never
  initialized and one that was destroyed both throw `E-TASK-SEM-STATE` instead
  of entering `sem_wait` on memory POSIX does not define.
- `TASK:SEMAPHORE-DESTROY` clears the guard before it destroys the object, and
  destroying an inactive semaphore is a no-op. POSIX leaves destroying a
  semaphore that still has blocked waiters undefined, so the owner signals its
  waiters out first.
- A waiting task is parked in the host call, so it observes no `TASK:HALT` until
  something signals it. `TASK:KILL` on a task blocked in `TASK:WAIT` will not
  return; signal the task, then kill it.
- Darwin stores a Mach semaphore port in the record and destroys it with the
  owning task port. Wait retries `KERN_ABORTED`; try-wait uses a zero
  `mach_timespec_t` and treats `KERN_OPERATION_TIMED_OUT` as an empty count.
  This avoids Darwin's unsupported `sem_init` and preserves destruction after
  consuming an initial count. The ABI is declared in the SDK's
  `mach/semaphore.h` and `mach/task.h`; see Apple's
  [synchronization primitives](https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/KernelProgramming/synchronization/synchronization.html).
- A signal delivered while a task is parked interrupts `sem_wait` with `EINTR`
  and consumes no count, so `TASK:WAIT` retries; any other failure is
  `E-TASK-THREAD`.
- `TASK:NEW-SEMAPHORE` answers a handle over a record from a fixed pool this
  package owns, for a caller that needs a semaphore at run time instead of
  defining one; the pool is `TASK-SEM-POOL-N` records and a request past that is
  `E-TASK-SEM-POOL`. The record arrives uninitialized, exactly like a defined
  one, so the caller still chooses the count with `TASK:SEMAPHORE-INIT`.
- `TASK:FREE-SEMAPHORE` destroys the semaphore if it is still live and returns
  the record to the pool, so a recycled record never carries a POSIX object into
  its next owner. A handle that did not come from the pool - a defined semaphore,
  a mailbox record - is `E-TASK-SEM-POOL`. Claiming a record is one atomic step,
  so two tasks asking at the same moment are handed different records.
- `sem_init`, `sem_wait`, `sem_post` and `sem_destroy` are declared with
  `FUNCTION:` over `PROCESS-SYMBOLS` in package TASK's private section, so
  package FFI owns their symbol resolution and their bounded staging and this
  module states only the prototypes and the `sem_t` extent. The pthread
  bindings beside them still hand-stage through `TRUSTED:` and are retired by
  `habu-ptx-m1-c-1df1d6e7`.

## Messages

Every task owns a one-cell mailbox in its TCB, taking VFX's words and semantics
([tasking-models.md](tasking-models.md) section 3) with the semaphores above in
place of VFX's `PAUSE` loop. The cell, the sender's TCB and the pending flag sit
in the TCB behind the throw slot; the two semaphores beside them are created with
the task and destroyed when it ends.

| Word | Effect | Blocks |
| --- | --- | --- |
| `TASK:SEND-MESSAGE` | `( n ptr a -- )` | while the target still holds an unread message |
| `TASK:GET-MESSAGE` | `( -- n ptr a )` | until this task's mailbox holds a message |
| `TASK:MSG?` | `( ptr a -- bool )` | never |

```forth
: WORKER ( -- )                       \ inside a task
   TASK:GET-MESSAGE {: msg from :}    \ parks in the kernel until a message
   msg 1 + from TASK:SEND-MESSAGE ;   \ the sender's TCB is the reply address
```

- The mailbox holds ONE unread message. A second send blocks in the sender until
  the target's `TASK:GET-MESSAGE` frees the cell, so many senders queue on one
  target in the order the slot semaphore releases them.
- `TASK:GET-MESSAGE` answers the message and the TCB of the task that sent it,
  which is the address a reply is sent to.
- Both ends of a send are tasks. A message names its sender and the main thread
  has no TCB, so a message operation from a thread without one is
  `E-TASK-MAILBOX`. Use a queue (below) where the main thread is one of the ends.
- Sending to a task that is not running is `E-TASK-MAILBOX`: nothing would read
  the message. Sending to the sending task is `E-TASK-MAILBOX` too; it could only
  wait for a get that task is not making.
- `TASK:MSG?` is a snapshot of the pending flag, true from the moment a send
  deposits a message until the get that takes it clears it. It never blocks and
  never throws, so a task that was never activated simply holds no message.
- The mailbox is created by `TASK:PREPARE` and destroyed with the task's memory,
  so messages do not survive a `TASK:KILL` and a reactivated task starts with an
  empty mailbox. `TASK:PREPARE` creates the mailbox through the same host
  semaphore backend as explicit semaphores.
- Ending a task ends its mailbox, and POSIX leaves destroying a semaphore with
  blocked waiters undefined: end a task's senders before the task, exactly as the
  semaphore section requires of its waiters.

## Queues

`lib/queue.f` is the other channel: a bounded ring of cells in package `QUEUE`,
for any number of producers and consumers, taking the blocking put and get of
VFX's `CQueues` ([tasking-models.md](tasking-models.md) section 3) over the
semaphores above instead of its `PAUSE` loop, and cells instead of bytes.

```forth
require lib/queue.f

$10 QUEUE:QUEUE JOBS                  \ JOBS ( -- QUEUE:queue ), 16 cells
JOBS QUEUE:INIT
\ producer, any task                  \ consumer, any task
7 JOBS QUEUE:PUSH                      JOBS QUEUE:POP
JOBS QUEUE:DESTROY
```

| Word | Effect | Blocks |
| --- | --- | --- |
| `QUEUE:QUEUE` | `( n -- )` | defines a queue of n cells |
| `QUEUE:INIT` | `( QUEUE:queue -- )` | no |
| `QUEUE:DESTROY` | `( QUEUE:queue -- )` | no |
| `QUEUE:PUSH` | `( n QUEUE:queue -- )` | while the queue is full |
| `QUEUE:POP` | `( QUEUE:queue -- n )` | while the queue is empty |
| `QUEUE:TRY-PUSH` | `( n QUEUE:queue -- bool )` | never; false when full |
| `QUEUE:TRY-POP` | `( QUEUE:queue -- n bool )` | never; false and zero when empty |
| `QUEUE:COUNT` | `( QUEUE:queue -- n )` | no |

- The capacity is a count of cells, not a mask: the ring wraps with a remainder,
  so any capacity of one or more is legal and a power of two is not required. A
  capacity below one is `E-QUEUE-OPERAND` at the definition.
- The handle is `QUEUE:queue`, a nominal cell, and it is an INDEX into the
  package's table of records rather than a record address. So a cell that names
  no definition is `E-QUEUE-OPERAND` at the first word that reads it, and no
  handle can ever name storage the package did not allot - the substitute for
  a record address it could dereference. One image holds `QUEUE:MAX-QUEUES` (256)
  queue definitions; past that a definition is `E-QUEUE-TABLE`.
- `QUEUE:INIT` opens the semaphores and empties the ring; every other word needs
  a live queue and throws `E-QUEUE-STATE` otherwise, including a second
  `QUEUE:INIT`. `QUEUE:DESTROY` on an inactive queue is a no-op.
- Two counting semaphores carry the exact state: free slots and items. A
  producer takes a free slot before it writes and a consumer takes an item before
  it reads, so `QUEUE:TRY-PUSH` and `QUEUE:TRY-POP` are that same take without
  the block and refuse exactly when the ring is full or empty.
- A third, binary semaphore is the lock over the head and tail indexes, so the
  slot is filled before the item semaphore announces it: two producers never
  claim one slot and a consumer never reads a slot a producer has claimed but not
  yet written. It is held for the index move and the one cell copy, never across
  a block, and nothing in between can throw.
- All three come from `TASK:NEW-SEMAPHORE`, so a live queue holds three pool
  records and an idle one holds none; the handles live in a typed row of
  `TASK:sem`, never as bare cells, and `QUEUE:DESTROY` gives them back. A
  `QUEUE:INIT` that cannot take all three returns the ones it took and throws
  `E-TASK-SEM-POOL`, so a refused queue leaks no record.
- Elements are cells. The queue copies the cell and nothing else; a pointer
  pushed through it stays the sender's to keep alive.
- `QUEUE:COUNT` is the count at the moment it is asked, for reporting rather than
  for a decision another task can invalidate; use `QUEUE:TRY-PUSH` or
  `QUEUE:TRY-POP` to act on fullness or emptiness.
- The same POSIX rule as the semaphores: end a queue's waiters before
  `QUEUE:DESTROY`.
- `lib/queue-test.f` covers the ring shapes, the wrap, the refusals, a push that
  blocks until a pop and a pop that blocks until a push, an arming the pool
  refuses part way, and a soak in which four producers and two consumers move 256
  elements through an 8-cell ring and every element arrives exactly once. The two
  definition-time refusals run in a child engine, because a definer's throw
  aborts the load that carries it. The full test suite runs it as `bounded-queue`.

## Invariants

- Tasks execute XTs only; they do not interpret source and do not compile.
- New definitions, `create`, `variable`, `constant`, `defer`, `cp!`, `ndict!`,
  and other dictionary/code mutation paths are invalid while tasks are live.
- Ordinary `variable` storage is shared process storage. Use `TASK:+USER` for
  task-local state and `TASK:HIS` to inspect another task's user cell before
  releasing that task. Every library states which of the three storage classes
  above it belongs to, in its own header and in the table above.
- A new task starts on its creator's input and output devices
  ([genio.md](genio.md)); `TASK-REGION-INIT` copies the routing indices and the
  device table into the new region.
- The task trampoline preserves the shared dictionary/code registers and swaps
  the data stack and data/user base for the worker.
- `TASK:FACILITY` is owner-tracked pthread mutex storage, not a spin lock.
  `TASK:GET` is idempotent for the owning task; `TASK:RELEASE` is a no-op for a
  non-owner or an already-free facility.
- A facility excludes, a semaphore counts. Hold a facility across shared updates;
  wait on a semaphore for work to exist. Neither is a substitute for the other.
- A mailbox is a rendezvous between two tasks and carries the sender's identity;
  a queue buffers between any number of producers and consumers and carries none.
  Neither is a substitute for the other.
- Every ending of a task - returned, threw, halted at `TASK:PAUSE` - runs the
  registered cleanup in the task's own thread and then signals the join, in that
  order. A task's outcome rows and its cleanup registration live in the TCB, which
  outlives the thread's memory; its stacks, region, mailbox and done semaphore go
  with that memory when a join or a kill releases it.
- A released TCB holds no address this process took: the thread handle, the
  join's answer and the data base, record count and code end `TASK:PREPARE`
  recorded are cleared with the memory, and the next prepare writes them again.
  That is what lets a program that ran a task while it loaded be captured -
  `hb-build`'s stripped link refuses a captured cell holding an address only the
  building process ever held.
- A capture runs `IMAGE-LIFECYCLE:PREPARE`, where `lib/task.f` sweeps every TCB
  the program declared: a prepared task is released and left EMPTY, and its next
  `TASK:ACTIVATE` prepares again, while a task still activated ends the capture
  with `task: activated task at capture` - no image carries a thread. Kill your
  tasks before a build or a snapshot captures.

## Tests

Run:

```sh
bin/hb --load lib/task-test.f
bin/hb --load lib/queue-test.f
bin/hb --load test/atomics-smoke.f
bin/hb --load test/run-in-stack-smoke.f
bin/hb --load lib/process-task-test.f
```

`lib/process-task-test.f` is the storage-class test for a library rather than
for the tasking primitives: one task runs 40 children through
`PROC-CMD:RUN-OUTCOME` while the main thread polls its own ready pipe through
`POLL-IN`, and every child and every poll must answer correctly inside a 20 s
budget. The full test suite runs it as `process-tasks`.

`lib/task-test.f` covers two pthread workers, facility-protected shared updates,
task-local `TASK:+USER` isolation via `TASK:HIS`, `TASK:SELF`, `TASK:HALT` /
`TASK:KILL`, facility owner semantics, a five-task application-shaped repeated
start/join soak, FFI from worker tasks, task-local FFI scratch isolation, the
live-task compile guard, process-fatal worker `die`, a contained worker `throw`
beside a worker that completes and is joined, a producer/consumer whose consumer
blocks in `TASK:WAIT` without a PAUSE loop and reads 64 items in order, an
initial count drawn without any signal, every named semaphore failure, a message
round trip between two tasks, a send that blocks until its target gets, a get
that blocks until a send, `TASK:MSG?` before and after a get, every refused
message operation, the semaphore pool exhausting, refusing a foreign handle
and recovering, a joined value and a joined throw, a worker that ends without
answering, a cleanup counted through `TASK:HIS` on both the returning and the
throwing ending, a cleanup that throws over a stored value and beside a body that
already failed, a halted task joined through the `TASK:PAUSE` ending, every
refused join and second answer, and a 50 ms `TASK:SLEEP` measured from the main
task and from a worker - wall time against the requested duration and the
sleeping task's own `RUSAGE_THREAD` CPU time against zero, so a sleep that spun
would fail - beside the refused negative duration, a counting task that ticks
inside a sleeper's 250 ms, and a `TASK:KILL` that waits one out.
The full test suite includes these as `tasking-primitive-smoke` and
`tasking-threads`.
