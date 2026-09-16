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
code `$4F`, printing the rejected token. Linux fatal exits use `exit_group`
(`94`), not thread-local `exit` (`93`), so an error in any thread terminates the
whole process instead of leaving worker threads behind.

Worker bodies run through a task wrapper. A worker `die` keeps its explicit exit
status and message and still ends the process. An uncaught worker `throw` ends
only that task, as SwiftForth's thread entry does: the wrapper records the throw
code in the task's TCB, the task reaches `DONE`, and the process and the other
live tasks continue. `TASK:THROW@` reads the code; it stays readable after
`TASK:KILL` has joined the task and is cleared by the next `TASK:ACTIVATE`. A
worker outcome richer than one code needs the typed result model; see
[tasking-models.md](tasking-models.md) for how SwiftForth, polyFORTH and VFX
handle this and the dots that change it.

## Public Words

```forth
TASK:TASK            ( n -- )          \ define a task TCB; n is stack bytes
TASK:MIN-STACK       ( -- n )
TASK:PREPARE         ( ptr a -- )      \ allocate task stack/region without starting
TASK:ACTIVATE        ( n ptr a -- )    \ run xt in a pthread-backed task
TASK:SELF            ( -- ptr a )
TASK:SELF-N          ( -- n )
TASK:PAUSE           ( -- )            \ yield; worker exits if HALT requested
TASK:HALT            ( ptr a -- )      \ request stop at next PAUSE
TASK:KILL            ( ptr a -- )      \ join/release task memory
TASK:DONE?           ( ptr a -- bool )
TASK:THROW@          ( ptr a -- n )    \ uncaught throw code, 0 if none

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
TASK:SEMAPHORE-BYTES ( -- n )          \ bytes to embed one in another record
TASK:FACILITY-BYTES  ( -- n )

TASK:SEND-MESSAGE    ( n ptr a -- )    \ post one cell to that task
TASK:GET-MESSAGE     ( -- n ptr a )    \ take this task's message and its sender
TASK:MSG?            ( ptr a -- bool ) \ does that task hold an unread message
```

Use `TASK:KILL` for teardown. A task that loops must call `TASK:PAUSE` or block
in a host call; `TASK:HALT` is cooperative and is observed by `TASK:PAUSE`.

The surface tracks the SwiftForth multitasking words captured in
`docs/swiftforth-task-api.md`. Habu keeps the task body typed by passing an XT to
`TASK:ACTIVATE` instead of parsing a following source body.

## Atomics

Shared cells used across tasks must be 8-byte aligned. `atomic@`, `atomic!`,
`atomic-add`, `atomic-cas`, and `fence` are native AArch64 primitives and are
checked in `src/core/checker.f`. Unaligned atomic cells can fault on LSE
hardware; align dictionary cells before sharing them.

## Semaphores

`TASK:SEMAPHORE` defines a counted semaphore over an unnamed POSIX semaphore,
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
  definition publishes: the child's `does>` body converts the record address to
  the handle, so package TASK exposes no raw-address-to-handle crossing and
  nothing outside it holds the record. The conversion costs no instructions.
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
- Unnamed POSIX semaphores are a Linux facility. Darwin's `sem_init` is a
  deprecated `ENOSYS` stub - which is why SwiftForth opens NAMED semaphores
  there - so `TASK:SEMAPHORE-INIT` throws `E-TASK-SEM-HOST` off Linux. That
  branch is unexercised: the suite runs on Linux, so nothing here has ever
  taken it. Darwin support means `sem_open`, not a fix to this guard.
- A signal delivered while a task is parked interrupts `sem_wait` with `EINTR`
  and consumes no count, so `TASK:WAIT` retries; any other failure is
  `E-TASK-THREAD`.
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
  `E-TASK-MAILBOX`.
- Sending to a task that is not running is `E-TASK-MAILBOX`: nothing would read
  the message. Sending to the sending task is `E-TASK-MAILBOX` too; it could only
  wait for a get that task is not making.
- `TASK:MSG?` is a snapshot of the pending flag, true from the moment a send
  deposits a message until the get that takes it clears it. It never blocks and
  never throws, so a task that was never activated simply holds no message.
- The mailbox is created by `TASK:PREPARE` and destroyed with the task's memory,
  so messages do not survive a `TASK:KILL` and a reactivated task starts with an
  empty mailbox. Because the mailbox is part of a task, `TASK:PREPARE` needs the
  unnamed POSIX semaphores above: on a host without them it throws
  `E-TASK-SEM-HOST` rather than creating a task that cannot receive.
- Ending a task ends its mailbox, and POSIX leaves destroying a semaphore with
  blocked waiters undefined: end a task's senders before the task, exactly as the
  semaphore section requires of its waiters.

## Invariants

- Tasks execute XTs only; they do not interpret source and do not compile.
- New definitions, `create`, `variable`, `constant`, `defer`, `cp!`, `ndict!`,
  and other dictionary/code mutation paths are invalid while tasks are live.
- Ordinary `variable` storage is shared process storage. Use `TASK:+USER` for
  task-local state and `TASK:HIS` to inspect another task's user cell before
  releasing that task.
- The task trampoline preserves the shared dictionary/code registers and swaps
  the data stack and data/user base for the worker.
- `TASK:FACILITY` is owner-tracked pthread mutex storage, not a spin lock.
  `TASK:GET` is idempotent for the owning task; `TASK:RELEASE` is a no-op for a
  non-owner or an already-free facility.
- A facility excludes, a semaphore counts. Hold a facility across shared updates;
  wait on a semaphore for work to exist. Neither is a substitute for the other.

## Tests

Run:

```sh
bin/hb --load lib/task-test.f
bin/hb --load test/atomics-smoke.f
bin/hb --load test/run-in-stack-smoke.f
```

`lib/task-test.f` covers two pthread workers, facility-protected shared updates,
task-local `TASK:+USER` isolation via `TASK:HIS`, `TASK:SELF`, `TASK:HALT` /
`TASK:KILL`, facility owner semantics, a five-task application-shaped repeated
start/join soak, FFI from worker tasks, task-local FFI scratch isolation, the
live-task compile guard, process-fatal worker `die`, a contained worker `throw`
beside a worker that completes and is joined, a producer/consumer whose consumer
blocks in `TASK:WAIT` without a PAUSE loop and reads 64 items in order, an
initial count drawn without any signal, every named semaphore failure, a message
round trip between two tasks, a send that blocks until its target gets, a get
that blocks until a send, `TASK:MSG?` before and after a get, and every refused
message operation.
The full test suite includes these as `tasking-primitive-smoke` and
`tasking-threads`.
