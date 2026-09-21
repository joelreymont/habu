# Task models in SwiftForth, polyFORTH and VFX Forth

What three mature Forth systems provide for tasks and inter-task communication,
restated in our words so Habu's `TASK` package can adopt proven names and
semantics instead of inventing them. The manuals themselves are not
reproduced: SwiftForth's evaluation licence forbids redistribution and the
polyFORTH reprint is licensed for GreenArrays chip development. Section and
page numbers below point into the local copies.

Sources, local copies as of 2026-09-16 (outside this repository):

- SwiftForth 4.1.10 for Linux, released 2026-08-05, evaluation package:
  `~/Downloads/forth-inc/swiftforth/SwiftForth/`. Reference manual
  `doc/SwiftForth-refman.pdf` chapter 7 "Multitasking" (pp. 53-57). Kernel
  source `src/ide/common/tasking.f`, `src/ide/tasking.f`,
  `src/ide/common/semaphore.f`; sample `lib/samples/echo-server.f`.
- polyFORTH Reference Manual, GreenArrays reprint DB005 revised 2012-08-25:
  `~/Downloads/forth-inc/polyforth-refman.pdf` chapter 4 "Multitasking"
  (pp. 127-139), especially 4.2, 4.5 and 4.7.
- SwiftX ARM evaluation (installer of 2024-08, Inno Setup 6.6.1, unpacked
  with innoextract pull request 214): `~/Downloads/forth-inc/swiftx/app/SwiftX/`.
  Reference manual `doc/swiftx.pdf` chapter 5 "The SwiftOS Multitasking
  Executive" (pp. 73-94); `doc/swiftx-arm.pdf` section 3.2.3 (p. 32); source
  `src/arm-cm/tasker.f`, `src/arm/linux/tasker.f`, `src/arm/linux/threads.f`,
  `src/vio.f`; application note `~/Downloads/forth-inc/SwiftX-LED-Demo.pdf`.
- VFX Forth 64 for x64 Linux, community edition dated 2023-11-09:
  `~/Downloads/forth-inc/mpe/x64/VfxForth64Lin/`. Manual `Doc/VfxLin64.pdf`
  chapter 18 "Multitasker" (p. 207). Source `Lib/Lin64/MultiLin64.fth`,
  `Lib/CQueues.fth`, `Examples/taskdemo.fth`.
- Habu: [threads.md](threads.md), [swiftforth-task-api.md](swiftforth-task-api.md),
  `lib/task.f`.

## 1. SwiftForth: hosted tasks on pthreads

A task is a pthread with a private user area (user variables, TIB, PAD and an
optional private dictionary). `n TASK name` defines the task control block at
compile time. `name ACTIVATE body ;` inside a colon definition starts the
thread: the rest of the definition is the task body, passed through the task's
`'CFA` user variable. The thread entry sets up the stacks, TIB, dictionary
pointer and FPU, then runs `'CFA @ CATCH ?DUP IF CAUGHT THEN 0 pthread_exit`:
an uncaught throw is reported through the system's error display and ends
only that thread. The process and the other tasks continue.

| Word | Effect | Meaning |
| --- | --- | --- |
| `TASK` | `( u -- )` name | TCB: user-area pointer, thread handle, dictionary size, link |
| `CONSTRUCT` | `( tcb -- )` | allocate the user area and dictionary once |
| `ACTIVATE` | `( tcb -- )` | halt, construct, `pthread_create` with the rest of the definition |
| `HALT` | `( tcb -- )` | `pthread_cancel` and join; memory kept for reactivation |
| `KILL` | `( tcb -- )` | halt, then free the memory |
| `PAUSE` | `( -- )` | `pthread_testcancel` then `sched_yield`: the cancel point and the yield |
| `STOP` | `( -- )` | `pthread_exit` of the caller |
| `#USER`, `+USER` | | user-variable allocation |
| `HIS` | `( tcb uvar -- addr )` | another task's copy of a user variable |
| `GET`, `RELEASE`, `GRAB` | `( a -- )` | facility variable: atomic compare-and-store spin with `PAUSE`; owner is the task's `STATUS` address; `GRAB` skips the initial `PAUSE` |
| `[C`, `C]` | `( -- )` | one global critical section (a pthread mutex) |
| `SEMAPHORE` class: `NEW`, `WAIT`, `SIGNAL`, `DELETE` | | POSIX named semaphore (`sem_open`, `sem_wait`, `sem_post`), count starts at 0; added 2025 |

Communication is shared memory: global variables, `HIS` to read or write
another task's user variables, facility variables for exclusion, and the
semaphore for a blocking wait. There are no message or queue words. The
echo-server sample runs the accept loop in one task and wraps each
connection's service in `CATCH DROP`, so a connection error returns to
`/ACCEPT`.

## 2. polyFORTH and SwiftX: the cooperative round robin

About thirteen words. Each task's `STATUS` cell holds either a jump to the
next task or the `WAKE` instruction; the tasks form a ring of jumps, "the
PAUSE loop". Control changes only at `PAUSE`, `STOP` or the assembler ending
`WAIT`, always between Forth words, so a switch saves only the interpreter
pointer, return pointer and stack pointer. Every I/O word ends in `WAIT`, so
the CPU serves other tasks while a device works: the multitasker is "I/O
driven". A task with no I/O calls `PAUSE` inside long loops.

| Word | Effect | Meaning |
| --- | --- | --- |
| `PAUSE` | `( -- )` | store `WAKE` in own `STATUS`, save state, run the ring; resumes next turn |
| `STOP` | `( -- )` | like `PAUSE` without the `WAKE`: sleep until another task or an interrupt stores `WAKE` into this task's `STATUS` |
| `WAIT` | code ending | assembler form of `STOP` |
| `BACKGROUND` | `( u s r -- )` name | SwiftX: define a background task with user, data-stack and return-stack sizes |
| `BUILD` | `( tcb -- )` | link into the ring, asleep |
| `ACTIVATE` | `( tcb -- )` | empty the slave's stacks, point its interpreter at the code after `ACTIVATE`, store `WAKE`; the master leaves the definition |
| `HALT` | `( tcb -- )` | the idiom `ACTIVATE STOP` |
| `GET`, `RELEASE` | `( a -- )` | facility variable holding the owner's `STATUS`; `GET` busy-waits in the ring; no deadlock detection, rule: hold one facility at a time |
| `HIS` | `( tcb uvar -- a )` | another task's user variable |

Communication is shared variables plus wake-by-status-cell: a producer stores
its data, then stores `WAKE` into the consumer's `STATUS`; the consumer loops
`BEGIN ACCEPT REDUCE STORE STOP AGAIN`. The status cell is at once the
scheduler link and the wake flag. This is the model for targets without an
operating system.

### 2.1 SwiftX specifics

SwiftX is FORTH, Inc.'s cross compiler; its target executive SwiftOS is the
polyFORTH model above, and the reference manual's chapter 5 is the current
statement of it. What the manual and the ARM sources add:

- `STATUS` is two cells: a flag, `WAKE` or `SLEEP`, then `FOLLOWER`, the
  absolute address of the next task's `STATUS`. On Cortex-M `PAUSE` is about
  ten instructions: store `WAKE`, push the return context, save the stack
  pointer, walk `FOLLOWER` until a `STATUS` holds `WAKE`, store `SLEEP` for
  the resumed task, restore its stacks. The ARM target manual states fewer
  than six instructions to suspend, resume or skip a task. `STOP` branches
  into the same code past the `WAKE` store; `WAIT` is that entry's label for
  code definitions. The whole Cortex-M tasker, with facilities and task
  building, is 142 lines.
- A task is a compile-time table: `nu ns nr BACKGROUND name` reserves the
  user area, data stack and return stack sizes; `n TERMINAL name` adds the
  terminal user variables, input buffer and a private dictionary of `n`
  bytes. At power-up `BUILD` (background) or `CONSTRUCT` (terminal) links
  the task after `OPERATOR` in the ring, copies the operator's user area and
  sets `S0`; the task then sleeps until `ACTIVATE`. `ACTIVATE` empties the
  slave's stacks, points its return stack at the code after `ACTIVATE`,
  stores `WAKE`, and returns the master from the definition. `NOD` is
  `BEGIN STOP AGAIN`; `HALT` is `ACTIVATE NOD`.
- The user variables every task needs are `STATUS`, `FOLLOWER`, `SSAVE`,
  `S0` and `CATCHER` (the current exception frame): about five cells.
  Terminal tasks add `DEVICE`, `BASE`, the I/O vectors and the input state.
  `+USER ( n1 n2 -- n3 )` threads the offset; `#USER` marks the end; `HIS`
  is `STATUS - SWAP @ +`.
- Interrupt drivers do the time-critical work, then store `WAKE` into the
  waiting task's `STATUS`; when several tasks share a device, a facility
  variable both serialises access and names the task to wake. `GRAB` is the
  raw acquire (spin on `PAUSE` until free or owned), `GET` is `PAUSE GRAB`,
  `RELEASE` clears the facility only for its owner.
- The ARM Linux target keeps this cooperative tasker and adds
  `threads.f`: pthreads with `THREAD`, `THREADS`, `HIS`, `YIELD`, `MUTEX`
  with `LOCK`/`UNLOCK`, `CONDITION` with `CONDITION-WAIT`/`CONDITION-SIGNAL`,
  and `[C`/`C]` over one critical mutex. That is the blocking primitive the
  hosted side needs and the cooperative side does not.
- Terminal I/O is vectored per task through user variables (`'EMIT`,
  `'TYPE`, `'CR`, `'PAGE`, `'ATXY`, `'KEY`, `'KEY?`, `'ACCEPT`, `'STRAIGHT`)
  and `@EXECUTE` (`src/vio.f`, 57 lines). This is the embedded form of the
  generic I/O device in [socket-models.md](socket-models.md): a table of
  vectors small enough for a microcontroller.

## 3. VFX Forth: pthreads behind the MPE embedded API

VFX schedules pre-emptively on pthreads but keeps the cooperative API of the
MPE cross compilers, so embedded and hosted programs share source. The TCB
holds: link, thread handle, user pointer, message-pump xt, status bits
(running, message pending, event triggered, event run), one message cell, the
sender's TCB, event xt, cleanup xt and a halt semaphore. `PAUSE` is where the
MPE layer runs: exit if termination was requested, call the message pump,
`sem_wait` on the halt semaphore (this is how `HALT` and `RESTART` block
without polling), run a triggered event handler, then `sched_yield`.

| Word | Effect | Meaning |
| --- | --- | --- |
| `TASK` | name `( -- tcb )` | define a TCB |
| `INITIATE` | `( xt tcb -- )` | start the thread on `xt`; waits until its user area exists |
| `HALT`, `RESTART` | `( tcb -- )` | suspend and resume through the halt semaphore |
| `STOP` | `( -- )` | halt self |
| `TERMINATE` | `( tcb -- )` | set the task's exit flag; it exits at its next `PAUSE` after cleanup |
| `AtTaskExit` | `( xt tcb -- )` | cleanup action; a task body may return an exit code |
| `SELF` | `( -- tcb )` | |
| `HIS` | `( tcb uvar -- a )` | |
| `SINGLE`, `MULTI` | `( -- )` | disable or enable the MPE layer, not the OS scheduler |
| `SEND-MESSAGE` | `( msg tcb -- )` | one-cell mailbox in the target TCB; the sender blocks while the target holds an unread message; sets the message bit and restarts the target |
| `GET-MESSAGE` | `( -- msg tcb )` | block until the message bit is set; returns the message and its sender |
| `MSG?` | `( tcb -- flag )` | |
| `TO-EVENT`, `SET-EVENT`, `CLEAR-EVENT`, `EVENT?` | | software interrupt: an xt run inside the target task at its next `PAUSE`; must have no net stack effect |
| `WAIT-EVENT/MSG` | `( -- )` | block until either arrives |
| `SEMAPHORE`, `InitSem`, `REQUEST`, `SIGNAL` | | counted semaphore with an owner field; `REQUEST` locks and decrements, `SIGNAL` increments and unlocks |
| `CQUEUE:`, `>CQUEUE`, `CQUEUE>`, `?>CQUEUE`, `CQFULL?`, `CQEMPTY?`, `CQCHARS` | | power-of-two byte ring queue; blocking put and get spin with `PAUSE` |

The thread entry does not catch; the demo wraps each handler in `CATCH`
itself. Blocking in the message and queue words is a `PAUSE` loop, which VFX
notes as a CPU hog when no delay is involved.

## 4. Habu today

`lib/task.f` provides `TASK:TASK`, `PREPARE`, `ACTIVATE` (takes an xt),
`PAUSE`, `HALT` (cooperative, observed at `PAUSE`), `KILL` (join and free),
`SELF`, `+USER`, `HIS`, `FACILITY` with `GET` and `RELEASE` (owner-tracked
pthread mutex) and atomics. The runner catches the worker xt; since commit 9de16800 an uncaught throw
is recorded in the task control block (`TASK:THROW@`) and ends only that task. There is no semaphore, message,
event, queue or result. Compilation is forbidden while tasks live.

## 5. What to adopt

| Gap | Model to follow | Dot |
| --- | --- | --- |
| A worker throw ends the process | SwiftForth trampoline: catch, record, end the thread only | `habu-contain-a-worker-fe0c8eb8`, done in 9de16800 |
| Waiting means polling | SwiftForth `SEMAPHORE` names, VFX counted semantics, SwiftX-ARM Linux condition variables; also gives blocking `HALT`/`RESTART` | `habu-add-a-blocking-fd79b713` |
| No result from a worker | VFX exit code and `AtTaskExit`, returned as `result<ok,err>` from a typed join | `habu-return-a-typed-b1c342cd` |
| No channel between tasks | VFX one-cell mailbox words and a typed bounded queue in the `CQueues` shape, blocking on the semaphore | `habu-add-task-msgs-c5a6af71` |
| No tasks without an OS | the SwiftX Cortex-M tasker: `STATUS`/`WAKE` ring in 142 lines, with the same public names as the hosted package, as VFX does | `habu-decide-the-cooperative-b463cc1c` |
| Events | VFX `SET-EVENT`/`TO-EVENT` | later, after the queue |

Keep Habu's package prefix `TASK:` and typed handles; take the product word
names for the operations above.

## Decision (proposed 2026-09-16)

For `habu-decide-the-cooperative-b463cc1c`. One public surface, two kernels:

| Word | Hosted (pthreads) | Target (round robin) |
| --- | --- | --- |
| `TASK`, `PREPARE` (build), `ACTIVATE` (xt), `SELF`, `+USER`, `HIS` | yes | yes |
| `PAUSE` | cancel point and `sched_yield` | store `WAKE` in own `STATUS`, run the ring |
| `STOP`, `WAKE ( tcb -- )` | landed: block on the task's own park semaphore in its TCB; `WAKE` posts it, and `WAKE` of the null TCB posts the main thread's one park ([threads.md](threads.md) "STOP and WAKE") | store `SLEEP`/`WAKE` in `STATUS` |
| `FACILITY`, `GET`, `RELEASE`, `GRAB` | owner-tracked mutex | owner in the facility cell, spin with `PAUSE` |
| `SEMAPHORE`, `WAIT`, `SIGNAL` | POSIX semaphore | counter plus `STOP`/`WAKE` of the first waiter |
| `SEND-MESSAGE`, `GET-MESSAGE`, `MSG?` | one-cell mailbox blocking on the semaphore | one-cell mailbox blocking with `STOP` |
| `HALT` | request stop, observed at `PAUSE` | `ACTIVATE` of `NOD` |
| `KILL`, `JOIN`, `AT-EXIT` | yes | not present: target tasks are eternal |

The representation on arm32 and tic6x is SwiftX's: `STATUS` is two cells,
flag then `FOLLOWER`; `PAUSE` is the short assembly sequence in
[cortex-m.md](cortex-m.md); a driver's blocking word ends in `STOP` and its
interrupt handler stores `WAKE`. Hosted code that uses only the shared rows
compiles for a target unchanged; the hosted-only rows are refused by the
target's checker rows rather than emulated.

Test strategy: the hosted rows keep `lib/task-test.f`; the target rows run
on the QEMU Cortex-M4 peer with the same test words, so one test file
exercises both kernels where the surface is shared.
