# C callbacks into checked Habu

`lib/ffi-callback.f`, package `FFI-CB`, lets C call a checked Habu word: a
libc `qsort` comparator, a `pthread_create` start routine, a C library's hook.
The engine carries a fixed pool of 16 C entry stubs (`CB-POOL`,
`src/habu/layout.f`). A declaration takes one for the life of the image, and a
binding points it at a *context*: the region whose data stack and dictionary
the body runs on. `lib/task.f` owns contexts and bindings. Only macOS on arm64
has run any of this; see "Untested" below.

```forth
require lib/ffi-callback.f

CALLBACK: CMP ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK

: CMP-IMPL ( ptr u8 ptr u8 -- n ) {: a b :}
   a CELL-VIEW @ b CELL-VIEW @ ORDER ;   \ ORDER ( n n -- n ): -1, 0 or 1
' CMP-IMPL CMP-BODY !

: SORT ( -- )
   CMP TASK:SELF-CONTEXT FFI-CB:ENTRY {: fn:n :}
   ROW BYTE-VIEW 8 CELL fn QSORT         \ ROW: 8 cells; QSORT: libc qsort
   CMP FFI-CB:UNBIND ;
```

`test/stripped-callback-subject.f` is the whole program, nested sort included.

## Declaring

`CALLBACK: NAME ( in -- out ) clauses ;CALLBACK` takes the next free slot and
generates three words:

| word | what it is |
|---|---|
| `NAME ( -- FFI-CB:callback )` | the descriptor |
| `NAME-BODY` | a `TYPED-VARIABLE [ in' -- out' ]` holding the body; `i32` and `u32` read as `n` |
| `NAME-DISPATCH ( -- )` | what the slot runs: it reads the arguments, runs the body under `catch` and leaves the result for C |

Arguments are register arguments only: `n`; `i32`, sign-extended from the low
32 bits; `u32`, the low 32 bits; `ptr u8`, the address C passed, unchecked; and
`r`. At most 8 integer arguments (6 on SysV) and 8 float arguments are allowed;
past that it is `E-FFI-ARITY`. The result is `n`, `i32`, `u32`, `r` or none. A
value result needs `n FALLBACK` (`r FFALLBACK` for `r`), the answer C gets when
the body throws. A pointer result, two results, a missing or mistyped fallback,
`FALLBACK` or `;CALLBACK` with nothing open and `CALLBACK:` inside an open
declaration are all `E-FFI-SYNTAX`, and a refusal abandons the open
declaration. The seventeenth declaration is `E-FFI-CALLBACK-FULL`. The pool is
fixed, no stub is emitted at run time and a slot is never given back. A
declaration defines words, so declare every callback before the first task
starts.

Store the body with `' IMPL NAME-BODY !`. If the body cell was never stored,
the dispatch faults `E-FFI-CALLBACK-STATE` and answers its fallback; it never
executes the zero.

## Binding and contexts

`FFI-CB:ENTRY ( callback n -- n )` binds the slot to context n and answers the
function pointer to hand C: a stub address, never a Habu xt.
`FFI-CB:UNBIND ( callback -- )` releases the slot. A context is a region with at
most one thread inside it at a time:

- `TASK:SELF-CONTEXT ( -- n )`, the calling task's own region (the main region
  on the main task). C calls back on the calling thread, from inside a foreign
  call that task made, as qsort calls its comparator.
- `TASK:CONTEXT ( ptr a -- n )` of an exposed task (below): a region that a
  foreign thread enters.
- `TASK:MAIN-BASE FFI:>CELL`, the main region, named from any task. Only the
  main thread can enter it. Inside a foreign call the main thread holds the
  region's owner, and outside one the region has no frame to enter.

ENTRY publishes the dispatch table and binds the slot, and only then answers
the pointer. Binding an idle slot to the context it already names is a no-op.
`E-TASK-STATE` refuses binding a slot named to another context, binding a slot
that a thread is inside, binding a slot that a bind, unbind, UNEXPOSE or task
end is moving, even one naming that context, and a context number that is none of the three above,
such as a `CONSTRUCTED` task's region. A bind to an exposed task's region reads
the task again after naming the slot and before releasing it, and one that an
UNEXPOSE has overtaken clears the slot and is refused, so no slot is left
naming a region that is no longer exposed. ENTRY and UNBIND call
`TASK:CONTEXT-BIND ( n n -- )` (context, slot) and
`TASK:CONTEXT-UNBIND ( n -- )`.

## The body's contract

The body runs on the context's VM, and `TASK:SELF` inside it answers the
context's task. A body that parks in `TASK:STOP` on an exposed context is woken
by `TASK:WAKE` of that task. A wake no body takes before `TASK:UNEXPOSE` does
not reach the task's next activation. Before an outbound FFI call branches to C, it
publishes the task's data stack cursor, data base, dictionary count and code
pointer into the region's callback band, and the thunk loads them. So a body on
a calling-task context works above the stack its task had when it called C. A
local, a loop index and a stack cell that are live across the foreign call
survive it; the qsort cases check all three.

- A body may call C again. A comparator that runs qsort through its own slot,
  or through another slot on the same context, works. The nested call shares
  its task's FFI scratch ([stdlib.md](stdlib.md#ffi-abi)) with the call it is
  nested in. C already holds that call's register and stack arguments, but a
  scratch area C reads through a pointer, such as an `FFI:KPARAM` table or an
  out-parameter cell, is not safe across a nested call.
- It may not unbind the slot it runs through. UNBIND is `E-TASK-STATE` there,
  and C's call completes.
- It may not keep a `ptr u8` argument past its return. The address is C's.
- It may not define. While any task is live, a definition ends the process
  (exit `$4F`), and every exposed context counts as live. With no task live,
  the thunk compares the dictionary count and code pointer with the ones it
  loaded when the body returns, and a body that moved either ends the process
  with exit 106 and `hb: callback: the body defined`: the thunk restores C's
  callee-saved registers on the way out, and the caller's VM registers with
  them, so the definition would not come back. An `allot` or `,` moves only
  the data pointer, which lives in memory, and survives the return.
- It is halted only after C returns. `TASK:PAUSE` inside a body only yields,
  even with a halt requested, so `TASK:HALT` and `TASK:KILL` of a worker inside
  a callback take effect at its first `TASK:PAUSE` after C's call has returned.

## Faults and fallbacks

The dispatch runs the body under `catch`, so no throw crosses into C: the thunk
has no Habu frame to unwind to. When the body throws, C gets the declared
fallback (a callback with no result just returns) and the data stack depth is
restored. The code is kept for `FFI-CB:FAULT@ ( callback -- n )` until
`FFI-CB:CLEAR ( callback -- )` or the next fault. `FFI-CB:ARG@`, `FARG@` and
`RESULT!` outside a live callback frame are `E-FFI-CALLBACK-STATE`.

## Exit 106

The thunk runs on C's stack with no Habu catch frame between it and C, so a
refusal cannot throw. Every refusal fails closed: `hb: callback: <reason>` on
fd 2, then `exit_group` with `ENGINE-ERROR:CALLBACK` 106. The thunk never
touches a region it could not claim.

| reason | cause |
|---|---|
| `slot out of range` | `callback-entry` was given a slot past `CB-POOL` |
| `no slot has been bound` | C called a stub before any binding in this process |
| `slot held by another thread` | another thread is inside through this slot |
| `slot is not bound` | a call after UNBIND, UNEXPOSE, the end of the task whose region it names, or a capture |
| `context busy on another thread` | another thread holds the region's owner: a second foreign thread on an exposed context, or a foreign thread on the main context while the main thread is inside a foreign call (asleep in `nanosleep` counts) |
| `context is not inside a foreign call` | the region has no published frame: a foreign thread on the main context while the main thread runs Habu code |
| `slot has no dispatch` | the dispatch table was not published, or the slot holds no xt |
| `the body defined` | a body on the main context, with no task live, returned with the dictionary count or code pointer moved |

A second thread on a busy context is refused, never made to wait. An outbound
FFI call refuses in the same way, `context busy on another thread`, if its
thread calls C while another thread holds the region's owner. A task that ends
with one of its slots still held also exits 106 (Lifetime, below). Exit 106 is
also the low byte of an uncaught `E-NBR-RANGE` throw (-8598), which prints
nothing ([gate.md](gate.md)); the `hb: callback:` or `task:` line tells the two
apart.

## Exposed tasks: contexts for foreign threads

`TASK:EXPOSE ( ptr a -- )` turns a task with no run in progress into a
context. It prepares the task, refuses anything but `CONSTRUCTED`, and writes
a resting frame: the base of the task's data stack and the dictionary
`TASK:PREPARE` recorded. A thread that enters with no outbound call in flight
therefore has a VM. The task is then `EXPOSED` (`TASK-ABI:EXPOSED` 5). It runs
no body of its own, and `TASK:ACTIVATE` refuses it. It counts as live on the
main region, as an activated task does, whichever task exposed it. No
definition can move the dictionary under a thread inside it.

`TASK:UNEXPOSE ( ptr a -- )` first moves the task from `EXPOSED` to
`CONSTRUCTED` by compare-and-swap, so a racing bind sees the change. It is then
`E-TASK-STATE`, with `EXPOSED` put back, while a thread holds the region, is
inside through any slot naming it, or a bind is moving such a slot, and the
refusal leaves every binding as it was. The refusal puts `EXPOSED` back by
compare-and-swap: a state that moved meanwhile means a second thread is running
the task's lifecycle, and the process ends with exit 236 and
`task: a task moved under its refused UNEXPOSE (TCB.STATUS)`. Otherwise it drops the bindings naming
the region, clears the region's band and gives the live count back. A bind or
`TASK:CONTEXT` that runs meanwhile finds the task `CONSTRUCTED` and is refused,
even when the UNEXPOSE itself is then refused. It claims every slot naming the
region before it drops one, so a refusal hands every slot back, and a thread
holding the region holds the slot it entered by, so the claim refuses it too.
A call that meets a slot the claim holds, nested or not, waits in the thunk
until the slot is handed back or dropped. `TASK:KILL` of an
exposed task unexposes it first, so it refuses in the same cases.

```forth
TASK:MIN-STACK TASK:TASK HOST              \ the region a C thread enters
CALLBACK: START ( n -- n ) 0 FALLBACK ;CALLBACK

: SERVE ( -- )
   HOST TASK:EXPOSE
   START HOST TASK:CONTEXT FFI-CB:ENTRY    \ pthread_create's start routine
   ... pthread_create, then pthread_join ...
   HOST TASK:UNEXPOSE ;                    \ drops START's binding
```

## Lifetime

A binding lasts from ENTRY until UNBIND, UNEXPOSE, the end of the task whose
region it names, or a capture. The caller stops C's calls and drains them
before UNBIND or UNEXPOSE: return from qsort, join the pthread. The engine
enforces every ordering it can observe:

- a held slot refuses UNBIND, UNEXPOSE and a capture;
- a held owner refuses UNEXPOSE;
- `EXPOSED` refuses ACTIVATE and ends a capture;
- a slot still held when the task whose region it names ends, which only a
  broken contract can bring about, ends the process with exit 106 and
  `task: a callback slot naming the ending task is in flight`. The slot's count
  of region stores confirms that its holder entered while the slot named the
  ending task's region.

A call through a slot that is no longer bound fails closed with 106 and never
touches a region. One that meets a drop moving its slot waits for it and then
fails closed the same way, unless a bind has taken the slot by then: the call
enters that binding. The thunk claims its slot before it reads the region
the slot names, and a region is never released while a slot names it: UNEXPOSE
and the task end, the only words that release a named region, drop its slots
first. A task drops its slots at its end, before its join is signalled, so a
joined task names none. The slots, their dispatch table and their fault cells
are one set for the image: they are process-wide in
[threads.md](threads.md#library-storage-classes)'s terms.

## The engine half

- **The band.** Every region has eight cells at `$41C8..$4208`, `CB-XDS`
  through `CB-ROWS` (`src/habu/layout.f`):
  - the frame an outbound call publishes (`CB-XDS`, `CB-DBASE`, `CB-NDICT`,
    `CB-CP`; `CB-XDS` 0 means no frame);
  - the owner (`CB-OWNER`);
  - the live marshal frame (`CB-FRAME`);
  - the dispatch and slot tables (`CB-XTS`, `CB-ROWS`), read only in the main
    region.

  Each cell is correct at zero, so a fresh task region, a fresh DATA mapping and
  a stripped image's anonymous DATA need nothing. `src/habu/aot-owned-cells.f`
  claims the band `FRESH-BYTES`.
- **Outbound calls.** The three outbound cores run `BCB-ENTER` before the
  branch and `BCB-LEAVE` after it (`src/habu/habu1.f`). ENTER takes the owner
  first: LDAR `CB-OWNER`. If it is this thread, the call is nested and claims
  nothing. If it is 0, CASAL it 0 to this thread. Any other value, or a lost
  CAS, exits 106 with the region untouched. Only then does ENTER push a `$30`
  frame (the claim flag, the core's `x20` park, the four cells it replaces) and
  publish the live cursors. LEAVE restores the cells and then releases a
  claimed owner last, by STLR. A thread that acquires the owner therefore sees
  a complete frame or `CB-XDS` 0.
- **The thunk.** `callback-entry ( n -- n )` answers stub n. The 16 stubs are
  two instructions each (the slot into `x9`, then a branch) into one thunk.
  The thunk:
  1. saves C's callee-saved set (`x19..x30`, `d8..d15`);
  2. parks `x0..x7` and `d0..d7` as the marshal frame, the floats at
     `CB-FRAME-FLOATS`;
  3. keeps everything it needs after the branch on its own frame, because
     checked code may hold state in `x21..x25` and `x29`
     (`src/compiler/native/abi.f`);
  4. claims the slot row by the owner rule above, then reads the region; a
     row a bind, unbind or drop holds `CB-ROW-BUSY` is waited out, spinning on
     LDAR with the YIELD hint, and claimed as the mover left it;
  5. claims the region's owner, then checks `CB-XDS` and the xt;
  6. loads the VM and branches to the dispatch;
  7. compares the dictionary count and code pointer with the ones it loaded,
     and exits 106 if the body moved either;
  8. restores `CB-FRAME`, then releases the owner and then the row, each by
     STLR.

  The result goes back in the x0 or d0 slot of the marshal frame.
- **Slot rows.** `CB-ROW-BYTES` 16 per slot: the region and the owner, the
  owner `CB-ROW-BUSY` 1 while a bind, unbind or drop moves the row, and a bind
  names only a row it found free and holds BUSY. A call that meets BUSY waits
  for the hand-back, so a mover holds BUSY only for steps that call nothing,
  wait on nothing and throw nothing. The rows are `lib/task.f`'s
  `ROWS`, in main DATA, which is never unmapped; beside them `MOVES` counts
  each row's region stores, which only the movers write and the thunk never
  reads. The thunk
  finds them, and the dispatch table, through the main region's `CB-ROWS` and
  `CB-XTS` at the absolute address `DATA-VA`.

## The thread pointer

An owner is the thread pointer. On Linux that is `TPIDR_EL0`, the TLS base
`clone` gives each thread. On macOS it is `TPIDRRO_EL0` with the low three bits
cleared, as libsyscall's `os/tsd.h` `_os_tsd_get_base` clears them. Measured on
macOS 27.0.1 (26A434, arm64): libsystem_pthread's `pthread_self` opens with
`mrs x0, TPIDRRO_EL0 ; ldr x8, [x0, #-0xe0]!`. Seven threads each read the
register four million times, across `sched_yield` and `usleep`. Each thread saw
one value, with its low three bits clear and always `pthread_self + $E0`, so the
values differed between threads. `lib/ffi-callback-test.f` checks this on the
host it runs on: each thread's owner sits at one offset from its own
`pthread_self`.

## Capture and stripped images

`IMAGE-LIFECYCLE:PREPARE` drops every idle binding, because a binding is
process state. Two things end the capture instead:

- a thread inside a slot: `task: callback slot in flight at capture ...`;
- an exposed task: `task: exposed task at capture ...`.

A stripped application therefore binds in its entry word.
`test/stripped-callback-subject.f` binds, sorts and unbinds there.

## Untested

Every run so far is macOS 27 on arm64. The Linux arm64 path (`TPIDR_EL0`,
`exit_group`) is written but has never run. x86-64 has neither half.
`callback-entry` is a refused row there (exit 76), and the SysV cores publish
no frame ([x86-64.md](x86-64.md#task-entry)).

## Tests

`bin/hb --load lib/ffi-callback-test.f` (gate row `ffi-callback`) runs every
case through libc's `qsort` and `pthread_create`. It writes
`build/ffi-callback-transcript.txt` and compares it whole:

- the main task: plain, nested and throwing comparators, and UNBIND refused
  inside one;
- every argument register;
- a worker on its own context, once unbinding and once ending bound;
- a foreign thread on an exposed task, with every refusal while it is parked
  inside;
- a foreign thread whose entry, and then its nested calls, meet slots the test
  holds BUSY and complete once they are handed back;
- a worker that exposes a task and ends;
- a capture;
- the declaration refusals;
- one child per exit-106 case, `body-defines` included, plus a definition
  while a task is live and a capture with a thread inside;
- children that exit 0: a worker halted inside its comparator, a yielding
  comparator killed mid-sort, and a body stopped on an exposed task that
  `TASK:WAKE` releases;
- a stripped image.

`lib/task-test.f` covers the EXPOSE, UNEXPOSE, CONTEXT and CONTEXT-BIND
refusals, an exposed task at capture, a worker activating a worker, which
counts on the main region, 20000 rounds of a bind racing an UNEXPOSE that must
leave no slot naming the unexposed region, and a hint posted while a task is
exposed that must not reach its next run.
