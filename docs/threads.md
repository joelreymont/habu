# Habu Tasking

`lib/task.f` implements native CPU tasks over `pthread` on macOS/aarch64 and
Linux/aarch64 in package `TASK`. Load it with:

```forth
require lib/task.f
```

The module owns its dependencies (`lib/errors.f`, `lib/memory.f`, `lib/ffi.f`);
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
status and message; an uncaught worker `throw` terminates the process with the
low byte of the throw code and `task: unhandled throw` on stderr. Task failure
is process-fatal by design until a checked result/future model exists.

## Public Words

```forth
TASK:TASK            ( n -- )          \ define a task TCB; n is stack bytes
TASK:MIN-STACK       ( -- n )
TASK:CONSTRUCT       ( ptr a -- )      \ allocate task stack/region without starting
TASK:ACTIVATE        ( n ptr a -- )    \ run xt in a pthread-backed task
TASK:SELF            ( -- ptr a )
TASK:SELF-N          ( -- n )
TASK:PAUSE           ( -- )            \ yield; worker exits if HALT requested
TASK:HALT            ( ptr a -- )      \ request stop at next PAUSE
TASK:KILL            ( ptr a -- )      \ join/release task memory
TASK:DONE?           ( ptr a -- bool )

TASK:#USER           ( -- n )
TASK:+USER           ( n n -- n )      \ define task-local user variable
TASK:HIS             ( ptr a ptr a -- ptr a )

TASK:FACILITY        ( -- )            \ define pthread mutex storage
TASK:FACILITY-INIT   ( ptr a -- )
TASK:GET             ( ptr a -- )
TASK:RELEASE         ( ptr a -- )
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
live-task compile guard, and process-fatal worker `die`/`throw` diagnostics.
The full test suite includes these as `tasking-primitive-smoke` and
`tasking-threads`.
