# Habu Tasking

`lib/task.f` implements native CPU tasks over `pthread` on macOS/aarch64 and
Linux/aarch64. Load it with:

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

## Public Words

```forth
TASK            ( n -- )          \ define a task TCB; n is stack bytes
CONSTRUCT       ( ptr a -- )      \ allocate task stack/region without starting
ACTIVATE        ( n ptr a -- )    \ run xt in a pthread-backed task
PAUSE           ( -- )            \ yield; worker exits if HALT requested
HALT            ( ptr a -- )      \ request stop at next PAUSE
TASK-KILL       ( ptr a -- )      \ join/release task memory
TASK-DONE?      ( ptr a -- bool )

#USER           ( -- n )
+USER           ( n n -- n )      \ define task-local user variable
HIS             ( ptr a ptr a -- ptr a )

FACILITY        ( -- )            \ define pthread mutex storage
FACILITY-INIT   ( ptr a -- )
GET             ( ptr a -- )
RELEASE         ( ptr a -- )
```

Use `TASK-KILL` for teardown. A task that loops must call `PAUSE` or block in a
host call; `HALT` is cooperative and is observed by `PAUSE`.

## Atomics

Shared cells used across tasks must be 8-byte aligned. `atomic@`, `atomic!`,
`atomic-add`, `atomic-cas`, and `fence` are native AArch64 primitives and are
checked in `src/core/checker.f`. Unaligned atomic cells can fault on LSE
hardware; align dictionary cells before sharing them.

## Invariants

- Tasks execute XTs only; they do not interpret source and do not compile.
- New definitions, `create`, `variable`, `constant`, `defer`, `cp!`, `ndict!`,
  and other dictionary/code mutation paths are invalid while tasks are live.
- Ordinary `variable` storage is shared process storage. Use `+USER` for
  task-local state and `HIS` to inspect another task's user cell before
  releasing that task.
- The task trampoline preserves the shared dictionary/code registers and swaps
  the data stack and data/user base for the worker.
- `FACILITY` is a pthread mutex, not a spin lock.

## Tests

Run:

```sh
bin/hb --load lib/task-test.f
bin/hb --load test/atomics-smoke.f
bin/hb --load test/run-in-stack-smoke.f
```

`lib/task-test.f` covers two pthread workers, mutex-protected shared updates,
task-local `+USER` isolation via `HIS`, `TASK-SELF`, `HALT`/`TASK-KILL`, and the
live-task compile guard. The full test suite includes these as
`tasking-primitive-smoke` and `tasking-threads`.
