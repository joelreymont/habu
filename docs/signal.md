# Process signals

[`lib/signal.f`](../lib/signal.f) delivers a chosen set of process signals to a
Habu program in ordinary checked code. A program that wants SIGTERM stops
polling a stop file and waits on a descriptor instead.

No Forth word is async-signal-safe: a handler that entered the engine would run
the compiler's storage, the dictionary and the VM stacks at whatever point the
signal interrupted them. So the only code the kernel ever runs in a handler is
the machine-code stub the engine bakes beside its crash handler
(`src/habu/crash.f` `EMIT-SIGNAL-HANDLER`). The stub writes the four-byte signal
number to whatever descriptor one process-wide word holds, and ignores the
result. Everything else happens in checked Habu on the reading end.

## Interface

| Word | Inputs | Result |
| --- | --- | --- |
| `INIT` | — | — |
| `CATCH` | Signal number, `1..64` | — |
| `FD` | — | `fd`, the read end |
| `PENDING?` | — | `bool`, readable now |
| `WAIT` | `ms` window | `signal-result`: `signal n` or `timeout` |
| `RELEASE` | — | — |

`SIGNAL:signal-result` is a sum type, so every caller matches both arms:

```forth
1000 >MS SIGNAL:WAIT
MATCH SIGNAL:signal-result
   signal OF STOP-REQUESTED ENDOF
   timeout OF SERVE-ANOTHER-ROUND ENDOF
;MATCH
```

`SIGNAL:SIGHUP`, `SIGINT`, `SIGTERM`, `SIGUSR1` and `SIGUSR2` name the numbers.

## The install-and-read pattern

`INIT` opens a pipe, sets close-on-exec on both ends, asks for the write end to
be non-blocking (see the gap below) and arms the fd word with it. `CATCH` then
installs the stub for one signal. `WAIT` polls the read end and reads the
number; `FD` hands the read end to a program that would rather poll it beside
its own sockets, and `PENDING?` answers whether one is waiting without
consuming it. `RELEASE`
clears the fd word first — so a signal delivered during the teardown is
absorbed rather than written to a descriptor that is about to close — then
restores `SIG_DFL` for every signal `CATCH` installed and closes both ends.

**`INIT` runs in the main task.** The engine publishes the stub's address and
the address of the fd word in two DATA cells at boot (`src/habu/layout.f`
package `SIGNAL-ABI`). A spawned task's region is a fresh mapping that carries
neither, so it reads zero at both. `INIT` reads them where the boot wrote them
and keeps the two values; every later arm and disarm reaches the one word by
its absolute address, which is why the stub itself loads that address as a
baked literal rather than through the running task's region base. `INIT` in a
spawned task, or on an engine that bakes no stub, reads zero and is refused
with `E-SIGNAL-ABI`.

The handler runs on whichever thread the kernel hands the signal to. Because
the fd word is one word for the whole process, that does not matter: a signal
raised while the main task sleeps and a worker task runs is written to the same
pipe and answered by the same `WAIT`.

## SA_RESTART, and which waits restart

`CATCH` installs with `SA_RESTART`, so the `read`, `write` and `wait` calls a
program is blocked in resume by themselves after a caught signal instead of
failing with `EINTR`.

`poll(2)` restarts for nobody. It reports `-EINTR`, which is not a failure:
nothing was consumed and no descriptor was touched. `SIGNAL:WAIT` therefore owns
a deadline — it sets one from the `ms` it was given and restarts its own poll
against what is left of it, exactly as `lib/process.f` `POLL-IN-OR-TIMEOUT`
does through `PROC-POLL-RESTART` — so a signal storm cannot push one `WAIT` past
the window it was given, and a window that has already closed answers `timeout`.
`PENDING?` owns no deadline: its zero-wait poll is simply asked again, because
the stub has already written by the time the call reports `-EINTR`.

A program that polls `SIGNAL:FD` itself inherits the same rule and must handle
`-EINTR` on its own poll; `PROC-POLL-RESTART` is the shape to copy.

## Four bytes, and what a slow reader loses

The stub writes the number with one `write` of four bytes. Four is far under
`PIPE_BUF`, so a pipe takes the write whole or not at all: a reader never sees a
torn number, and a reader that falls behind loses whole signals rather than
framing. Two signals raised before one `WAIT` are answered in order by two
`WAIT`s. A short read is therefore an invariant violation, not a partial
delivery to be resumed, and `WAIT` names it `E-SIGNAL-READ`.

Repeated deliveries of the SAME signal while it is blocked coalesce in the
kernel, as they do for any handler: the facility reports signals, not counts.

## Known gaps

**The write end is not actually non-blocking on Linux yet.** `INIT` calls
`PROC-NONBLOCK!`, but `lib/process.f` `O-NONBLOCK` is the macOS value (`4`);
Linux spells it `$800`, so the `F_SETFL` is a no-op there and the write end
blocks when the pipe is full. Until dot `habu-give-o-nonblock-cff35c7a` lands,
a program whose reader stalls long enough to fill the pipe can block the thread
the signal landed on. Four bytes a signal against a 64 KiB pipe makes that a
distant hazard, not a near one, but it is real and it is not this library's to
fix: the value belongs to `lib/process.f`.

**The signal numbers are Linux's.** `SIGHUP` 1, `SIGINT` 2 and `SIGTERM` 15 are
the same on macOS, but `SIGUSR1` and `SIGUSR2` are 30 and 31 there, not 10 and
12, and `SA_RESTART` is 2 rather than `$10000000`. Nothing in `lib/` carries a
target switch for constants like these — `lib/process.f` simply spells the macOS
values for `O_NONBLOCK` and `F_SETNOSIGPIPE` — so this file spells the Linux
ones and records the pair here instead of inventing a switch. A macOS port
needs those three constants and the `struct sigaction` layout revisited.

**The image-restore hook is registered but unexercised.** A restored image is a
different process, so `INIT` registers a reset with `IMAGE-LIFECYCLE` that drops
the facility's cells; no test writes and restores an image with signals armed.

## Refusals

| Code | Meaning |
| --- | --- |
| `E-SIGNAL-STATE` | A word that needs the facility before `INIT`, or a second `INIT` without `RELEASE` |
| `E-SIGNAL-ABI` | The published stub cells read zero: no baked stub, or not the main task |
| `E-SIGNAL-NUMBER` | A signal number outside `1..64` |
| `E-SIGNAL-INSTALL` | `sigaction` refused the install or the restore |
| `E-SIGNAL-READ` | The descriptor answered short of the four-byte number |
| `E-SIGNAL-POLL` | `poll` refused the read end with an errno |
| `E-SIGNAL-CLOSE` | Closing an end of the self-pipe failed |

`lib/signal-test.f` covers each of them, the delivery cases above, and the
absolute-address property observed from a spawned task.
`test/signal-stub.f` covers the engine half: that the two cells are published,
where they sit, and that an armed word carries a raised number out.
