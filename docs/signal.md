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

| Word | Inputs | Result | Task |
| --- | --- | --- | --- |
| `INIT` | — | — | the main task |
| `CATCH` | Signal number, `1..64` | — | the task that ran `INIT` |
| `FD` | — | `fd`, the read end | any |
| `PENDING?` | — | `bool`, readable now | any |
| `WAIT` | `ms` window | `signal-result`: `signal n` or `timeout` | any |
| `RELEASE` | — | — | the task that ran `INIT` |

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

`INIT` opens a pipe, sets close-on-exec and `O_NONBLOCK` on both ends and arms
the fd word with the write end. `CATCH` then installs the stub for one signal.
`WAIT` waits on the read end and reads the number; `FD` hands the read end to a
program that would rather poll it beside its own sockets, and `PENDING?`
answers whether one is waiting without consuming it. Both wait through the AIO
loop ([aio.md](aio.md)) - one `AIO:POLL` for the window, `PENDING?`'s being
zero-length, and one `AIO:AWAIT` - so a program calls `AIO:START` before
its first `WAIT` or `PENDING?`, and either with no loop running is
`E-AIO-STATE`. `RELEASE`
clears the fd word first — so a signal delivered during the teardown is
absorbed rather than written to a descriptor that is about to close — then
restores `SIG_DFL` for every signal `CATCH` installed and closes both ends.

Both ends are non-blocking for the same reason from two directions. The write
end, so a pipe a stalled reader has filled refuses the stub's four bytes rather
than parking whichever thread the signal landed on. The read end, so a `WAIT`
that polls `POLLIN` and then finds the bytes gone is refused rather than parked
in `read` until the next signal — see "Two tasks, one delivery" below.

**`INIT` runs in the main task, and owns the facility.** The engine publishes
the stub's address and the address of the fd word in two DATA cells at boot
(`src/habu/layout.f`
package `SIGNAL-ABI`). A spawned task's region is a fresh mapping that carries
neither, so it reads zero at both. `INIT` reads them where the boot wrote them
and keeps the two values; every later arm and disarm reaches the one word by
its absolute address, which is why the stub itself loads that address as a
baked literal rather than through the running task's region base. `INIT` in a
spawned task, or on an engine that bakes no stub, reads zero and is refused
with `E-SIGNAL-ABI`.

`INIT` also records `TASK:SELF-N`, and `CATCH` and `RELEASE` refuse any other
task with `E-SIGNAL-STATE`. They share one `struct sigaction` pair — the record
handed to `sigaction` and the `oldact` it fills — and one caught set, so a
second task in either would be overwriting a record the owner is in the middle
of using. `FD`, `PENDING?` and `WAIT` touch none of the three and answer any
task.

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
A read another task won re-polls against that same deadline, so the two ways one
`WAIT` can poll twice share one clock. Only the FIRST poll of a window is given
the `ms` as the caller spelled it: `PROC-LEFT-MS` floors, and re-deriving the
window from the deadline before anything had consumed it would shave a
millisecond off it.
`PENDING?` owns no deadline: its zero-wait poll is simply asked again, because
the stub has already written by the time the call reports `-EINTR`. It refuses a
descriptor `poll` counts without `POLLIN` — see "Two tasks, one delivery".

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

## Two tasks, one delivery

Nothing stops two tasks from calling `WAIT` on the same pipe, and one delivery
wakes both of their polls with `POLLIN`. Only one of them finds four bytes left
to read. The other's `read` is refused, and `WAIT` treats that as the lost race
it is: it re-polls against the deadline it already set and answers `timeout`
when that window runs out. A caller that loses gets its own window back, never
another task's signal and never a wait past the milliseconds it asked for.

The refusal is what makes the loser's window close at all. The engine collapses
every failed syscall but `poll` to a bare `-1` (`src/habu/habu1.f`, "THE ERRNO
RULE FOR THIS FILE'S SYSCALL WRAPPERS"), so there is no errno to read here — but
on a non-blocking descriptor the facility owns, reading four bytes into its own
span with a write end held open for the facility's life, `EAGAIN` is the only
refusal the kernel has left. With a BLOCKING read end the loser would park in
`read` until the next signal arrived, turning a race into a `WAIT` that never
returns; `lib/signal-test.f` runs two tasks against one raised signal and
asserts that both finish, that exactly one carries the number and that the other
times out inside its window.

A refusal only means the lost race when `POLLIN` put the bytes there to begin
with, so `WAIT` reads the slot's `revents` and not just `poll`'s count. `poll`
counts `POLLNVAL` — a read end closed behind the facility's back — as ready, and
re-reading that against the deadline would never end; `WAIT` names it
`E-SIGNAL-POLL` instead. `PENDING?` makes the same test on the same slot, so
"ready" never reaches a caller as "readable": a descriptor `poll` counts without
`POLLIN` is `E-SIGNAL-POLL` there too, rather than a `true` that sends the
caller into the `WAIT` that would refuse it.

## Hosts

Linux and macOS, on aarch64, and the values differ on all three counts that
matter. `SIGHUP` 1, `SIGINT` 2 and `SIGTERM` 15 are the same, but `SIGUSR1` and
`SIGUSR2` are 10 and 12 on Linux and 30 and 31 on macOS; `SA_RESTART` is
`$10000000` and 2; `struct sigaction` is `$98` bytes with `sa_flags` at `$88` on
glibc/aarch64 and `$10` bytes with `sa_flags` at `$0C` on macOS. `lib/signal.f`
selects each behind `HB-TARGET-LINUX?` / `HB-TARGET-MACOS?`, the way
`lib/process.f` `O-NONBLOCK`, `lib/fs.f` and `lib/process-pty-io.f` select
theirs, and `INIT` refuses any third target with `E-PROC-HOST` before it opens
anything. A file that spelled the Linux numbers everywhere would install the
stub for `SIGBUS` with no flags on macOS and say nothing about it.

**The macOS arm is selected for but untested.** No macOS host runs this suite,
so its numbers, its flag and its record layout are read off the platform headers
and never observed. `lib/signal-test.f` selects its expectations by target too,
spelling the platform facts a second time so a wrong arm in the library still
fails against an independent copy — and so the `process-signals` gate is not red
by construction on the target it never runs on. `SIG-MAX` is 64, the range
Linux's `sigaction` installs: on macOS a number between 32 and 64 passes that
check and is refused by `sigaction` itself as `E-SIGNAL-INSTALL`.

## Image restore

`INIT` registers a one-shot reset with `IMAGE-LIFECYCLE`. Capture clears the
facility's cells and registration flag, so the restored process starts cold
and its next `INIT` registers a fresh reset. `test/process-image.f` captures
with signals armed, restores outside the checkout, and repeats capture twice.
Each restore refuses descriptor access before `INIT`, then proves the new pipe
delivers a signal.

## Refusals

| Code | Meaning |
| --- | --- |
| `E-SIGNAL-STATE` | A word that needs the facility before `INIT`, a second `INIT` without `RELEASE`, or `CATCH`/`RELEASE` from a task that did not run `INIT` |
| `E-SIGNAL-ABI` | The published stub cells read zero: no baked stub, or not the main task |
| `E-SIGNAL-NUMBER` | A signal number outside `1..64` |
| `E-SIGNAL-INSTALL` | `sigaction` refused the install or the restore |
| `E-SIGNAL-READ` | The descriptor answered short of the four-byte number |
| `E-SIGNAL-POLL` | `poll` refused the read end with an errno, or reported it ready without `POLLIN` |
| `E-SIGNAL-CLOSE` | Closing an end of the self-pipe failed |
| `E-PROC-HOST` | `INIT` on a target that is neither Linux nor macOS |

`lib/signal-test.f` covers each of them except `E-PROC-HOST`, which no host here
can reach, along with the delivery cases above, the two-task race, the owner
rule and the absolute-address property observed from a spawned task.
`test/signal-stub.f` covers the engine half: that the two cells are published,
where they sit, and that an armed word carries a raised number out.
