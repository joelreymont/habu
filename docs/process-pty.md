# Process And PTY Runtime Notes

Habu's native process harness uses raw target syscalls only. It does not call
libc helpers.

## Focused Gate

Use the direct test through `bin/hb --load`; do not concatenate files with
host shell logic:

```sh
bin/hb --load lib/errors.f lib/process.f test/proc-pty.f
```

The suite requires `lib/pty-harness.f` (below) itself; the two files named on
the line are the ones the engine bakes neither of.

The canonical native registry loads this test through `test/run.f`.

## Process Capture

`lib/process.f` exposes checked helpers over the raw runtime primitives.
Checked code should use `PROC-SPAWN-IO`, `PROC-WAIT-RC`, `PROC-RUN-RC`, and capture helpers;
they accept counted paths and throw named process errors for primitive failures.

`spawn-io ( pathz stdinfd stdoutfd stderrfd -- pid|-errno )` is target-specific
and creates each spawned child as its own process-group leader before the child
execs. The test-pool timeout/reaper path depends on this invariant; parent-side
`setpgid(child, child)` is racy and must not be used as the proof.

- macOS wraps syscall 244, `posix_spawn(pid*, path, adesc, argv, envp)`. The
  descriptor folds file actions and attributes; Habu emits dup2/chdir actions
  and a `POSIX_SPAWN_SETPGROUP` attribute with pgroup 0.
  Failures preserve the kernel errno as a negative pid code.
- Linux uses a close-on-exec exec-failure pipe around `clone`/`execve`. The
  child calls `setpgid(0,0)` before `chdir`, `dup2`, and `execve`; the parent
  returns a pid only after the child has either successfully exec'd (pipe EOF)
  or reported setup failure.

`PROC-CMD:CWD! ( ptr u8 len -- )` runs every later `PROC-CMD` child from that
directory instead of the loader's until `PROC-CMD:RESET` clears it; a missing
path or a non-directory is refused before any spawn (`E-PROC-PATH`). A gate uses
it to start a relocated executable from a private directory and prove that it
resolves nothing relative to the working directory.

Pass a negative fd to leave that stream unchanged. Parent-only pipe and PTY fds
must be marked close-on-exec before spawning, or children can inherit writers and
prevent EOF.

## PTY Foundation

The PTY flow is target-specific but the parent contract is shared:

1. open the PTY master;
2. unlock/grant or derive the slave path with target ioctls;
3. open the slave;
4. spawn with the slave duplicated to fd 0, 1, and 2;
5. parent drives the close-on-exec master with `POLL-IN` and `read`/`write`.

Steps 1 and 2 are `PTY:OPEN`'s (`lib/pty.f`), the one opener in the tree: macOS
uses `/dev/ptmx` with `TIOCPTYGRANT`, `TIOCPTYUNLK`, and `TIOCPTYGNAME`, Linux
uses `/dev/ptmx`, `TIOCSPTLCK`, `TIOCGPTN`, and `/dev/pts/<n>`, and any other
host is `E-PROC-HOST`. Step 3 stays with the caller, which opens the slave
`O_RDWR | O_NOCTTY` at the moment it wants one. Linux gate hosts must provide
`/dev/ptmx` and a mounted `/dev/pts`.

The PTY gate is the native compatibility baseline for process capture, PTY
startup, line editing, history, breakpoints, stepper recovery, Ctrl-C, Ctrl-D,
and async exit.

## Test harness (`lib/pty-harness.f`)

One module owns that flow for the suites that drive a child engine at a
terminal: `test/proc-pty.f`, `test/aot-data-span-forge.f` and the reading half
of `test/process-pty-tty-smoke.f`. They keep no copy of it. `PTY-HARNESS`
publishes:

- `SPAWN-ON-PTY ( ptr u8 n -- )` opens the pair, starts the executable with the
  slave on fds 0, 1 and 2, closes the slave and keeps the close-on-exec master;
  `CHILD-PID`, `MASTER-FD` and `CLOSE-MASTER` are the rest of that child's
  handle. A failed spawn throws what failed and leaves nothing open.
- `REAP ( -- outcome )` reaps that child within `WAIT-BUDGET-MS`. It reads the
  master to the hang-up first, because a child on a terminal nobody empties
  blocks in write() until someone does, and then waits out what is left of the
  budget on the child's lifetime watch. On expiry it kills the child and names
  it -- the pid, the budget it overran and the bytes still unread at the master
  -- and answers `OUTCOME:TIMEOUT`, so a case asserting a clean exit reds
  instead of hanging. `REAP-WITHIN ( n -- outcome )` is the same with the
  caller's own budget, and `WAIT-EXIT ( pid n -- outcome )` the same bound for a
  child that is no longer writing to us, such as a suite's pipe children.
  `PROC-WAIT-RC` keeps its unbounded contract for callers that want one.
- `SEND` / `SEND-LINE` / `SEND-BYTE` type at the master; `WRITE-ALL`,
  `WRITE-BYTE` and `WRITE-LINE` write to any descriptor, so a suite's pipe
  children use the same buffer.
- `READ-STEP ( fd -- n )` is one poll and, when the descriptor has bytes, one
  read: above zero for bytes appended, `0` for a quiet poll, `-1` for the
  far side's hang-up. A poll that reports no readiness is quiet, an errno
  included -- the descriptors are blocking, so a read no poll authorised could
  block past the caller's deadline. `READ-TO-EOF` and `DRAIN ( fd n -- )` are
  the two bulk shapes.
- `WAIT-FOR ( ptr u8 n -- bool )` reads the master until the text appears, the
  child hangs up, or the wait budget passes, on an absolute deadline
  (`PROC-DEADLINE-AT` / `PROC-LEFT-MS`, reached through `WAIT-OPEN` /
  `WAIT-LEFT`). No count of polls or reads bounds it: reads that bring data and
  a dead child's ready-and-empty polls both spend a count without spending time
  -- measured, the 500-poll budget one of these waits used to carry went by in
  19 ms.
- `BUF$`, `BUF-CLEAR`, `FIND-FROM`, `IN-BUF?`, `FIND-AFTER` and `AFTER?` are the
  buffer and one span search over it. A full buffer keeps its tail, so old bytes
  do leave it.
- A negative claim therefore never scans the buffer alone. `WAIT-BARRIER` closes
  a window at a marker the child printed PAST the point where the rejected text
  would have appeared and `WINDOW-ABSENT?` reads only that window (with no
  barrier there is no claim, and the answer is false); `WATCH+ ( ptr u8 n --
  watch )` records a never-seen fact as the bytes arrive, which a compaction
  cannot erase, and `NEVER-SEEN?` reads it back.
- `ROOM$` / `TOOK` are the append itself, for a reader the module does not own:
  `test/process-pty-tty-smoke.f` reads through the supervised linear handle
  below and hands the bytes back through them.

The state is process-wide: one child and one wait at a time, and `SPAWN-ON-PTY`
refuses a second pair while one is open. `lib/pty-harness-test.f` drives the
buffer, the search, the watches and the spawn's abort path directly, and forces
the wedge the bounded reap exists for: a child that writes into the terminal
forever and never exits is killed on the clock, not waited on.

## Supervised sessions (`lib/process-pty-io.f`)

`PROCESS-PTY:SPAWN` runs a target over pipes; `PROCESS-PTY:SPAWN-TTY` runs it
with the slave side of a fresh pseudo-terminal on fds 0, 1 and 2, so a child
engine sees a terminal, installs its line editor and recovers from a refused
definition instead of stopping at it (the batch contract of the pipe mode).
Both modes share the handle lifecycle (`LAUNCH`, `ALIVE?`, `AWAIT`, `SIGNAL`,
`TEARDOWN`) and the two I/O words:

- `WRITE-LINE ( handle ptr u8 n -- handle )` sends the bytes and a line feed to
  the target's input; a short write throws `E-PROC-OUTPUT`.
- `AWAIT-BYTES ( handle ptr u8 n ms -- handle n )` waits up to `ms` for output
  and reads what is there: the byte count, `0` when nothing arrived in time,
  `-1` once the target's side is gone; a broken descriptor throws. It answers
  with ONE read, never with the target's whole answer -- a child's line editor
  redraws on every keystroke and a loaded host splits one line's echo across
  dozens of reads -- so a caller waiting for text loops to its marker under an
  absolute deadline (`PROC-DEADLINE-AT` / `PROC-LEFT-MS`) and ends only on the
  marker, the `-1`, or the clock.

`AWAIT` and `AWAIT-BYTES` wait on the AIO loop ([aio.md](aio.md)): one
`AIO:POLL` for the milliseconds the caller named and one `AIO:AWAIT`, with
`-1` the unbounded wait `POLL` spells the same way. A program calls
`AIO:START` before the first of them, and a wait with no loop running is
`E-AIO-STATE`.

The slave is opened `O_NOCTTY`, so a session-leading supervisor without a
terminal never adopts it, and nobody takes it as a controlling terminal (that
ioctl is an unencoded request the engine's ioctl guard refuses); job control is
outside this layer. `test/process-pty-tty-smoke.f` proves both modes against
`bin/hb`.
