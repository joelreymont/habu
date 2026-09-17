# Process And PTY Runtime Notes

Habu's native process harness uses raw target syscalls only. It does not call
libc helpers.

## Focused Gate

Use the direct test through `bin/hb --load`; do not concatenate files with
host shell logic:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/process.f \
  lib/process-argv.f lib/process-env.f lib/test.f test/proc-pty.f
```

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

macOS uses `/dev/ptmx` with `TIOCPTYGRANT`, `TIOCPTYUNLK`, and `TIOCPTYGNAME`.
Linux uses `/dev/ptmx`, `TIOCSPTLCK`, `TIOCGPTN`, and `/dev/pts/<n>`. Linux
gate hosts must provide `/dev/ptmx` and a mounted `/dev/pts`.

The PTY gate is the native compatibility baseline for process capture, PTY
startup, line editing, history, breakpoints, stepper recovery, Ctrl-C, Ctrl-D,
and async exit.

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

The slave is opened `O_NOCTTY`, so a session-leading supervisor without a
terminal never adopts it, and nobody takes it as a controlling terminal (that
ioctl is an unencoded request the engine's ioctl guard refuses); job control is
outside this layer. `test/process-pty-tty-smoke.f` proves both modes against
`bin/hb`.
