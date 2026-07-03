# Process And PTY Runtime Notes

Habu's native process harness uses raw target syscalls only. It does not call
libc helpers.

## Focused Gate

Use the native gate slice through `bin/hb --load`; do not concatenate files with
host shell logic:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/process.f \
  lib/process-argv.f lib/process-env.f lib/test.f test/proc-pty.f
```

The full port gate loads this slice through `test/run.f`.

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
