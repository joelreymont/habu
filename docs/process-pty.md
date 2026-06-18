# Process And PTY Runtime Notes

Habu's native Darwin process harness uses raw syscalls only. It does not call
libc helpers.

## Process Capture

`lib/process.f` exposes checked helpers over the raw runtime primitives. The
focused process/PTY gate bundles it with:

```sh
cat lib/errors.f lib/process.f test/proc-pty.f | bin/hb
```

The raw `spawn-io ( pathz stdinfd stdoutfd stderrfd -- pid|-1 )` primitive wraps syscall 244,
`posix_spawn(pid*, path, adesc, argv, envp)`. Darwin's kernel ABI differs from
libc's public `posix_spawn` signature: the third argument is a private
`struct _posix_spawn_args_desc`.

The descriptor fields used by Habu are:

- `file_actions_size` at offset 16
- `file_actions` at offset 24
- total descriptor size 128 bytes on arm64

The file-actions blob is the XNU `_posix_spawn_file_actions` layout:

- header: `psfa_act_alloc` at offset 0, `psfa_act_count` at offset 4
- action array starts at offset 8
- `_psfa_action_t` stride is 1040 bytes
- `PSFA_DUP2 = 2`
- each dup2 action writes `type` at `+0`, source fd at `+4`, target fd at `+8`

`spawn-io` emits only dup2 actions. Pass a negative fd to leave that stream
unchanged. It returns the spawned pid or `-1`; `wait-rc ( pid -- rc )` returns
`WEXITSTATUS(status)` or `-1`. Checked code should use `SPAWN-IO`,
`WAIT-RC`, and `RUN-RC`; those wrappers accept counted paths and throw named
process errors for primitive failures.

When all three fds are negative, `spawn-io` passes a null descriptor. XNU rejects
an args descriptor whose file-actions pointer names a zero-action blob with
`EINVAL`.

The parent must mark parent-only fds close-on-exec with `FD-CLOEXEC!` before
spawning. For stdin capture, this includes the pipe write end; otherwise the
child inherits that writer and never sees EOF after the parent closes its copy.

## PTY Foundation

Darwin's `openpty`, `forkpty`, `posix_openpt`, `grantpt`, `unlockpt`, and
`ptsname` are libc symbols, not direct syscalls. The raw path is:

1. `open("/dev/ptmx", O_RDWR, 0)` for the master.
2. `ioctl(master, TIOCPTYGRANT, 0)`.
3. `ioctl(master, TIOCPTYUNLK, 0)`.
4. `ioctl(master, TIOCPTYGNAME, pathbuf)` for the slave path.
5. `open(pathbuf, O_RDWR, 0)` for the slave.
6. `spawn-io` with the slave fd duplicated to `0`, `1`, and `2`.
7. Parent drives the close-on-exec master with `POLL-IN` and `read`/`write`.

Darwin constants used by `test/proc-pty.f`:

- `TIOCPTYGRANT = $20007454`
- `TIOCPTYGNAME = $40807453`
- `TIOCPTYUNLK = $20007452`
- `POLLIN = 1`
- `F_SETFD = 2`
- `FD_CLOEXEC = 1`

The focused gate is the bundled command shown above. It is the native
compatibility baseline for process capture, PTY startup, line editing, history,
breakpoints, stepper recovery, Ctrl-C, Ctrl-D, and async exit.
