# Porting Habu

The supported native targets are macOS/arm64 and Linux/aarch64. A new port adds
one target seam and proves it with native refresh plus the port gate; it does not
add host-language build logic or benchmark-runtime requirements.

## Target Source Seam

Each target owns these files under `src/os/<target>/`:

- `target.f` — target predicates used by source selection:
  `HB-TARGET-<NAME>?` for each supported target and `HB-TARGET-KNOWN?` for the
  closed target set.
- `layout.f` — executable/data virtual addresses and image layout constants.
- `sys.f` — raw syscall numbers and `SYS, ( n -- )`.
- `env.f` — argv/envp access over startup-captured DATA cells.
- `repl-term.f` — terminal ioctl, raw-mode, and termios offsets for baked REPLs.
- image builder and signer — `elf.f`/`sign.f` on Linux, `macho.f`/`sign2.f` on
  macOS.

The common engine layout lives in `src/habu/layout.f`. Standard source prefixes
load it before target env code; do not duplicate fixed DATA offsets in env,
REPL, stepper, debugger, or tool support files.

## Source Selection

The target must be wired in exactly these source-list owners:

- `tools/bootstrap.sh` for no-binary recovery.
- `tools/build-fixpoint.f` for native refresh, AOT, and REPL builds.
- `src/habu/habu2.f` and `bootstrap/cg/forth.fs` for the runtime `--load`
  prefix.
- `src/habu/stdin.f` and `tools/hb-build-lib.f` for baked REPL runtime sources.
- `tools/srclist.f`, `tools/filemap-lint.f`, and `tools/lint/shadow-lint.f` for
  discoverability and lint coverage.

No caller should pass a target prelude manually for normal tests. `bin/hb
--load` selects the host prefix from the running binary.

Target selection must fail closed. Do not write boolean branches that mean
"Linux, otherwise macOS"; every selector should handle each supported target
explicitly and call a named target-unknown error when none match.

## Syscalls And Signals

All kernel entry goes through `sys.f`. The engine assumes arguments in x0..x5;
the syscall-number register and trap instruction are target-owned:

- macOS/arm64 uses Darwin numbers and `svc #0x80`.
- Linux/aarch64 uses Linux numbers and `svc #0`.

Signal handlers are target ABI boundaries. Crash and profiler handlers must use
the target's `sigaction` frame, ucontext pointer, PC offset, `sigreturn`
convention, and installed signal list. On Linux/aarch64, `rt_sigaction` also
requires the sigset-size argument.

## Executable Images

Drivers call `BUILD-IMAGE`; the target image file implements the actual format.

- macOS uses Mach-O plus signing.
- Linux uses ELF64 with executable `PT_LOAD` detection requiring read+execute
  and not write.

The deterministic re-link contract is unchanged across targets: headers are
rebuilt from constants, code is copied from `[rbase, CODELEN)`, and native
refresh reaches a byte-for-byte fixpoint.

## Runtime And Snapshot Layout

The engine ABI is currently ARM64:

    x19 XDS   data-stack pointer (grows up)     x9-x15  VS register pool
    x16       literal/call scratch, syscall #   x17     branch-flag scratch
    x20 DATA  data region base                  x21/x22 INP/INE source
    x23/x24   TKA/TKL current token             x25     PEND (open def slot)
    x26 DBASE dict+code region                  x27/x28 NDICT/CP
    sp        machine stack: word frames, locals frames

Snapshots rely on fixed mappings and the 40-byte trailer convention: magic, old
text base, ndict, region length, and data length. A non-fixed mapping port must
implement full relocation before it can pass refresh.

## Port Gate

After `bin/hb` exists on the target, run only native validation on the target
machine:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f \
  lib/process-argv.f lib/process-env.f lib/codesign.f tools/build-fixpoint.f \
  tools/build-fixpoint-main.f -- install

bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f \
  lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f
```

Language-runtime and model benchmark gates are benchmark-host work, not port
validation.

Do not run Linux port tests from a macOS host by injecting a Linux source
prelude. Bring up `bin/hb` on the Linux machine, let that binary select its own
host target sources, and run the native gate there.
