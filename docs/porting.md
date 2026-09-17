# Porting Habu

The supported native targets are macOS/arm64 and Linux/aarch64, with
Linux/x86-64 declared: its seam directory, ELF64 writer, target contract and
syscall emitters exist and are exercised from an aarch64 host, and no x86_64
instruction has executed yet — the engine body and the compiler backend are
still to come. A new port adds one target seam and proves it with native refresh
plus the port gate; it does not add host-language build logic or
benchmark-runtime requirements.

## Target Source Seam

Each target owns these files under `src/os/<target>/`:

- `target.f` — target predicates used by source selection:
  `HB-TARGET-<NAME>?` for each supported target and `HB-TARGET-KNOWN?` for the
  closed target set.
- `layout.f` — executable/data virtual addresses and image layout constants.
- `sys.f` — raw syscall numbers and `SYS, ( n -- )`.
- `repl-term.f` — terminal ioctl, raw-mode, and termios offsets for baked REPLs.
- image builder and signer — `elf.f`/`sign.f` on Linux, `macho.f`/`sign2.f` on
  macOS.
- process primitives — `proc-watch.f` and `proc-control.f`, the syscall
  emitters the supervisor uses after fork.

`src/os/linux-x86-64/` is the third seam and carries the whole file set under the
word names the other two use, so an image builder selects a seam instead of
branching. Its emitters are written against package `X64ASM`
(`src/arch/x86-64/asm.f`) where the other two are written against `A64ASM`'s
mnemonics. Two names they use are not the seam's own: `G-POP` and `G-PUSH` from
the engine's runtime layer, and `ASM-SINK ( -- ptr u8 )`, the byte buffer the
code stream being emitted appends into. An x86_64 instruction has no value
representation, so every `X64ASM` encoder takes that sink as its last operand
rather than returning a word, and supplying it is the x86-64 code layer's
obligation; `test/x86-64-emit.f` binds it to a test-owned buffer and pins every
byte the seam emits.

A syscall stencil — the write and exit steps the MATCH bad-tag die emits into a
user word — is a BYTE STRING `( -- ptr u8 n )` in every seam, not an instruction
word, because the x86_64 spelling is five bytes of `mov eax, imm32` and two of
`syscall` where the ARM64 spelling is one four-byte word. `C-EMIT-STENCIL`
(`src/habu/jit.f`) compiles the runtime emission from that span.

The common engine layout lives in `src/habu/layout.f`. Startup argv/envp access is
shared in `src/os/env-base.f`; `src/os/script-argv.f` owns the `bin/hb --load`
argument convention, and `src/habu/bundle-argv.f` owns standalone bundle
arguments. Do not add per-target env files or duplicate fixed DATA offsets in
REPL, stepper, debugger, or tool support files.

## Source Selection

The target must be wired in exactly these source-list owners:

- `tools/bootstrap.sh` for no-binary recovery.
- `tools/build-fixpoint.f` for native refresh, AOT, and REPL builds.
- `tools/native-emit.f` for the source-bound emitter's own seam load.
- `src/habu/habu2.f` and `bootstrap/cg/forth.fs` for the runtime `--load`
  prefix.
- `src/habu/stdin.f` and `tools/hb-build-lib.f` for baked REPL runtime sources.
- `tools/lint/shadow-lint.f` for lint coverage.

No caller should pass a target prelude manually for normal tests. `bin/hb
--load` selects the host prefix from the running binary.

Target selection must fail closed. Do not write boolean branches that mean
"Linux, otherwise macOS"; every selector should handle each supported target
explicitly and call a named target-unknown error when none match.

## Engine Primitives

`src/habu/prims.f` is the single specification of the engine's primitives: one
row per primitive carrying its name, its checker effect and the name of the
reference implementation that answers for it, and nothing machine-specific.
`src/core/checker.f` replays that table to build its own rows, so an effect is
stated once; `src/habu/habu1.f` refuses a machine body whose name has no row and
`src/habu/habu2.f` refuses a row that the backend never answered. A new backend
adds bodies under the row names and declares no effects of its own. Rows of kind
`ELAB` (the checker computes the effect at the call site) and `UNROWED` (no
declaration anywhere) name the primitives that deliberately have no row.

## Native Compiler ABI

`src/compiler/native/abi.f` maps the same target predicates to
`aapcs64-linux` or `aapcs64-darwin`; `src/compiler/a64-effect.f` and
`src/arch/arm64/asm.f` make x18 available only on Linux. The ABI field records
the host platform identity. Habu-to-Habu calls still use Habu's internal
contract: arguments and results occupy caller data-stack slots, the usable
register pool is scratch, and frames are 16-byte aligned. No current compiler
consumer uses platform variadic or stack-argument rules.

## Syscalls And Signals

All kernel entry goes through `sys.f`. The engine assumes arguments in x0..x5;
the syscall-number register and trap instruction are target-owned:

- macOS/arm64 uses Darwin numbers and `svc #0x80`.
- Linux/aarch64 uses Linux numbers and `svc #0`.
- Linux/x86-64 uses the x86_64 numbers and `mov eax, imm32` then `syscall`.

A target's numbers are chosen against the argument shapes the engine's
primitives build, not against the kernel's most convenient spelling. The
aarch64 engine loads `AT_FDCWD` and a flags register for access, unlink,
rename, chmod, stat, lstat, open and mkdir, so both Linux seams name the *at*
family - `faccessat`, `unlinkat`, `renameat`, `fchmodat`, `newfstatat`,
`openat`, `mkdirat` - and `unlinkat` serves rmdir while `newfstatat` serves
lstat, through their flag arguments.

Two things about Linux/x86-64 are not settled by the numbers. Its syscall ABI
passes arguments in rdi, rsi, rdx, r10, r8 and r9 rather than in the engine's
x0..x5 order; the seam maps x0..x5 onto them in that order, so `OS-OPEN-RD` and
the process primitives name x86_64 register numbers. And the error convention
differs in POLARITY: the aarch64 seam reconciles Linux's `-errno` return into the
carry flag with `cmp x0, #-4095`, where ARM sets C on NO borrow, while x86 sets
CF on borrow, so `cmp rax, -4095` would set CF exactly when the call SUCCEEDED.
The x86_64 seam therefore reverses the operands — `mov rcx, -4096` then
`cmp rcx, rax`, with rcx free because `syscall` clobbers it — which sets CF on
the same -errno range the aarch64 sequence flags, so `SYS-PUSH`'s `C-CS`
consumers read the bit the same way on both.

Signal handlers are target ABI boundaries. Crash and profiler handlers must use
the target's `sigaction` frame, ucontext pointer, PC offset, `sigreturn`
convention, and installed signal list. On Linux/aarch64, `rt_sigaction` also
requires the sigset-size argument. `src/habu/prof.f` models the two aarch64
hosts' frames and refuses any other target at load rather than emitting an
aarch64 frame for it; the x86_64 frame, where the trap decoding reads `RIP` and
`RSP`, arrives with that engine's own primitives.

## Executable Images

Drivers use the checked phase chain
`ASM-CODE BUILD-IMAGE SET-SIGID CODESIG2 DRV-WRITE-IMAGE`; the target image file
implements the actual format.
`src/os/image-bytes.f` owns the shared executable byte buffer, endian stores,
patch helpers, and signing blob cursor; target image files own only format
layout policy. Use `M-LEN` and `M-OFF` at target-format boundaries before
calling typed helpers such as `M-BYTES-LEN`, `M-NAME16-LEN`, `M-PAD-OFF`,
`M-LE32@`, `M-LE32!`, and `M-LE64!`; raw byte counts should not cross into
these helpers.
`BUILD-IMAGE ( asm -- img )`, `CODESIG2 ( img -- img )`, and
`DRV-WRITE-IMAGE ( img ptr u8 n -- )` pass nominal phase cells whose payload is
ignored; they exist so the checker can enforce image-build ordering.

- macOS uses Mach-O plus signing.
- Linux uses ELF64 with executable `PT_LOAD` detection requiring read+execute
  and not write. Both Linux seams write the same four program headers, the same
  `PROT-PAGE-MAX` boundary between the text and the read-write tail, and the
  same dynamic table; what the architecture owns is `e_machine`
  (`EM_AARCH64` 183, `EM_X86_64` 62), the interpreter the image names, and the
  GOT relocation type (`R_AARCH64_GLOB_DAT`, `R_X86_64_GLOB_DAT` 6).
  `test/x86-64-seam.f` writes an x86_64 image from an aarch64 engine and checks
  those headers field by field.

The deterministic re-link contract is unchanged across targets: headers are
rebuilt from constants, code is copied from `[rbase, CODELEN)`, and native
refresh reaches a byte-for-byte fixpoint.

Snapshot writers use the matching `snap` token: `BUILD-SNAP-HDR ( n -- snap n )`
creates the current snapshot header state, stale provisional headers are
explicitly invalidated, and `SNAP-WRITE ( snap -- )` consumes the final header
state before streaming the snapshot image.

## Runtime And Snapshot Layout

The engine ABI is currently ARM64:

    x19 XDS   data-stack pointer (grows up)     x9-x15  VS register pool
    x16       literal/call scratch, syscall #   x17     branch-flag scratch
    x20 DATA  data region base                  x21/x22 INP/INE source
    x23/x24   TKA/TKL current token             x25     PEND (open def slot)
    x26 DBASE dict+code region                  x27/x28 NDICT/CP
    sp        machine stack: word frames, locals frames

Snapshots rely on fixed mappings and the trailer convention: magic, old text
base, ndict, region length, data length, and format version. Its size and field
offsets are owned by `src/habu/layout.f` (`SNAP-TRL-BYTES` and the
`SNAP-TRL-*` field constants); the writer, the loader and every reader derive
from there, so a port must never spell the numbers itself. A non-fixed mapping
port must implement full relocation before it can pass refresh.

## Port Gate

After `bin/hb` exists on the target, run only native validation on the target
machine:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f \
  lib/process-argv.f lib/process-env.f lib/memory.f lib/codesign.f \
  tools/build-fixpoint.f tools/build-fixpoint-main.f -- install --force

bin/hb --load test/run.f
```

Language-runtime and model benchmark gates are benchmark-host work, not port
validation.

Do not run Linux port tests from a macOS host by injecting a Linux source
prelude. Bring up `bin/hb` on the Linux machine, let that binary select its own
host target sources, and run the native gate there.
