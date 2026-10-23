# src/os — target seams

`linux/` (linux-aarch64), `linux-x86-64/` and `macos/` (macos-aarch64) hold
the same file set under the same word names, so a builder selects a seam
instead of branching: `target.f` (target flags), `sys.f` (syscalls),
`repl-term.f`, `proc-watch.f`, `proc-control.f`, and the image writer and
signer (`elf.f`/`sign.f`, `macho.f`/`sign2.f`).
Layout: `macos/layout.f`; both Linux targets load `linux/layout.f` and
`linux/layout-constants.f`, and `linux-x86-64/target-layout.f` is the x86-64
layout for a cross build.

Shared: `env-base.f` (argv, envp), `script-argv.f` (`--load` arguments),
`image-bytes.f` (image byte cursor). Selection fails closed
(`docs/xcomp.md` "Porting").
