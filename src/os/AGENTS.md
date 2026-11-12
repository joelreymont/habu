# src/os — target seams

`linux/` (linux-aarch64), `linux-x86-64/` and `macos/` (macos-aarch64) hold
the same file set under the same word names, so a builder selects a seam
instead of branching: `target.f` (target flags and name), `sys.f` (syscalls),
`repl-term.f`, `proc-watch.f`, `proc-control.f`, and the file writer and
signer (`elf.f`/`sign.f`, `macho.f`/`sign2.f`). hb loads both arm64 writers
and the link picks one by the target's file format (`src/habu/link-data.f`
EMIT-IMAGE).
Layout: `macos/layout.f`; both Linux targets load `linux/layout.f` and
`linux/layout-constants.f`, and `linux-x86-64/target-layout.f` is the x86-64
layout for a cross build. `wasm/` holds only `target.f` and `layout.f`: a
Wasm module has no OS seam.

Shared: `env-base.f` (argv, envp), `script-argv.f` (`--load` arguments),
`image-bytes.f` (image byte cursor). Selection fails closed
(`docs/xcomp.md` "Porting").
