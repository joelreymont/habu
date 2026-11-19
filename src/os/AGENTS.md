# src/os — target seams

`linux/` (linux-aarch64), `linux-x86-64/` and `macos/` (macos-aarch64) hold
the same file set under the same word names, so a builder selects a seam
instead of branching: `target.f` (target flags and name), `sys.f` (syscalls),
`proc-watch.f` and `proc-control.f`. The file writers and signers are
`linux/elf.f`/`linux/sign.f`, which both Linux targets use, and
`macos/macho.f`/`macos/sign2.f`; hb loads both and the link picks one by the
target's file format (`src/habu/link-data.f` EMIT-IMAGE).
Layout: `macos/layout.f`; both Linux targets load `linux/layout.f`, package
LINUX-LAYOUT, and `linux/host-layout.f`, its bare copies. `wasm/`
holds only `target.f` and `layout.f`: a Wasm module has no OS seam.

Shared: `env-base.f` (argv, envp), `script-argv.f` (`--load` arguments),
`image-bytes.f` (image byte cursor), `import-names.f` (the import names an
image writer is given), `linux-flags.f` (the BSD-to-Linux open and mmap flag
bits both Linux `sys.f` files translate). Selection fails closed
(`docs/xcomp.md` "Porting").
