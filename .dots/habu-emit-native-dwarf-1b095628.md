---
title: Emit native DWARF symbols
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T13:31:31.732175+03:00"
---

The native build currently emits a custom text map for image-size and generation-difference diagnostics. LLDB/GDB do not read it, and the native executable has no ordinary symbols. Emit debugger-readable native word names and code address ranges using DWARF, with a matching dSYM bundle on macOS and appropriate ELF debug information on Linux. Reuse captured word names/spans and correct image address coordinates; keep the internal analysis map separate. Relevant code: src/os/macos/macho.f, src/os/linux/elf.f, src/os/linux-x86-64/elf.f, tools/native-build-core.f, src/habu/aot-capture.f; see docs/debugging.md for the current native debugging boundary. Acceptance: validate the emitted DWARF with standard tooling, then use LLDB/GDB on a real native executable to resolve a known Forth word and stop at a breakpoint in its code, including ASLR address handling. Start with native word symbols; richer locals/types, source stepping and dynamic JIT registration are separate requirements when requested.
