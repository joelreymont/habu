---
title: Add the x86-64 code layer
status: active
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.517403+03:00"
---

Problem: `ASM-SINK`, labels and forward rel32 fixups have no home (`src/os/linux-x86-64/sys.f:15-22`; `docs/porting.md:29-35`).
Acceptance: `src/arch/x86-64/icode.f` (package `X64CODE`): a `BUF` sink, `ASM-SINK ( -- ptr u8 )`, labels with rel32/rel8 fixups and `MOVABS` label sites, refusal on unresolved labels; `test/x86-64-emit.f` and `test/x86-64-peer-image.f` bind to it instead of their private sinks. Discharges cross-build obligation (1).
Files: new `src/arch/x86-64/icode.f`, `test/x86-64-emit.f`, `test/x86-64-peer-image.f`.
Verify: spark `bin/hb --load test/x86-64-emit.f` and `bin/hb --load test/x86-64-peer-image.f`; ThinkPad peer statuses 0 (`hb-x64-peer`) and 21 (`hb-x64-peer-negative`) unchanged.
Depends: none.
Route: direct.
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-add-the-x86-aad02c7e.
Preflight corrections (these override the lines above where they differ):
- `X64CODE` publics: `ASM-SINK ( -- ptr u8 )` (the `BUF` header; the driver owns its lifetime through `BUF:INIT`/`CLEAR`/`DISPOSE` on it), `CODE ( -- ptr u8 )`, `ASM-LEN ( -- n )`, `CODE-CAP-BYTES` (a value that passes `src/os/linux-x86-64/elf.f:72-75 ELF-MSIZE-CHECK`; K3 may grow it). These are the names `elf.f:64,78,225`, `sys.f:92-95,142-182` and `proc-watch.f:20-21` consume bare today, supplied by `X64PEER` privates (`test/x86-64-peer-image.f:11,18-19`). A `MOVABS` label site patches the label's VA, `VMBASE CODE-OFF +` plus its offset. An unresolved label, or a rel8 delta outside [-128,127], refuses before any byte is patched (precedent `src/arch/arm64/icode.f:424,442`).
- Files add: `src/os/linux-x86-64/sys.f`, `src/os/linux-x86-64/proc-watch.f`, `src/os/linux-x86-64/elf.f` (each binds with `using X64CODE … ;using`); rewrite the `sys.f:13-22` note and `docs/porting.md:29-36`, which say the test owns the sink.
- A tree that loads `src/arch/arm64/icode.f` and uses `X64CODE` collides (`E-USING-SHADOW-GLOBAL`); `tools/native-emit.f:3` loads it, so K3's x86 arm must exclude it. Not K1's change.
- habu-run-emitted-x86-b704f918 (C8) depends on this leaf and binds to `X64CODE`.
- Route: direct. `docs/porting.md` is documentation that no build loads; the Alder route exists for files a macOS build loads.
