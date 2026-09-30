---
title: Share one x86 target-layout package
status: closed
priority: 3
issue-type: task
created-at: "2026-09-30T13:02:53.346419+03:00"
closed-at: "2026-09-30T16:15:47.449904+03:00"
close-reason: "done: X64LAYOUT in src/os/linux-x86-64/target-layout.f is the one replay; elf.f, boot-x64.f and kernel-x64.f read it qualified. [ThinkPad x86 proof: 20 suites ok, 128 hb-x64-* images name, status and sha12 equal to master, manifest bad=0; the same with macOS layout values stood in for the host globals.]"
blocks:
  - habu-port-the-crash-99c87339
  - habu-emit-the-x86-3b63853e
---

Problem: the x86 target layout (`src/os/linux-x86-64/layout.f`) is replayed inside three packages so its constants do not collide with the host's globals: X64BOOT (`src/habu/boot-x64.f:32`), X64KERNEL (`src/habu/kernel-x64.f:45`) and X64LAYOUT (`src/os/linux-x86-64/elf.f`, from X4a habu-write-and-link-f6e6017f).
Acceptance: one package owns the replayed x86 target layout; the three users read it through that package; no other replay remains; host globals stay untouched (the seam suite runs on the macOS gate).
Files: `src/os/linux-x86-64/elf.f`, `src/habu/boot-x64.f`, `src/habu/kernel-x64.f`, the owning file, `docs/x86-64.md`.
Verify: ThinkPad: every x86 suite, every `hb-x64-*` image natively, the x64-routines manifest loop bad=0; byte-identical images before and after.
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: krait.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Owner: new `src/os/linux-x86-64/target-layout.f`, package `X64LAYOUT` (the package `elf.f:30` opens today; K10d's `test/x86-64-boot-signal.f:111` reads it). No `require`s, loads at top level (K10c loads it on its own before `boot-x64.f`). It replays `layout.f` privately and after `public` copies `NAME constant NAME` for exactly `DATA-VA`, `DATA-SIZE`, `CODE-OFF`, `IMAGE-TEXT-SIZE-OFF`, `LINUX-DLSYM-SLOT-OFF` (copies, not EXPORTs: `elf.f:26-29`, E-CAP-TRUSTED); the rest stays private and defines no globals.
- Every read is qualified `X64LAYOUT:NAME`, never bare and never under `using X64LAYOUT`: `boot-x64.f:104,113,114`, `kernel-x64.f:61,876-878,1475,1477`, `elf.f:109,188`, the new reads K10b and K10d add (K10d's `FD-WORD-VA` reads `DATA-VA`; K10b reads `CODE-OFF`), and X64PROF (K10c). Why: `src/os/linux/layout.f` and `src/os/linux-x86-64/layout.f` differ only in comments, so a leftover bare read binds the host global and gives the same bytes on Linux; only a Mac host differs (`src/os/macos/layout.f:6-10`), and the Mac gate never runs the images (`test/gate-stdlib-cases.f:1350-1352`); a bare interpret-level read under `using` silently bound the host's `$2000000` (measured on the master engine).
- `boot-x64.f`, `kernel-x64.f`, `elf.f` require the file above their `package` lines and drop their replays (`:36-38`, `:47-50`, `:22-35`). `elf.f`'s bare `CODE-OFF` reads stay global (image-builder surface, `elf.f:2-3,269`; `$1000` on every host).
- Also edit: `tools/hb-build-lib.f:502-510` (build key), `tools/lint/shadow-lint.f:217-220`, `tools/build-fixpoint.f:925-929` (append the file as a module before `elf.f`), `docs/x86-64.md:580-582`, `test/x86-64-peer-harness.f:34-35`.
- Measured violation: `rg -n 'linux-x86-64/layout.f" included' src` finds three replays today and must find one afterwards.
- Verify add: on Alder's Mac gate the booted suites' `hb-x64-*` images equal the ThinkPad's byte for byte (measure on the base first).
- Depends: K10b `habu-port-the-crash-99c87339`, K10d `habu-emit-the-x86-3b63853e`. Base: master after K10b, K10d, K9b and K11c land. Route: Alder (Mac gate; shared tools).
