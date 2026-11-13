---
title: "Write the engine's libc imports from one table"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T10:17:53.610011+02:00"
closed-at: "2026-11-13T12:13:07.525822+02:00"
---

Problem: src/os/macos/macho.f and src/os/linux/elf.f each hard-code two loader imports (dlopen, dlsym); the kernel cannot bind free, realpath or fork at load until the running hb's writer emits them (docs/bootstrap.md Staged hosts), and a writer is in every arm64 hb while each OS layout is not. Acceptance: each writer emits its imports from one table in its own file (macOS adds free, realpath$DARWIN_EXTSN, fork; Linux arm64 adds free, realpath), dlopen and dlsym keep their slots, and engines it writes run on macOS and Linux arm64. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-imports
