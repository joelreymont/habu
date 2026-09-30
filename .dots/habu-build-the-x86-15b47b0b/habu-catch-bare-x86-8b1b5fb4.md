---
title: "Refuse bare layout names while x86 target sources load"
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T16:40:27.194285+03:00"
---

Problem: the x86 target's layout lives in package `X64LAYOUT` (`src/os/linux-x86-64/target-layout.f`), and the rule that x86 target sources read it qualified is a convention with nothing behind it. A bare read still resolves, to the host's layout global: on the macOS engine bare `DATA-VA` is `$44000000000` and `X64LAYOUT:DATA-VA` is `$340000000`. A Linux host gives both the same value, so a bare read changes no image there and every Linux check passes; only a macOS host builds a different image, and the Mac gate runs no image. Fourteen bare `CODE-OFF` reads remain (`src/os/linux-x86-64/elf.f` 78, 107, 111, 133, 229, 257, 265, 281, 287; `test/x86-64-seam.f:241`; `test/x86-64-kernel-engine.f:303`; `test/x86-64-peer-harness.f:224,282,316`), right only because `CODE-OFF` is `$1000` in every layout.
Acceptance: a bare `DATA-VA`, `DATA-SIZE`, `CODE-OFF`, `IMAGE-TEXT-SIZE-OFF` or `LINUX-DLSYM-SLOT-OFF` does not resolve while an x86 target source loads: the load fails on every host and names the word. The fourteen bare `CODE-OFF` reads become `X64LAYOUT:CODE-OFF`, and the exception for `elf.f` leaves `docs/x86-64.md` and the file headers. The refusal is the check: no gate row compares images built under two layouts.
Known: `undefine` works on baked globals (measured in the share-one review). `REGION`, `REGION-OFF` and `PROT-PAGE-MAX` come from `src/habu/layout.f`, are the same on every host and stay bare.
Files: `src/os/linux-x86-64/target-layout.f`, `src/os/linux-x86-64/elf.f`, `src/habu/boot-x64.f`, `src/habu/kernel-x64.f`, `test/x86-64-seam.f`, `test/x86-64-kernel-engine.f`, `test/x86-64-peer-harness.f`, `docs/x86-64.md` "The target's layout".
Verify: the x86 suites pass on a macOS host and on a Linux host; a bare `DATA-VA` in `boot-x64.f` `DATA-REGION,` fails the load on both.
Route: direct.
Ownership: krait (Intel lane).
