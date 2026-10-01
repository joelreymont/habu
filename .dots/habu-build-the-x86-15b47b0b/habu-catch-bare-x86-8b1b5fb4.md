---
title: "Refuse bare layout names while x86 target sources load"
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T16:40:27.194285+03:00"
closed-at: "2026-10-01T15:07:03.476729+03:00"
close-reason: "Eleven x86 sources guard with using X64LAYOUT after their last load; on product c3905243 the 17 x86 suites print test: ok and their 252 images are byte-identical to the parent tree's with the same exit statuses (peer-routines manifest 73 of 73); a bare DATA-VA in DATA-REGION, refuses rc 67 naming data-va (rc 0 before) and a bare top-level CODE-OFF in elf.f rc 105; snapshot-format.f and aot-arm.f load after the x86 sources."
blocks:
  - habu-refuse-shadowed-bare-aa1c9b74
---

Problem: the x86 target's layout lives in package `X64LAYOUT` (`src/os/linux-x86-64/target-layout.f`), and the rule that x86 target sources read it qualified is a convention with nothing behind it. A bare read still resolves, to the host's layout global: on the macOS engine bare `DATA-VA` is `$44000000000` and `X64LAYOUT:DATA-VA` is `$340000000`. A Linux host gives both the same value, so a bare read changes no image there and every Linux check passes; only a macOS host builds a different image, and the Mac gate runs no image. Fourteen bare `CODE-OFF` reads remain (`src/os/linux-x86-64/elf.f` 78, 107, 111, 133, 229, 257, 265, 281, 287; `test/x86-64-seam.f:241`; `test/x86-64-kernel-engine.f:303`; `test/x86-64-peer-harness.f:224,282,316`), right only because `CODE-OFF` is `$1000` in every layout.
Acceptance: a bare `DATA-VA`, `DATA-SIZE`, `CODE-OFF`, `IMAGE-TEXT-SIZE-OFF` or `LINUX-DLSYM-SLOT-OFF` does not resolve while an x86 target source loads: the load fails on every host and names the word. The fourteen bare `CODE-OFF` reads become `X64LAYOUT:CODE-OFF`, and the exception for `elf.f` leaves `docs/x86-64.md` and the file headers. The refusal is the check: no gate row compares images built under two layouts.
Known: `undefine` works on baked globals (measured in the share-one review). `REGION`, `REGION-OFF` and `PROT-PAGE-MAX` come from `src/habu/layout.f`, are the same on every host and stay bare.
Files: `src/os/linux-x86-64/target-layout.f`, `src/os/linux-x86-64/elf.f`, `src/habu/boot-x64.f`, `src/habu/kernel-x64.f`, `test/x86-64-seam.f`, `test/x86-64-kernel-engine.f`, `test/x86-64-peer-harness.f`, `docs/x86-64.md` "The target's layout".
Verify: the x86 suites pass on a macOS host and on a Linux host; a bare `DATA-VA` in `boot-x64.f` `DATA-REGION,` fails the load on both.
Route: direct.
Ownership: krait (Intel lane).

Lead correction (2026-10-01, design after the lane's measurements; it overrides the lines above where they differ):
- Mechanism: the existing `E-USING-SHADOW-GLOBAL` rule, enforced at top level by habu-refuse-shadowed-bare-aa1c9b74 (engine rc 105) and in definitions by the checker (rc 67). Each x86 target source opens `using X64LAYOUT` after its last load, closes it with `;using` before its end, and keeps reading every layout value qualified; a bare `DATA-VA`, `DATA-SIZE`, `CODE-OFF` or `IMAGE-TEXT-SIZE-OFF` under the guard then refuses by name on every host. A permanent `undefine` is ruled out: host sources loaded later in the same session read the globals (`src/habu/snapshot-format.f` `TEXT-SIZE`, `src/habu/aot-arm.f` `HERE-N`, the x86 self-build order in `tools/build-fixpoint.f`).
- `LINUX-DLSYM-SLOT-OFF` has no macOS global, so a bare read under the guard resolves to `X64LAYOUT`'s value there: the right value, not a refusal.
- Guarded files: `src/os/linux-x86-64/elf.f`, `src/habu/{boot-x64,kernel-x64,link-x64,prof-x64}.f`, `test/x86-64-{seam,kernel-engine,peer-harness,kernel-crash,boot-signal,kernel-prof}.f`; a test that uses `X64LAYOUT` without requiring `target-layout.f` requires it. Nothing loads under a guard, so host readers loaded later are unaffected.
- The fourteen bare `CODE-OFF` reads are qualified, and so is `src/habu/link-x64.f`'s `TEXT-VA`.
- Verify: the 17 x86 suites with image statuses unchanged; the bare `DATA-VA` mutation in `boot-x64.f` `DATA-REGION,` refused naming `data-va` (rc 67), and a bare top-level `CODE-OFF` in `elf.f` refused (rc 105); `snapshot-format.f` and `aot-arm.f` loaded after an x86 source still work. macOS runs in Alder's gate.
