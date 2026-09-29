---
title: Add Linux-family predicates to the libraries
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.782660+03:00"
blocks:
  - habu-run-bin-hb-6378f297
---

Problem: three shapes. Fail-closed three-arm selectors that refuse on x86: `lib/signal.f:51-57,78-86,309-310`, `lib/codesign.f:111-137`, `lib/genio.f:286-287`, `lib/fs-mutate.f:335-337` (`OPEN-FLAGS` -> `E-FS-OPEN`). Fail-open `HB-TARGET-MACOS? if ... else <linux> then` forms that take the Linux arm on x86 by accident: `lib/net/tcp4.f:89,91`, `lib/net/udp4.f:60,122,135,173,211,230`, `lib/serial.f:57-60,138,146,181,189,200,259,269,302`, `lib/aio.f:157-158,1173`. A Linux-only block skipped on x86: `lib/serial.f:279`. No predicate at all: `lib/net/curl.f`, `lib/crypto/evp.f` (their `E-PLATFORM` is library availability). One explicit x86 arm: `lib/engine-id.f:62`.
Acceptance: `HB-TARGET-LINUX-KERNEL?` defined in every `target.f` (true for both Linux seams) for kernel facts (signals, ioctls, socket flags, errno, open flags); every listed selector rewritten to the closed form with a named refusal per `docs/porting.md:63-67`; the listed suites green on both hosts.
Files: `src/os/linux/target.f`, `src/os/linux-x86-64/target.f`, `src/os/macos/target.f`, the listed libraries.
Verify: spark gate (no ARM64 change); ThinkPad: the listed libraries' suites.
Depends: habu-run-bin-hb-6378f297 (X6).
Route: Alder (shared: src/os/linux/target.f, src/os/macos/target.f, the listed lib/ files).
Ownership: krait (Intel lane).
Claim: unassigned.
