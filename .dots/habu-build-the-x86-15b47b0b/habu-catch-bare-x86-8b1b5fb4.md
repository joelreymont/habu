---
title: Catch bare x86 layout reads on a Linux gate
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T16:40:27.194285+03:00"
---

Problem: a bare x86 layout read in `src/os/linux-x86-64/elf.f`, `src/habu/boot-x64.f` or `src/habu/kernel-x64.f` binds the host layout global instead of `X64LAYOUT:`. On Linux the host value equals the target value, so every image is unchanged and every Linux check passes; only a macOS host builds different images, and the Mac gate runs no image. Measured in the share-one review: a bare `DATA-VA` in `DATA-REGION,` leaves `hb-x64-skel` byte-identical on Linux and puts `$44000000000` into it once the macOS values stand in for the globals (`undefine` works on baked globals, then redefine with `src/os/macos/layout.f` values).
Acceptance: a gate row builds the images of every booted x86 suite twice, as is and with the macOS layout values stood in for the host layout globals, and fails naming the first image whose bytes differ. Checked Habu only; the row spawns `bin/hb` per build.
Files: a new test under `test/`, `test/gate-stdlib-cases.f`, `docs/x86-64.md` "The target's layout".
Verify: green on master; a bare `DATA-VA` in `boot-x64.f` `DATA-REGION,` turns it red naming `hb-x64-skel`.
Route: direct (test-only).
Ownership: krait (Intel lane).
