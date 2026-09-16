---
title: "Campaign C6: targets"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:54:06.641317+03:00"
---

Problem: native compilation exists for arm64 on Linux and macOS only. src/arch/arm32 (ARMv7-R A32 and ARMv7E-M Thumb-2) and src/arch/tic6x hold instruction constructors with no lowering, image layout or load path behind them (docs/embedded-encoders.md); there is no x86_64 backend; the cross-target load, run and inspect loop over serial that SwiftX provides does not exist. One program text cannot yet run on the server and on a microcontroller. Acceptance: one program compiles for arm64 and x86_64 hosts and for one Cortex-M board through the target registry, and the board image can be loaded and inspected over serial. Children (open): habu-bind-compiler-targets-ff970b99 habu-design-the-x86-330bc78c habu-design-the-first-61f718f0 . Absorbed on 2026-09-16: 3 dots closed with the reason 'superseded by habu-campaign-c6-targets-86bb56bb'; find their text with dot find. Files: src/compiler/target.f, src/arch/, src/os/, docs/porting.md, docs/embedded-encoders.md, docs/roadmap.md section C6. Verify: per-target gates; test/serial.py and test/xmodem.py device peers. Depends: habu-bind-compiler-targets-ff970b99. Ownership: compiler lane. Claim: unassigned. Absorbed: see the archive entries closed with 'superseded by' this id.
