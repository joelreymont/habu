---
title: Design the first microcontroller target
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:54:54.728386+03:00"
---

Problem: src/arch/arm32 holds ARMv7E-M Thumb-2 instruction constructors with no lowering, image layout, load path or debug loop behind them, so no Habu program runs on a microcontroller; SwiftX's cross-target workflow (compile on the host, load over serial, inspect the running target) is the reference (docs/tasking-models.md section 2.1). Acceptance: a section in docs/embedded-encoders.md or a new docs/cortex-m.md fixing: the board and toolchain-free flash or serial load path, the image layout and startup, which VM words are primitives on the target, how the cooperative kernel (habu-decide-the-cooperative-b463cc1c) and vectored terminal I/O serve the serial console, the emulator or device peer under test/ used by the gate, and the first demo program; it opens the implementation dots. Files: docs/embedded-encoders.md, docs/roadmap.md section C6. Verify: the section answers each point; the child dots exist. Depends: habu-bind-compiler-targets-ff970b99, habu-decide-the-cooperative-b463cc1c. Ownership: compiler lane. Claim: unassigned.

State: the design document landed as `docs/cortex-m.md` (1f0386990579), with a work breakdown of the dots to open. Opening them is what remains, and it waits on habu-decide-the-cooperative-b463cc1c.

Design input: docs/portability.md section 16 (freestanding, kernel and TI profiles, its package P10) and its test rows L12-L20 (section 25.4).
