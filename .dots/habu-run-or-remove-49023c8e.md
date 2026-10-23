---
title: Register native-wide-frame by what it reads
status: open
priority: 3
issue-type: task
created-at: "2026-10-23T17:33:29.580954+03:00"
---

Problem: test/gate-arm-cases.f's comment above native-identity-spill and native-wide-frame says both read AArch64 instructions, but native-wide-frame only checks 60 snapshots surviving 64 conditional calls and the call count, so it may belong in the shared registry rather than the ARM-only one (g7 test audit lane).
Acceptance: native-wide-frame sits in the registry its behaviour needs and the comment states what each row reads.
Verification: the affected rows pass on arm64; the registry entry guard passes.
