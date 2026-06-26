---
title: Share bootstrap image buffer
status: closed
priority: 3
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.569674+02:00\\\"\""
closed-at: "2026-06-25T14:44:00.199649+02:00"
close-reason: "completed: commit 305d6429 moved bootstrap image cursor/buffer words to bootstrap/cg/image.fs; bootstrap-codegen-test, trust-lint, stale-status-lint, engine suite, build-helper bundle, full native gate, and local recovery probe passed/recorded"
---

Finding F19. Evidence: docs/factorization-review.md:47; bootstrap/cg/elf.fs:10 and bootstrap/cg/macho.fs:17. Root cause: bootstrap ELF and Mach-O duplicate image buffer writer words. Fix: move common bootstrap image-buffer emitters into one shared file and keep target headers separate. Why: bootstrap executable writers should share cursor semantics. Validate with bootstrap-codegen-test, recovery bootstrap if supported, and native gate.
