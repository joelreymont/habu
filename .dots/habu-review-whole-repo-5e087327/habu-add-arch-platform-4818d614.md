---
title: Add arch/platform stack effects
status: closed
priority: 1
issue-type: task
created-at: "\"2026-06-25T12:19:43.512635+02:00\""
closed-at: "2026-06-25T12:32:32.499636+02:00"
close-reason: "completed: added definition-local stack effects to src/arch/arm64/asm.f, src/arch/arm64/mnem.f, src/arch/arm64/icode.f, src/os/linux/elf.f, src/os/macos/macho.f, and src/os/macos/sign2.f in commit d355621e. Evidence: mechanical missing-comment scan returned no matches for those files; focused Linux and macOS source loads passed; trust-lint reported 236 TRUST site(s), 318 manifest row(s), 0 finding(s); full native gate passed."
---

Finding F03. Evidence: docs/factorization-review.md:31; src/arch/arm64/asm.f:6, src/arch/arm64/mnem.f:11, src/arch/arm64/icode.f:9, src/os/macos/macho.f:12, src/os/linux/elf.f:11. Root cause: encoder/platform files use prose and locals without formal definition-local stack effects. Fix: add exact ( in -- out ) comments to every definition and factor repeated encoder signatures into shared wrappers where duplication becomes obvious. Why: raw encoder code is not exempt from checked review discipline. Validate with relevant focused build/disasm tests and full native gate.
