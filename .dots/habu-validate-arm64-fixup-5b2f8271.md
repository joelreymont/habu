---
title: Validate ARM64 fixup kinds
status: open
priority: 1
issue-type: task
created-at: "2026-07-14T04:34:16.790670+02:00"
---

Full context: src/arch/arm64/icode.f dispatches fixup kind 0 as B26, 1 as B19, and every other value as ADR, so corrupt/internal invalid kinds silently patch as ADR. Root fix: introduce named FX-B26, FX-B19, and FX-ADR constants and validate kinds before any table or code mutation. Acceptance: all six branch/ADR emitters produce exact encodings; an invalid kind exits 72 with an exact diagnostic and leaves fixup chains, free list, NFX, and code unchanged; codegen-role tests use named kinds. Files: src/arch/arm64/icode.f, tools/codegen-role.f/test if required, focused assembler regression. Depends on fixup-chain reclamation.
