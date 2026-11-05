---
title: Place a native shadow emission at a link address (S6 L2)
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T16:41:28.892976+02:00"
---

Problem: the arm64 link must copy each product routine (a native shadow emission, NSHADOW) to its link-time address and rewrite its rows so it runs there: design r2 D4 (.jj-ws.noindex/carl-dots/.out/carl-file/design-s6-r2.md). Acceptance: A64ASM:ENC-ADRP (Joel 2026-11-05, DECISIONS 'the arm64 page-address encoder'); src/arch/arm64/link.f, package LINKER private part ARM64: CALL! (bl/b, E-A64EMIT-REACH), CODE-ADDRESS! (adrp+add+nop+nop), PLACE (copy + every call, code-address and data-address row); a test runs a placed copy of a fixture word at a fresh code address and gets the original's result. Files: src/arch/arm64/asm.f, src/arch/arm64/link.f (new), test/compiler/arm64-place.f, test/compiler/arm64-place-fixture.f (new), test/gate-arm-cases.f. Verify: rebuilt bin/hb; bin/hb --load test/run.f -- compiler-shadow. Depends: none (S1a landed). Ownership: those files. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-s6-l2.
