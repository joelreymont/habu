---
title: Place a native shadow emission at a link address (S6 L2)
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T16:41:28.892976+02:00"
closed-at: "2026-11-05T19:30:41.269596+02:00"
close-reason: "Landed on master d614ca41 (c3bdcad8 placement: A64ASM:ENC-ADRP, src/arch/arm64/link.f LINKER part ARM64 CALL!/CODE-ADDRESS!/PLACE, test/compiler/arm64-place.f; d614ca41 the one assembler: ENC-B/BL/BCOND/CBZ/CBNZ take byte distances and refuse unreachable or misaligned values with E-A64EMIT-REACH, T0, tier 1, the code buffer, debug.f and the linker call them with no reach check of their own). Reviewed by code-reviewer. Proof at the landed tip: engine g1 from the installed hb, g2 from g1, two-gen: same image; full suite on g2 514 of 514 green; build-fixpoint refresh install OK."
---

Problem: the arm64 link must copy each product routine (a native shadow emission, NSHADOW) to its link-time address and rewrite its rows so it runs there: design r2 D4 (.jj-ws.noindex/carl-dots/.out/carl-file/design-s6-r2.md). Acceptance: A64ASM:ENC-ADRP (Joel 2026-11-05: "Public beside ENC-ADR; the assembler owns every instruction encoding."); src/arch/arm64/link.f, package LINKER private part ARM64: CALL! (bl/b, E-A64EMIT-REACH), CODE-ADDRESS! (adrp+add+nop+nop), PLACE (copy + every call, code-address and data-address row); a test runs a placed copy of a fixture word at a fresh code address and gets the original's result. Files: src/arch/arm64/asm.f, src/arch/arm64/link.f (new), test/compiler/arm64-place.f, test/compiler/arm64-place-fixture.f (new), test/gate-arm-cases.f. Verify: rebuilt bin/hb; bin/hb --load test/run.f -- compiler-shadow. Depends: none (S1a landed). Ownership: those files. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-s6-l2.
