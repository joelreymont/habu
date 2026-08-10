---
title: Package the arm64 assembler
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T19:23:58.210921+02:00"
---

src/arch/arm64/asm.f predates the package rule and publishes 146 global names (ENC-B, ENC-ADR, ...); today they are baked into the metabuild only, but the chain's seed closure pulls asm.f into EVERY boot (Stage B) at which point the globals collide with any package's own names under using - the checker already refuses it (E-USING-SHADOW-GLOBAL on maki/onnx's enc-b, seeda lane 2026-08-11: the checker doing its job). Give asm.f a real package per CLAUDE.md's rule; the call-site migration covers the native chain's encoders (a64ir/emit reference ENC-* bare) and ~11 direct requirers. ORDERED BEFORE habu-seed-the-chain-e98b03d4 (Stage B). asm.f is CUT from Stage A (the eight lib files land without it). Files: src/arch/arm64/asm.f + call sites. Depends: none. Blocks: habu-seed-the-chain-e98b03d4.
