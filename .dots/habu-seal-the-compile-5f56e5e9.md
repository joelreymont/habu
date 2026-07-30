---
title: Seal the compile-time builder capability
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T14:49:48.011339+02:00"
---

Full context: design section 7.1 class 2 says a checked immediate word may run during elaboration but may affect the generated program ONLY through a sealed HIR-builder capability. src/compiler/native/immediate.f records that a word is in that class, but nothing yet enforces the 'only through the builder' half, because there is no HIR builder to seal against (habu-elaborate-straight-line-72b55798). Today NIMM-CLASS:COMPILE-TIME is a declared intent, not a proof. Required result: once the HIR builder exists, give it a sealed capability that a compile-time immediate must hold to emit anything, and make holding it the only way to reach the builder. Acceptance: a compile-time immediate that tries to emit without the capability is refused by name; the capability cannot be minted by checked code outside the builder; a checker fixture proves a forged capability rejects; NIMM's compile-time class is bound to it. Dependency: straight-line HIR elaboration.
