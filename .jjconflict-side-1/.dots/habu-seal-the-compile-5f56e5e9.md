---
title: Seal the compile-time builder capability
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T14:49:48.011339+02:00"
---

Full context: design section 7.1 class 2 says a checked immediate word may run during elaboration but may affect the generated program ONLY through a sealed HIR-builder capability. src/compiler/native/immediate.f records that a word is in that class, but nothing yet enforces the 'only through the builder' half, because there is no HIR builder to seal against (habu-elaborate-straight-line-72b55798). Today NIMM-CLASS:COMPILE-TIME is a declared intent, not a proof. Required result: once the HIR builder exists, give it a sealed capability that a compile-time immediate must hold to emit anything, and make holding it the only way to reach the builder. Acceptance: a compile-time immediate that tries to emit without the capability is refused by name; the capability cannot be minted by checked code outside the builder; a checker fixture proves a forged capability rejects. Dependency: straight-line HIR elaboration.

Amended 2026-08-20 (habu-delete-the-nimm-329100c9): the fourth acceptance clause, "NIMM's compile-time class is bound to it", is dropped. There is no NIMM: src/compiler/native/immediate.f was deleted under the user's hard-cut ruling because nothing ever asked it, so a clause binding this capability to that table can no longer be satisfied and would not be worth satisfying - a class record with no consumer is what got deleted. The design commitment this leaf owns is unchanged and is the whole of it: when a checked immediate may run during elaboration, the only way it reaches the generated program is a sealed builder capability. Whoever lands that also decides how such a word is recognised in the first place, and re-derives a classifier from history if it wants one.
