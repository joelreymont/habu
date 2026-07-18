---
title: TMA legality design-rule family + negative fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T13:18:59.518275+02:00"
---

docs/tma-gather.md missing-piece-3. Checker rules keyed to TMA lowering choice: base align-16, strides %16B==0, box dims <=256, dtype set, smem box fits target stage budget. Evidence order: plan-derived (padding) > declared marks > runtime witness/refusal. Negative fixture: hand-forced TMA plan on misaligned span must be refused naming the rule. Rules land BEFORE emitter (rules first, engine second). After 'MOVE plan node'; parallel with pointer-increment dot.
