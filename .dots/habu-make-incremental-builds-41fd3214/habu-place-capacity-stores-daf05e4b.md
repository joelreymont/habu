---
title: Place capacity stores a rolled-back grow moved
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T07:09:25.829088+01:00"
---

Registry stores persisted at full capacity through REG-PERSIST-BUF (TF-A, TF-PK, SUMV, PF, LAY in type-family.f TFAM-SNAPSHOT-PERSIST; SCH-A/SCH-ROOT in type-schema.f; CT, VREC, VRN, VNARG, SYMS in checker.f; USIGS) keep the capacity a grow doubled to. A rolled-back declaration that adds more rows than a store's baked spare grows it; the rewind lowers the count but the store stays moved and doubled, so the cold image differs from a hit's, which never grew. The used-size pools got a home rule (rolltip lane, REG-POOL-PERSIST); decide whether capacity stores need the same or the rewind should return them. Reproduce with a rolled-back declaration past a store's spare (ROLL-HIT-CASE shape).
