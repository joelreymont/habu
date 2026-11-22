---
title: Let library tokens carry their records
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:11:01.329776+01:00"
---

Problem: five library tokens mint from and erase to ^n and reach their record through casts and offsets, a claim the checker does not prove; LINEAR: now carries a pointer to a closed record (test/linear-suite.f LR1-LR2), as EDIT does (src/lib/byte-edit.f:41). One child per token. Acceptance: the children are closed. Depends: children. Claim: unassigned.
