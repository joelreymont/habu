---
title: +loop is refused by the chain and unowned
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T19:54:53.437373+02:00"
---

From thecut-1's census (2026-08-18): +loop refuses -8502 E-NELAB-CTRL through the real entry and no dot owns it (the do/?do/loop/leave/unloop/j family is modeled; +loop is the gap). Model it in the elaborator's control lowering beside its family; judge row + mutation per the family's existing pattern. Cut-blocking under no-acceptable-refusals (Phase B of a5aa3f1f).
