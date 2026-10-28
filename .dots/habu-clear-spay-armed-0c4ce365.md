---
title: Clear SPAY-ARMED with the check state
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T22:35:56.271750+02:00"
---

src/core/checker.f: SPAY-ARMED is set only in CHECK-SCAN-WALK (~25301) and never cleared at CHECK-CLEAR or TOP-BEGIN (~30239), so a check that exits after arming it leaves it set for the next check. Clear it at CHECK-CLEAR or TOP-BEGIN (or declare it in a checker scratch span). Found in the lane 3b review (O1); kept out of 3b.
