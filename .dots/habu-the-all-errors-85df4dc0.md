---
title: The all-errors pass halts at the first undefined
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T12:11:32.005150+02:00"
---

Found by route3-1 (2026-08-20): the whole-buffer multi-error pass continues past E-MISMATCH but HALTS at the first E-UNDEFINED, hiding every finding after it - measured with a five-definition fixture where the last mismatch never appears. checker.f:6770-6778 claims the pass reaches every other error in the file; the claim is false for the undefined class. Fix the pass or fix the claim; a diagnostic mode that silently under-reports is the same defect family as fail-open gates.
