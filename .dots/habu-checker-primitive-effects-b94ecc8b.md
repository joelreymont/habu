---
title: Checker primitive effects are data
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T16:55:56.885960+02:00"
---

Problem: primitive/signature tables still start as name/signature strings (PTAB/PT+ paths) and are reparsed/compared by string name. Fix: represent primitive rows as structured records {symbol id, canonical effect record, flags}; keep source string only as bootstrap adapter input. Acceptance: FIND-SIG returns an effect record for primitives without reparsing signature text; primitive negative/positive gate tests pass; TRUST stays source-boundary adapter only.
