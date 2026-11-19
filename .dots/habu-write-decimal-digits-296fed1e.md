---
title: Write decimal digits in one word
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:09:26.635721+01:00"
---

Problem: two decimal writers: ERROR-TEXT:DIGITS$ (src/core/util.f, a number's magnitude into one shared 'create' buffer; t0.f, link-kernel.f, link-map.f, checker.f call it) and REJECTION's EMIT-U (src/core/rejection.f, the unsigned value into each task's NUM-BUF; 60 callers), with NUM-BUF's size written as a literal 20 in two places. Acceptance: one word writes a number's decimal digits and every caller uses it; its buffer is each caller's own, so two tasks never share one; the size is a named constant. Depends: habu-reject-a-too-07b69417. Claim: unassigned.
