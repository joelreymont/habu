---
title: One splice-meaning table
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T20:53:12.419354+02:00"
---

Destruction review of NINL, low. SPLICE-MEANING? (elaborate.f:1442-1455) answers true for literal and real-literal; INLINE-NAME (elaborate.f:2374-2375) throws E-NELAB-INLINE for both. Unreachable today (LITERAL only arises from token kinds) but the table is framed as future-proofing and its yes aborts a migration hard where every other inline refusal is a soft fall-back-to-call. Unify the two tables into one authority and make the refusal soft or the table honest.
