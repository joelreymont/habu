---
title: Put each entry locals group on its own line
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T06:05:52.003961+01:00"
---

Problem: docs/forth-card.md:161 says a {: … :} group that binds at entry goes on its own line after the name and stack effect, but 6207 words in src and 1693 in tools and test bind on the name line (rg '^: \S+ \( [^)]*\) \{:'), among them compile-reads.f UNMAP, SPAN, ROOM, RECORD and FILE+; each new change copies the form beside it. Acceptance: that rg finds nothing in src, tools or test; a rewrite of layout alone, so the engine built before and after is byte-identical apart from source positions, and the full suite passes. Depends: the in-flight lanes landing (it touches every file they do). Claim: unassigned.
