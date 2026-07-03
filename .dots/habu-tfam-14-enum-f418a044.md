---
title: "TFAM 14: enum families + legacy ENUM migration"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.956105+02:00"
---

PLAN.md item 14. Retire/rename numeric ENUM/ENUM4 chain (src/core/enums.f) BEFORE reserving block ENUM ... END-ENUM; migrate call sites; block enums define checked constructors + exhaustive MATCH; duplicate/missing/bad variants reject. Gate 17n. Depends: TFAM 9-13.
