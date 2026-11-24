---
title: "Drop the clobber lint's rows for words nothing defines"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:49:40.819466+02:00"
---

Problem: tools/lint/clobber-lint-core.f names words no file in src defines: the `MOVZHW,`, `MOVKHW,` and `MOVNHW,` rows (:276-277; src names them only in comments) and `C-FIND-GLOBAL` (GLOBAL-FIND? :206 with its arms at :209-225 and :772, and the comments at :193 and :214). Acceptance: those rows, arms and comments go; the lint run over src reports what it reported before. Depends: none. Claim: unassigned.
