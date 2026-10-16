---
title: Delete retired export commenting
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:42.029564+03:00"
---

src/lib/source.f COMMENT-EXPORTS$ parser/storage/size pass has no consumer except own tests and stale comments; engine C-EXPORT already handles top-level export. Remove exclusive pipeline/helpers/tests and update stale engine/checker comments; preserve whole-file/stdin reading, assembly, quoting and shared lexical helpers. Existing source and real compiler loaders.
