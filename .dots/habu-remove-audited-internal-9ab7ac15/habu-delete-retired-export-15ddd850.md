---
title: Delete retired export commenting
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:42.029564+03:00"
closed-at: "2026-10-16T11:44:00.737161+03:00"
close-reason: Deleted the obsolete export-commenting parser, buffers and dedicated tests; engine EXPORT already handles directives. Source, stdin, discovery, whole-source reader, export-package and focused loader checks pass; caller trace and diff review complete.
---

src/lib/source.f COMMENT-EXPORTS$ parser/storage/size pass has no consumer except own tests and stale comments; engine C-EXPORT already handles top-level export. Remove exclusive pipeline/helpers/tests and update stale engine/checker comments; preserve whole-file/stdin reading, assembly, quoting and shared lexical helpers. Existing source and real compiler loaders.
