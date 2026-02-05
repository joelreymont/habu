---
title: Load logical-path
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-05T20:10:48.458193+01:00\""
closed-at: "2026-02-05T22:06:50.588577+01:00"
close-reason: Implemented loader and covered via stdlib_paths test
---

Context: /Users/joel/Work/habu/lib/stdlib.habu (Pathname section); cause: missing loader; fix: search <host>.translations.lisp in *default-pathname-defaults* else ./, read list, set translations; deps: habu-add-logical-path-f29f4d16; verification: temp file load returns t + table updated
