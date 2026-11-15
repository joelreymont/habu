---
title: Label a path record that stands off the path
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:30:07.310760+01:00"
closed-at: "2026-11-16T04:07:35.659483+01:00"
---

Problem: LOADER-PATH-STOP (src/core/include.f ~:108) labels the token it falls back to (the loader word or the last token read) 'holds a NUL byte', 'too long' or 'empty path', which is false of that token. Acceptance: a record not at the path (LOADER-PATH-STOP, ROOT-LONG, PATH-DIE) labels the token as passing the path, saying what is wrong with it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
