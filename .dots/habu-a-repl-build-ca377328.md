---
title: A --repl build run outside the tree fails E-BUILD-PATH
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T12:04:49.178854+01:00"
---

Problem: from a directory outside the Habu tree, `hb --load <tree>/tools/hb-build.f -- --repl main.f -o prog` dies 'exit-status.f threw -2803 E-BUILD-PATH' (installed hb and master); a plain build from the same place works. tools/hb-build-core.f REG-ROW (:219) takes SOURCE-ROOT:CWD$ as the tree root, so hb's own rows are not below it and are refused. Acceptance: a --repl build gives the same program from any cwd; a test builds one from outside the tree. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-replpath.
