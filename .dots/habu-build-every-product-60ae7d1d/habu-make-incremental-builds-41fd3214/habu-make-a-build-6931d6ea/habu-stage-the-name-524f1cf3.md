---
title: Stage the name-recording rows in commits
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T14:09:05.460060+01:00"
---

Problem: the name-recording stack (workspace .jj-ws.noindex/jerry-nr) moves search-wl, scope-find and tok-imm? to private CHECKER-RESOLVE bare-* package rows, which the running hb must hold first (docs/bootstrap.md "Staged hosts", src/habu/linker.f OWNED+), but no commit of the stack is a revision that adds them without needing them: the three-stage recipe lives only in /tmp/jerry-nrl3-brief.md. Acceptance: the stack holds stage commits so each commit builds with the engine of the commit before it, from master's installed hb. Depends: none. Claim: unassigned.
