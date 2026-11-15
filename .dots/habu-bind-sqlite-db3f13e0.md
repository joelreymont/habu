---
title: Bind SQLite
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T20:22:45.365759+02:00"
---

Problem: Habu has no SQLite binding; a program that keeps a transactional journal in SQLite needs one (omabox docs/habu-rewrite.md, Habu libraries). Acceptance: src/lib/sqlite.f, package SQLITE over libsqlite3: a task-owned connection opened read-write or read-only without following a symlink, a busy timeout, EXEC, a linear statement inside WITH-STATEMENT with integer, text, blob and null binds and column reads, WITH-TRANSACTION as BEGIN IMMEDIATE with rollback on a throw, the change count and last rowid; every failure logged once with SQLite's text and thrown as one of corrupt, full or other; src/lib/sqlite-test.f. Depends: none. Claim: unassigned.
