---
title: Say how to build past a deleted engine file
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T10:40:49.302470+02:00"
---

Problem: docs/bootstrap.md covers a renamed file hb carries (What the engine carries) but not a deleted one: an hb whose build list names the file cannot build a tree that deleted it, as building 60818c07 with its parent's hb showed; it built from an exported copy with the file put back. Acceptance: docs/bootstrap.md gives the procedure, checked by building a deletion of a carried file with the previous engine. Depends: none. Claim: unassigned.
