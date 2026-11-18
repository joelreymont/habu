---
title: E-SPLIT-STR-LINE on stdin misstates the cause
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T08:47:34.197853+01:00"
---

Problem: a program read from stdin whose first line is split-str is rejected with 'its file's first line is not split-str alone' (src/habu/interpreter.f SPLIT-STR-REJECT), though the line is first and alone: stdin cannot carry the marker. Acceptance: the record says a program read from stdin cannot carry split-str; or the marker is deleted (the parent's acceptance). Depends: none. Claim: unassigned.
