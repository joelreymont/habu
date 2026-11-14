---
title: "Point open dots' citations at the lines that hold them"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T08:47:37.705877+01:00"
---

Problem: open dots cite lines that no longer hold the word they name: of 232 citations written 'path WORD (:N)' or 'WORD (path:N)', 136 miss WORD within three lines of N (TASK-KILL src/lib/task.f:1444 is '0 k ROW-NAME!'; TASK-KILL is at :1455; BPOLL src/habu/habu1.f:975 is '1+'; BPOLL is at :804), and bare ':N' citations after another word drift the same way. A dot that points at the wrong line sends its worker to the wrong code. Acceptance: every open or active dot's line citation names a line that holds what the clause says, a clause whose code is gone is cut, and a word-checked scan of the two forms above finds no miss. Depends: none. Claim: unassigned.
