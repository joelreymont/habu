---
title: Fix stdlib CompileError in subseq
status: closed
priority: 2
issue-type: task
created-at: "\"2026-01-17T05:08:32.761064+02:00\""
---

stdlib.habu:682: subseq function fails to compile with setf. Check if setf macro exists, if not replace with set! or fix setf implementation. Blocking stdlib load.
resolved-at: "2026-01-17T11:55:00+02:00"
resolution: "Already resolved: stdlib loads without CompileError after math_ext fix"
