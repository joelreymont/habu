---
title: Let a calling quotation live under a locals group
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T19:06:58.691499+02:00"
---

Found pre-existing by the exceptions lane (6ceb7667, reproduced on the PARENT binary through the pre-catch route): a quotation body that CALLS, compiled under a definition with a locals group, refuses E-IR-VERIFY-SCOPE. This is what bounds the dominant catch production shape today - [: WORD ;] catch {: rc :} rc 0<> if rc throw then compiles only while the caught body does not call. Files: src/compiler/native (quotation build path / scope rows - find the owner). Depends: none technical; sibling of habu-compile-a-quotation-7efa798e.
