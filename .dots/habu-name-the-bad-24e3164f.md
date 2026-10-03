---
title: Name the bad signature in C-SIG-BAD
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T11:53:54.565335+02:00"
---

C-SIG-BAD prints no message text: `TRUSTED: W 1 ;` refuses rc 76 with only `W at <file>:2`, at both tiers and on master's engine. It should say what is wrong with the signature. Found by the Fable review eof-rev6 (finding 4) while landing habu-eof-inside-a-7a539941; outside the end-of-source path.
