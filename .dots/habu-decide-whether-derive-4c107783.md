---
title: Decide whether DERIVE addr FIELD box<recs> is refused
status: open
priority: 3
issue-type: task
created-at: "2026-10-30T11:20:57.596629+02:00"
---

phil 2026-10-30, unverified on master (seen only on an old engine hb-b3): 'STRUCTURE outer DERIVE addr FIELD b box<recs>' is refused with 7126 while 'outer<a> FIELD b box<a>' is accepted. First reproduce on the installed hb; if it reproduces, find the rule that refuses it and whether the refusal is right; if not, close.
