---
title: Scrub the registry rows a rewind retires
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T06:17:21.244671+01:00"
closed-at: "2026-10-28T17:13:33.027174+02:00"
close-reason: landed 17bf6b07
---

Workspace carl-hscr (commit wqlnstmy on wpvzuqor). A rolled-back declaration leaves family/variant/schema rows past the rewound counts in a cold image; TFAM-REWIND and schema REWIND scrub them like PF-REWIND. Brief ~/.cache/tmp/carl-file/brief-scrub.md.
