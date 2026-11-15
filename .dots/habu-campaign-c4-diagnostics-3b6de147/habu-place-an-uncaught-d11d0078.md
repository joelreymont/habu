---
title: "Place an uncaught throw's record"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:09:22.096626+01:00"
closed-at: "2026-11-16T04:07:35.069321+01:00"
---

Problem: a throw nothing catches prints `hb: uncaught throw code N` from the kernel reporter (src/habu/habu2.f LUNCRPT), with no place, on bin/hb FILE, --load, stdin and from a required file. Acceptance: one REJECTION record at the cause (in the innermost text the throw left), rc 67; a caught throw prints nothing; 1..255 exit silently as themselves. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
