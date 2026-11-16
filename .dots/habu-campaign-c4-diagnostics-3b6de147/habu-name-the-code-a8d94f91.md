---
title: "Name the code in a built program's uncaught line"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T09:25:16.164204+01:00"
---

Problem: a built program's uncaught throw prints only `hb: uncaught throw code N` (habu2.f LUNCRPT), so a built program that fills its data space says -9850 with no name or text; app-end.f ENTRY sends only a record the throw carried. Acceptance: a built program's uncaught throw of a code with an error.f row names the row's code and text. Depends: habu-kernel-stops-write-c3fef65f. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w218.
