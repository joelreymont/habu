---
title: "Delete FS's uncalled COPY-FILE and READ-LINK"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T18:05:04.104010+02:00"
---

Problem: after the seal's test rewrites, the private words COPY-FILE (src/lib/fs-mutate.f) and READ-LINK (src/lib/fs.f) have no caller in habu, zen, tender, maki or etch (rg -i --no-ignore). Acceptance: both are deleted with any comment that names them; hb builds. Depends: none. Claim: unassigned.
