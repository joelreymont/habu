---
title: Write the header row REACHED-CK reads when a reached piece holds a header address
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:17:33.239161+02:00"
---

Problem: linker.f REACHED-CK (carl-words :2234) runs in HEADER-CK before the pieces a file requests are built, so a requested piece that holds a header-data address is ROW-ABSENT and its flag is never read, while RAN-FILES+ (:945) walked it after it was built and took the header storers: the check requires fewer storers than the compile reached. Found by the review of xqvqnuts; no piece is flagged in today's builds. Acceptance: RAN-FILES+ appends the -1 row whenever its walk took the header data's storers (own or a walked row flagged); REACHED-CK reads the -1 alone. Files: src/habu/linker.f. Verify: file-objects. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-hdrf.
