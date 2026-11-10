---
title: "Measure: what a scratch reset at each file's end breaks and costs"
status: active
priority: 1
issue-type: task
created-at: "2026-11-10T13:04:39.158487+02:00"
---

Problem: design habu-design-no-file-298feb05 has the linker zero every scratch cell in file data at the end of each compiled piece. Not determined by the design (read only): which files rely on a scratch cell surviving a piece's end (their own require included), and what the reset costs a cold build and a one-file edit. The cost can reopen DECISIONS 'scratch storage lives apart' and so what the shared scratch declaration is, so it comes before the name is put to Joel. Acceptance: a prototype of the reset in PIECE-END only (not landed); engine built through it three generations; suite run through it with each failing row's cause as path:line; cold build, warm edit and suite times before and after; rows visited and cells zeroed per build. Files: src/habu/linker.f (prototype only). Verify: the report's numbers and failure list. Depends: none. Ownership: none (nothing lands). Claim: agent=jerry workspace=.jj-ws.noindex/jerry-reset.
