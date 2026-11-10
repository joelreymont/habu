---
title: BUILT+ and PIECE-PLACE take the part FILE-PART already holds
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T21:43:45.365372+02:00"
---

Problem: src/habu/linker.f BUILT+ ( piece t -- ) and PIECE-PLACE take the part number as an argument, and every caller passes `FILE-PART @` (LINK-PART, NEST, the compile's end) or 0 with FILE-PART set to 0 (OWNED-RECORDS). IDENTIFY and PART-REC read FILE-PART, so the argument and the cell are two sources for one fact: OWNED-RECORDS passing 0 while FILE-PART held an earlier build's part placed the open's records under the wrong part (fixed by setting the cell). Acceptance: one source: the words read FILE-PART and take no part argument, or IDENTIFY takes it from its caller; no behaviour change. Files: src/habu/linker.f. Verify: test/file-objects.f passes; an engine build's objects are byte-equal before and after. Depends: the incremental-builds stack landed. Ownership: src/habu/linker.f. Claim: unassigned.

2026-11-11 (carl): the lead makes the change. Claim: agent=carl workspace=.jj-ws.noindex/carl-fix.
