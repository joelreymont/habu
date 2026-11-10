---
title: "Record a header store that rewrites the cell's value"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:17:33.228671+02:00"
---

Problem: src/habu/linker.f FIND-STORES (carl-words :1670) learns a piece's stores by comparing the space with its copy and by new address-cell rows; a routed xt! or ptr-cell! (habu2.f ROUTE-HEADER :8270) that writes into a header cell the value an earlier piece stored changes nothing and adds no row (EMIT-MARK keeps one row per cell), so the piece's object holds no store. With header stores out of the run rule the piece links when the earlier storer is removed or edited: the cell is never stored or keeps the wrong value, and warm differs from cold. Found by the review of xqvqnuts (inferred, not reproduced). Acceptance: fixture sequence same (k.f, s1.f and s2.f both storing K's xt into one header cell; build k s1 s2, then k s2): the second build links s2 and its program holds the store; the sequence fails on the stack's hb before the fix. Fix: ROUTE-HEADER marks each routed cell in a band the linker maps below the image (layout.f ROUTED-BYTES); FIND-STORES counts every marked cell a store. Files: src/habu/habu2.f, src/habu/layout.f, src/habu/linker.f, test/file-objects.f, test/fixtures/file-objects/build.f. Verify: file-objects; cold/warm engine cmp. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-hdrf.

Merged (carl, 2026-11-10) into "Drop header stores from the run rule" (xwwymlnw). Measured before merging: the fold's two scans of the header per compiled piece (CLEAR-MARKS, MARKED+ over DATA-START / 8 cells) took a cold engine build from 12.3 s to 13.9 s CPU (two runs each). Fixed in the fold: ROUTE-HEADER also sets the band's last cell (linker.f ANY-MARK), and both scans run only when it is set; cold build 13.1 s wall, file-objects passes with the `same` sequence. Not covered by a test: REACHED-CK against RAN-FILES+ (dot habu-test-a-piece-8111919c).
