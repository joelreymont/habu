---
title: Reclaim the DATA-CLAIMS tables after the build check
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T12:32:41.320389+03:00"
---

Problem: src/habu/layout.f DATA-CLAIMS allots NAMES ($1000 bytes), MSG-BUF ($100) and TAB (~110 rows x 16 bytes) in image DATA, and the words NAME-AT, ROW-OFF, ROW-LEN, COUNT-ROWS, OVERLAP?, CLAIMS-DIE and CLAIMS-ASSERT stay in the dictionary, so every product image carries roughly 6 KB that is read only while the engine is being built: CLAIMS-ASSERT at the end of layout.f and habu1.f BANDS-DECLARED. Acceptance: both checks still run at build time and die naming the rows on an overlap or an undeclared PROT-GUARD band; the product image no longer carries the blobs or the words (engine size measured before and after, expected about 6 KB less); the whitebox image may keep them. Files: src/habu/layout.f, src/habu/habu1.f, tools/native-build-core.f if the strip is the mechanism. Verify: tools/native-build.f size line; test/protection-span.f; test/wide-store-seal.f; lib/task-test.f; a deliberate overlapping row must still fail the build. Depends: none. Ownership: src/habu/layout.f DATA-CLAIMS, src/habu/habu1.f BANDS-DECLARED. Claim: unassigned.
