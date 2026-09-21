---
title: Reclaim the DATA-CLAIMS tables after the build check
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T12:32:41.320389+03:00"
---

Problem: src/habu/layout.f DATA-CLAIMS allots NAMES ($1000 bytes), MSG-BUF ($100) and TAB (~110 rows x 16 bytes) in image DATA, and the words NAME-AT, ROW-OFF, ROW-LEN, COUNT-ROWS, OVERLAP?, CLAIMS-DIE and CLAIMS-ASSERT stay in the dictionary, so every product image carries roughly 6 KB that is read only while the engine is being built. Acceptance: move the map and its assertion to build-only src/habu/data-claims.f, keep CLAIMS-ASSERT and habu1.f BANDS-DECLARED live during engine construction, and ship neither the map storage nor its words in a product image. The baseline measured 839,516 DATA value bytes and a DATA-CLAIMS package (20 records, 1,608 code bytes); the moved build measured 835,092 DATA value bytes and no DATA-CLAIMS package, while the padded file remained 3,735,744 bytes. An overlapping row and an undeclared PROT-GUARD extent must still fail with their existing diagnostics. Files: src/habu/layout.f, src/habu/data-claims.f, src/habu/habu1.f, test/data-claims-build.f, test/gate-stdlib-cases.f, docs/threads.md. Verify: protection-span, wide-store-seal, task, the data-claims fixture, the affected codegen/bootstrap rows, bootstrap check-only, and three private generations with gen2 == gen3. Depends: none. Ownership: src/habu/layout.f DATA-CLAIMS, src/habu/habu1.f BANDS-DECLARED. Claim: agent=alder workspace=.jj-ws/alder-reclaim-data.
