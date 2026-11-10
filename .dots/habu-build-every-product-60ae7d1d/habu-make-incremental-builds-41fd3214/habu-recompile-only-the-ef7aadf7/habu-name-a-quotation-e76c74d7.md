---
title: "Name a quotation's owner by its identity"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T15:47:26.815296+02:00"
---

Problem: a quotation reference (src/habu/linker.f CODE-REF+ tag 2) saves its emission's number in its file's piece, which a definition inserted before the owner moves, so files holding one recompile for nothing. Acceptance: tag 2 is file, the owning definition's name sum and count, function; the owner comes from the shadow's rows (EM-OWNER, OWNER-EM, binary searches, no table); FN-REF-CK and FN-ADDR resolve it by ID-REF-CK's and ID-REC's rule; EM-ROW and FILE-EMS removed; the fixture's REF-SKIP follows. Files: src/habu/linker.f, test/fixtures/file-objects/build.f. Verify: ev.sh (warm rewritten 0, cold=warm, file-objects PASS). Depends: habu-read-a-ref-c7e4df78 (merged). Ownership: carl. Brief: carl-layout/.out/briefs/lane-quotation-by-owner.md. Claim: merged into the stack as ynonltsp f015211f (workspace retired), lands with the feature. Report: carl-layout/.out/reports/lane-quotation-by-owner.md.
