---
title: Retire the pre-record by-name regime once the seed is post-record
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T18:19:08.180438+03:00"
---

Problem: the tier-1 front end reaches the checker through the declaration-owner record (habu-build-the-compiler-c348eab0), but the cold-build seed /tmp/cedar-crossing-realpath/hb-stdin predates the record and publishes none, so tools/native-build.f declares itself a pre-record host and the dispatch word binds by name under that declaration; that arm is correct only while a pre-record host exists. Acceptance: the cold-build seed is a post-record engine (a product engine of the landed root, verified by the five-generation chain and the cold build's byte identity with the seeded build); the pre-record declaration row in tools/native-build.f and the by-name arm of the dispatch word are deleted so the record is the sole authority; the inventory test that refuses direct checker references in src/compiler still passes; docs/bootstrap.md names the new seed and how it was minted. Files: tools/native-build.f, src/compiler/native/checker-owner.f, docs/bootstrap.md. Verify: the cold build from the new seed, tools/two-generation-build.f, test/run.f. Depends: habu-build-the-compiler-c348eab0. Ownership: hazel. Claim: unassigned.
