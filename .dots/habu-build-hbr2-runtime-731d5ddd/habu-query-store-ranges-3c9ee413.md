---
title: Query store ranges as snapshot jobs
status: open
priority: 2
issue-type: task
created-at: "2026-10-04T05:10:11.481921+03:00"
---

Problem: HBR2 §6 renders large collections from query jobs that scan or paginate an index under a snapshot and return a stable ordered logical collection, so no view callback reads a million rows; §4.1 lists the semantic page query among the required job kinds. Acceptance: in package RT-QUERY, a query job over a key range of a store tree in habu-publish-rootsets-and-e43b4749, holding one lease, paging in bounded steps through the habu-build-the-persistent-e2154c78 cursor and resuming on the same snapshot; an optional declared (field ordinal, value) equality filter; the result is an ordered collection of Id128 entity identities with their revisions; the query revision is the store slot's revision at the snapshot, and a later commit to that store reruns live queries as jobs; cancellation releases the lease and partial pages; no secondary index. Files: lib/runtime/query.f (new, package RT-QUERY; mints E-RT-QUERY-FIRST/LAST -9760..-9769 in its owning file), lib/errors.f (one comment line), test/browser/query-test.f (new), test/gate-stdlib-cases.f. Verify: bin/hb --load test/browser/query-test.f: a query over 1,000,000 entities completes over many bounded steps while 100 commits publish, and its result equals the snapshot it started on; a cancelled query frees its lease; bin/hb --load test/run.f. Depends: habu-publish-rootsets-and-e43b4749, habu-run-explicit-jobs-37f296ae. Ownership: lib/runtime/query.f. Lane: tim. Claim: unassigned.
