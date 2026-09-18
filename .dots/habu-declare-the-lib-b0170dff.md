---
title: Declare the lib reader and header records
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T10:24:05.103195+03:00"
---

Problem: eight lib records mix a pointer field with scalars and reach the pointer through ptr-field or the scalars through a view: BUF header (lib/byte-buffer.f:26,52-56), VEC header (lib/vector.f:6-10, PTR-FIELD: VEC.DATA), EDIT header and row (lib/byte-edit.f), JR reader (lib/json-read.f:148-170), XML reader (lib/xml/state.f:12-37), MAP slot and header (lib/map.f), TBL pair (lib/table.f:68-87), BUILD step (lib/build.f:30-41); each publishes a size constant its callers create-and-allot against. Acceptance, one commit per record (split into per-record dots at dispatch): the record declared as a private STRUCTURE with DERIVE addr, the callers' create-and-allot replaced by TYPED-VARIABLE or TYPED-BUFFER, every field word generated away, no ptr-field or view cast left for the record, the record's own suite unchanged in what it asserts; before each lands, sweep loom, maki, kiba, radar and Tender (rg only, never load) for the record's public accessor names and announce a break to the owner. Files: the eight lib files, their callers, their suites. Verify: their suites; test/run.f. Depends: habu-generate-a-private-80272413, habu-generate-typed-field-ba63866e, habu-replay-derive-addr-5fd9a813. Ownership: lib. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
