---
title: Store immutable schema-tagged records
status: open
priority: 2
issue-type: task
created-at: "2026-10-04T05:10:11.361080+03:00"
---

Problem: HBR2 §3.1 stores entity state as fixed-schema immutable field records carrying schema and version, a bounded payload length and references to immutable children, chunks values larger than a page, and requires owning edges to form a DAG; §7.1 needs the same immutable schema-tagged records for ui-props and ui-state. Acceptance: in package RT-RECORD, a linear record builder (DEFLINEAR) that takes field values by declared ordinal and seals into a shared immutable record with explicit retain and release through habu-pool-and-reclaim-a0f574a7; the record builder is a DEFLINEAR minted by one private TRUSTED: pair beside its generation check (D13); a sealed record is never written; a record may reference only records already sealed, so owning edges form a DAG by construction; a payload larger than a page becomes a chain of immutable chunks read back through a bounded cursor; a field read checks the schema version; NaN is refused in a float field (§6). Files: lib/runtime/record.f (new, package RT-RECORD; mints E-RT-RECORD-FIRST/LAST -9530..-9539 in its owning file), lib/errors.f (one comment line), lib/runtime/record-test.f (new), test/gate-stdlib-cases.f. Verify: bin/hb --load lib/runtime/record-test.f: a 1 MiB value round-trips through chunks; duplicating or dropping a builder is a checker refusal; a read under the wrong schema version is refused by code; releasing a record with children reclaims them through RECLAIM-STEP; bin/hb --load test/run.f. Depends: habu-pool-and-reclaim-a0f574a7. Ownership: lib/runtime/record.f. Lane: tim. Claim: unassigned.
